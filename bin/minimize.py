#!/usr/bin/python2

# Based on the delta-debugging code from http://www.st.cs.uni-saarland.de/dd/

import DD

from difflib import SequenceMatcher
from glob import glob
import math
import re
import numpy
from optparse import OptionParser
import os
import scipy.stats
from subprocess import call, check_call, Popen, PIPE, CalledProcessError
import sys
import tempfile
import threading

# Casting minimization of energy as a delta-debugging problem. The original
# assembly code is taken as "yesterday's code" and the output from the GA is
# taken as "today's code." A subset of deltas "passes" if the modeled energy
# usage is substantially different from the energy used by the GA output, and
# fails if it is substantially the same. Thus, the minimal failing deltas
# constitute the minimal set of deltas that have substantially the same energy
# as the optimized variant.
#
# As an optimization, variants are preemptively terminated if they take longer
# than twice as much time as the optimized variant does on average. This
# heuristic is necessary in some cases to avoid indefinitely waiting for a
# variant to terminate when we have a good idea that it isn't what we are
# looking for.

parser = OptionParser( usage = "%prog [options] benchmark resultsdir" )
parser.add_option(
    "--alpha", type = float, default = 0.01,
    help = "alpha value for statistical tests"
)
parser.add_option(
    "--line-by-line", action = "store_true",
    help = "split multi-line deltas into several individual-line deltas"
)
parser.add_option( "-s", "--size", help = "the input size to use" )
parser.add_option( "--store", help = "the stored checkpoint to use" )
parser.add_option(
    "-r", "--reps", metavar = "N", type = int, default = 5,
    help = "repeat energy measurement N times"
)
options, args = parser.parse_args()

root = os.path.dirname( os.path.dirname( os.path.abspath( sys.argv[ 0 ] ) ) )

if len( args ) < 1:
    parser.print_help()
    exit()

bmark  = args[ 0 ]

if options.size is None:
    p = Popen( [
        "bash", "-c",
            ". %s; BENCHMARK=%s; size_for_benchmark" \
                % ( os.path.join( root, "bin", "common" ), bmark )
    ], stdout = PIPE )
    options.size = p.communicate()[ 0 ].strip()

if options.store is not None:
    if not os.path.exists( options.store ):
        print "ERROR:", options.store, "is not a valid path"
        exit( 1 )
elif len( args ) < 2:
    print "please either use --store or provide the results directory"
    parser.print_help()
    exit()
else:
    resdir = args[ 1 ]

bmarkdir = os.path.join( root, "benchmarks", bmark )
if not os.path.exists( os.path.join( bmarkdir, bmark + ".s" ) ):
    print "Could not find original assembly to work from"
    exit( 1 )

# Utilities to write messages without buffering getting things out of order

def write_msg( stream, prefix, msg ):
    print >>stream, prefix, msg
    try:
        stream.flush()
        os.fsync( stream.fileno() )
    except: pass

def info( msg ):
    write_msg( sys.stdout, "INFO:", msg )

def warn( msg ):
    write_msg( sys.stderr, "WARNING:", msg )

def error( msg ):
    write_msg( sys.stderr, "ERROR:", msg )

# Utilities to limit the runtime of child processes

timeout = 0
timedout = False
def timesup( p ):
    global timedout
    try:
        p.kill()
        timedout = True
    except Exception as e:
        warn( e )

def run( *args, **kargs ):
    global timedout
    timedout = False

    if timeout == 0:
        return call( *args, **kargs )

    p = Popen( *args, **kargs )
    t = threading.Timer( timeout, timesup, [ p ] )
    t.start()
    try:
        status = p.wait()
    except KeyboardInterrupt as e:
        try:
            p.kill()
        except: pass
        raise e
    finally:
        t.cancel()
    return status

########

def writeGenome( deltas, fname ):
    global counter
    deltas.sort( key = lambda x: ( x[ 1 ], x[ 3 ] ), reverse = True )
    genome = list( original )
    for tag, i1, i2, j1, j2 in deltas:
        if tag == "delete":
            del genome[ i1:i2 ]
        elif tag == "replace" or tag == "insert":
            genome[ i1:i2 ] = optimized[ j1:j2 ]
        elif tag == "nop":
            pass
        else:
            raise Exception( "Unrecognized delta tag!: " + str( tag ) )

    with open( fname, 'w' ) as fh:
        for gene in genome:
            print >>fh, gene.rstrip()

def runBmark( binary ):
    cmd = [
        os.path.join( root, "bin", "run" ),
            bmark, str( binary ), "-s", options.size, "-p"
    ]
    results = list()

    tmp = tempfile.NamedTemporaryFile( delete = False )
    tmp.close()
    try:
        for i in range( options.reps ):
            with open( tmp.name, 'w' ) as fh:
                status = run( cmd, stdout = fh, stderr = fh )
                if status != 0:
                    raise CalledProcessError(
                        returncode = status,
                        cmd = cmd,
                        output = None
                    )
            with open( tmp.name ) as fh:
                results.append( fh.readlines() )
    finally:
        os.remove( tmp.name )
    return results

def collectSeconds( binary ):
    results = runBmark( binary )

    values = list()
    for result in results:
        for line in result:
            terms = line.strip().split( "," )
            if len( terms ) != 2:
                continue
            val, key = terms
            if key == "seconds":
                values.append( val )
            elif ( key == "exit" or key == "error" ) and val != "0":
                raise Exception( "invalid %s: %s" % ( key, val ) )
        else:
            values.append( None )
    return values

def collectEnergy( binary ):
    results = runBmark( binary )
    for result in results:
        for line in result:
            terms = line.strip().split( "," )
            if len( terms ) != 2:
                continue
            val, key = terms
            if ( key == "exit" or key == "error" ) and val != "0":
                raise Exception( "invalid %s: %s" % ( key, val ) )

    values = list()
    cmd = [ os.path.join( root, "bin", "calc-energy" ) ]
    for result in results:
        p = Popen( cmd, stdin = PIPE, stdout = PIPE )
        energy = p.communicate( "".join( result ) )[ 0 ]
        for line in energy.splitlines():
            values.append( line.strip() )
            break
        else:
            values.append( None )
    return values

# Tests whether x and y are likely to be sampled from normal distributions with
# different means.
#
# The side parameter indicates whether this is a one- or two-tailed tests
#   -1 -> test if x < y
#    0 -> test if x != y
#    1 -> test if x > y
#
def ttest( x, y, side = 0 ):
    avgx = numpy.mean( x )
    avgy = numpy.mean( y )
    varx = numpy.var( x )
    vary = numpy.var( y )
    nx = len( x )
    ny = len( y )
    t = ( avgx - avgy ) / math.sqrt( varx / nx + vary / ny )
    if side == 0:
        t = abs( t )
    elif side < 0:
        t = -t
    df = ( ( varx / nx + vary / ny ) ** 2 ) \
       / ( varx / nx * varx / nx / ( nx - 1 ) + vary / ny * vary / ny / ( ny - 1 ) )
    p = 1 - scipy.stats.t.cdf( t, df )
    if side == 0:
        p *= 2
    return p, avgx, avgy

class AltBinary:
    def __init__( self, deltas ):
        self.deltas = deltas
        self.tmpname = None

    def __enter__( self ):
        tmp = tempfile.NamedTemporaryFile(
            dir = bmarkdir,
            suffix = ".s",
            delete = False
        )
        tmp.close()
        self.tmpname = tmp.name
        os.rename( os.path.join( bmarkdir, bmark + ".s" ), self.tmpname )

        if os.path.exists( os.path.join( bmarkdir, bmark ) ):
            os.remove( os.path.join( bmarkdir, bmark ) )
        writeGenome( self.deltas, os.path.join( bmarkdir, bmark + ".s" ) )
        if bmark == "vips":
            status = call( [
                os.path.join( root, "bin", "link-vips" ),
                    "-o", os.path.join( bmarkdir, bmark ),
                    os.path.join( bmarkdir, bmark + ".s" ),
            ] )
        else:
            status = call( [
                os.path.join( root, "bin", "mgmt" ), "link", bmark
            ] )
        if status != 0:
            self.__exit__( None, None, None )
            raise Exception( "mgmt failure" )
        return self

    def __exit__( self, typ, val, trace ):
        if self.tmpname is not None and os.path.exists( self.tmpname ):
            os.rename( self.tmpname, os.path.join( bmarkdir, bmark + ".s" ) )

    def __str__( self ):
        return os.path.join( bmarkdir, bmark )

class MyDD(DD.DD):
    def __init__(self):
        DD.DD.__init__(self)
        self.counter = 0

    def _test(self, deltas):
        global timedout

        # "Passing" behavior is more like the original (slower, more energy)
        # "Failing" behavior is more optmized (faster, less energy)

        try:
            timedout = False
            with AltBinary( deltas ) as binary:
                myenergy = collectEnergy( binary )
        except Exception as e:
            if timedout:
                info( "timed out" )
                return self.PASS
            else:
                info( "error (%s)" % str( e.message ) )
                return self.UNRESOLVED
        if timedout:
            info( "timed out" )
            return self.PASS

        myenergy = map( float, filter( lambda x: x is not None, myenergy ) )

        self.counter += 1
        p = scipy.stats.ranksums( myenergy, energy )[ 1 ] / 2
        myavg = numpy.mean( myenergy )
        avg   = numpy.mean( energy )
        info( "p = %g (%g vs %g)" % ( p, myavg, avg ) )
        if p < options.alpha and myavg > avg:
            return self.PASS
        else:
            return self.FAIL

info( "reading assembly sources" )

original = list()
with open( os.path.join( bmarkdir, bmark + ".s" ) ) as fh:
    original = fh.readlines()

if options.store is not None:
    store = options.store
elif os.path.exists( os.path.join( resdir, "final-best.store" ) ):
    store = os.path.join( resdir, "final-best.store" )
else:
    matcher = re.compile( "best-(\d+).store" )
    store = ( 0, "" )
    for fname in glob( resdir + "/best-*.store" ):
        m = matcher.match( os.path.basename( fname ) )
        if not m:
            info( "no match for", os.path.basename( fname ) )
            continue
        num = int( m.group( 1 ) )
        if num > store[ 0 ]:
            store = ( num, fname )
    if store[ 0 ] == 0:
        error( "Could not find checkpoint to minimize" )
        exit( 1 )
    store = store[ 1 ]

# dump the best variant assembly to a temporary file

tmp = tempfile.NamedTemporaryFile( delete = False )
tmp.close()
try:
    with open( tmp.name, 'w' ) as fh:
        check_call(
            [ os.path.join( root, "bin", "objread" ), store, "-G" ],
            stdout = fh
        )

    optimized = list()
    with open( tmp.name ) as fh:
        optimized = fh.readlines()
finally:
    os.remove( tmp.name )

info( "computing deltas" )

deltas = list()
matcher = SequenceMatcher( None, original, optimized, False )
if options.line_by_line:
    for tag, i1, i2, j1, j2 in matcher.get_opcodes():
        if tag == "delete":
            for i in range( i1, i2 ):
                deltas.append( ( tag, i, i+1, j1, j2 ) )
        elif tag == "insert":
            for j in range( j1, j2 ):
                deltas.append( ( tag, i1, i2, j, j+1 ) )
        elif tag == "replace":
            for i in range( i1, i2 ):
                deltas.append( ( "delete", i, i+1, j2, j2 ) )
            for j in range( j1, j2 ):
                deltas.append( ( "insert", i1, i1, j, j+1 ) )
else:
    for opcode in matcher.get_opcodes():
        if opcode[ 0 ] != "equal":
            deltas.append( opcode )

info( "found %d deltas" % len( deltas ) )

with AltBinary( deltas ) as binary:
    info( "computing timeout threshold" )
    seconds = collectSeconds( binary )
    seconds = map( float, filter( lambda x: x is not None, seconds ) )
    timeout = numpy.mean( seconds ) * 2
    info( timeout / 2 )

    info( "computing baseline energy" )
    energy = collectEnergy( binary )
    energy = map( float, filter( lambda x: x is not None, energy ) )
    info( numpy.mean( energy ) )

mydd = MyDD()

info( "Confirming that original and final are substantially different" )
res = mydd._test( [] )
if res == mydd.FAIL:
    info( "no difference detected" )
    writeGenome( [], bmark + ".minimized.s" )
    exit()
else:
    info( "Simplifying failure-inducing input..." )
    c = mydd.ddmin(deltas)              # Invoke DDMIN
    writeGenome( c, bmark + ".minimized.s" )
    print "The 1-minimal failure-inducing input is", c
    print "Removing any element will make the failure go away."
    print
    
