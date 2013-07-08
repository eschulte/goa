#!/usr/bin/python2.7

import filecmp
from glob import glob
from optparse import OptionParser
import os
import shutil
from subprocess import call, check_call
import sys
import tempfile

parser = OptionParser( usage = "%prog [options] benchmark executable" )
parser.add_option(
    "-v", "--verbose", action = "store_true",
    help = "show diagnostic information for failed tests"
)
options, args = parser.parse_args()

if len( args ) != 2:
    parser.print_usage()
    exit( 1 )

benchmark  = args[ 0 ]
executable = args[ 1 ]

root = os.path.dirname( os.path.dirname( os.path.abspath( sys.argv[ 0 ] ) ) )

if options.verbose:
    stdout = sys.stdout
    stderr = sys.stderr
else:
    devnull = open( "/dev/null" )
    stdout = devnull
    stderr = devnull

def mktemp( suffix = "" ):
    tmp = tempfile.NamedTemporaryFile( delete = False, suffix = suffix )
    tmp.close()
    return tmp.name

def find_golden( benchmark ):
    bmark_dir = os.path.join(
        root, "benchmarks", "parsec-3.0", "pkgs", "apps", benchmark, "inst"
    )
    matches = list()
    for d, dnames, fnames in os.walk( bmark_dir ):
        for fname in fnames:
            if fname == benchmark:
                matches.append( os.path.join( d, fname ) )
    if len( matches ) != 1:
        raise Exception( "couldn't find golden binary for " + benchmark )
    return matches[ 0 ]

class Benchmark:
    def __init__( self, bmark, inputs ):
        self.bmark   = bmark
        self.inputs  = inputs
        self.outputs = list()

        for i in range( len( inputs ) ):
            golden = self.__get_golden_name( i )
            if os.path.exists( golden ):
                self.outputs.append( golden )
            else:
                self.outputs.append( None )

    def __get_golden_name( self, i ):
        return os.path.join(
            root, "benchmarks", self.bmark, self.bmark + ".golden.%d" % i
        )

    def getNumInputs( self ):
        return len( self.inputs )

    def check( self, i ):
        if self.outputs[ i ] is None:
            golden = find_golden( self.bmark )
            obj = self.__class__( golden )
            tmp = obj.run( i )
            master = self.__get_golden_name( i )
            shutil.move( tmp, master )
            self.outputs[ i ] = master
        return self._check_impl( i )

class Blackscholes( Benchmark ):
    def __init__( self, executable ):
        self.executable = executable

        input_dir = os.path.join(
            root, "etc", "additional-inputs", "blackscholes"
        )
        inputs = list()
        for fname in glob( os.path.join( input_dir, "blackscholes.input.*" ) ):
            num = int( os.path.splitext( fname )[ 1 ][ 1: ] )
            while len( inputs ) <= num:
                inputs.append( None )
            inputs[ num ] = fname

        if len( inputs ) != 6:
            raise Exception( "couldn't find all inputs for blackscholes!" )
        for fname in inputs:
            if fname is None:
                raise Exception( "couldn't find all inputs for blackscholes!" )

        Benchmark.__init__( self, "blackscholes", inputs )

    def run( self, i ):
        if options.verbose:
            print "Running %s (%d)" % ( self.executable, i )
        out = mktemp()
        try:
            check_call(
                [ self.executable, "1", self.inputs[ i ], out ],
                stdout = stdout, stderr = stderr
            )
        except Exception as e:
            os.remove( out )
            raise e
        return out

    def _check_impl( self, i ):
        tmpfile = self.run( i )
        cmd = [ "diff", self.outputs[ i ], tmpfile ]
        result = call( cmd, stdout = stdout, stderr = stderr )
        os.remove( tmpfile )
        return result == 0

ctors = {
    "blackscholes": Blackscholes
}
if not benchmark in ctors:
    print "tests for %s not implemented yet"
    exit( 1 )
bmark = ctors[ benchmark ]( executable )
for i in range( bmark.getNumInputs() ):
    status = "pass" if bmark.check( i ) else "FAIL"
    print "%d: %s" % ( i, status )
if bmark.getNumInputs() == 0:
    print "no inputs defined for", benchmark

