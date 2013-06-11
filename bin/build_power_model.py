#!/usr/bin/python

import csv
import math
import numpy
from numpy.distutils import cpuinfo
from optparse import OptionParser
import sys

parser = OptionParser( usage = "%prog [options] filename" )
parser.add_option(
    "--avg", metavar = "fname", help = "write average values to named file"
)
parser.add_option(
    "--conf", metavar = "fname", help = "write confidence interval to named file"
)
parser.add_option(
    "--fit", action = "store_true", help = "fit a linear model to the data"
)
parser.add_option(
    "--brute", action = "store_true",
    help = "perform a brute-force search for feature subset"
)
parser.add_option(
    "--leave-one-out", action = "store_true",
    help = "hold out each benchmark for testing"
)
parser.add_option(
    "--limit", metavar = "N", type = int,
    help = "number of features to use in the model"
)
parser.add_option(
    "--model", metavar = "features",
    help = "comma-separated list of features to include in the model; " \
         + "optional weights may be fixed using 'feature=weight'"
)
parser.add_option(
    "--no-table", action = "store_true", help = "do not print table"
)
parser.add_option(
    "--target", default = "energy", metavar = "feature",
    help = "feature to model as a linear combination of features"
)
options, args = parser.parse_args()

if len( args ) < 1:
    parser.print_usage()
    exit()

if not options.avg and not options.conf:
    options.fit = True

def avg( data ):
    return numpy.mean( numpy.array( data ) )

def confidence( data ):
    sd = numpy.std( numpy.array( data ), ddof = 1 )
    return 1.96 * sd / math.sqrt( len( data ) )

def enumsubsets( size, elems ):
    if size < 1 or len( elems ) < size:
        return []
    if size == 1:
        return [ [ x ] for x in elems ]
    if len( elems ) == size:
        return [ list( elems ) ]

    subsets = []
    for i in range( len( elems ) ):
        tmp = enumsubsets( size - 1, elems[ ( i + 1 ): ] )
        subsets.extend( map( lambda x: [ elems[ i ] ] + x, tmp ) )
    return subsets        

def format_table( table, formats = None ):
    if not formats:
        formats = [ str ] * max( map( len, table ) )

    stringified = list()
    for row in table:
        tmp = list()
        for i, cell in enumerate( row ):
            if isinstance( cell, str ):
                tmp.append( cell )
            else:
                tmp.append( formats[ i ]( cell ) )
        stringified.append( tmp )

    widths = list()
    for row in stringified:
        for i, cell in enumerate( row ):
            while len( widths ) <= i:
                widths.append( 0 )
            widths[ i ] = max( widths[ i ], len( cell ) )

    result = list()
    for i, row in enumerate( table ):
        tmp = list()
        for j, cell in enumerate( row ):
            if isinstance( cell, int ) or isinstance( cell, float ):
                tmp.append( stringified[ i ][ j ].rjust( widths[ j ], " " ) )
            else:
                tmp.append( stringified[ i ][ j ].ljust( widths[ j ], " " ) )
        result.append( tmp )

    return result

fst = lambda t: t[ 0 ]

class Model:
    def __init__( self, data, y, xs ):
        self.data  = dict( data )
        self.y     = y
        self.xs    = list()
        self.fixed = dict()
        self.coeff = None

        for x in xs:
            pair = x.split( "=", 2 )
            if len( pair ) == 1:
                pair.append( None )
            else:
                pair[ 1 ] = float( pair[ 1 ] )
            self.xs.append( pair[ 0 ] )
            self.fixed[ pair[ 0 ] ] = pair[ 1 ]
        if not 'intercept' in self.fixed:
            self.xs.append( 'intercept' )
            self.fixed[ 'intercept' ] = None
        for bmark in self.data:
            if not 'intercept' in self.data[ bmark ]:
                self.data[ bmark ][ 'intercept' ] = 1

    def __str__( self ):
        if self.coeff is None:
            return str( None )

        table = list()
        for h, c in zip( self.xs, self.coeff ):
            table.append( ( h + ":", c ) )
        table = format_table( table, [ str, "%g".__mod__ ] )
        return "Modeling " + self.y + "\n" + "\n".join( map( "   ".join, table ) )

    def train( self, bmarks ):
        free  = list()
        for x in self.xs:
            if self.fixed[ x ] is None:
                free.append( x )
        cs = dict()

        if len( free ) > 0:
            bmarks = filter( lambda b: self.data[ b ][ self.y ] != 0, bmarks )
            A = numpy.empty( ( len( bmarks ), len( free ) ) )
            B = numpy.empty( ( len( bmarks ), ) )
            for i, bmark in enumerate( bmarks ):
                B[ i ] = self.data[ bmark ][ self.y ]
                for j, x in enumerate( free ):
                    A[ i, j ] = self.data[ bmark ][ x ]

            cs = dict( zip( free, numpy.linalg.lstsq( A, B )[ 0 ] ) )

        for x, c in self.fixed.items():
            if not c is None:
                cs[ x ] = c
        self.coeff = list()
        for x in self.xs:
            self.coeff.append( cs[ x ] )

    def test( self, bmarks ):
        table = [ ( "benchmark", "measured", "predicted", "error" ) ]
        for i, bmark in enumerate( bmarks ):
            v = 0
            for j, x in enumerate( self.xs ):
                v += self.coeff[ j ] * self.data[ bmark ][ x ]
            target = self.data[ bmark ][ self.y ]
            if self.y == "watts":
                v *= self.data[ bmark ][ "time" ]
                target *= self.data[ bmark ][ "time" ]
            try:
                err = abs( v - target ) / target * 100
                if not math.isinf( err ):
                    table.append( ( bmark + ":", target, v, err ) )
                else:
                    print "WARN: skipping bmark:", bmark
            except:
                pass
        return table

def evalModel( values, target, varnames ):
    model = Model( values, target, varnames )
    if options.leave_one_out:
        master_table = list()
        for i, bmark in enumerate( bmarks ):
            model.train( bmarks[ :i ] + bmarks[ ( i + 1 ): ] )
            table = model.test( [ bmark ] )
            if len( master_table ) == 0:
                master_table.extend( table )
            else:
                master_table.extend( table[ 1: ] )
        table = master_table
    else:
        model.train( bmarks )
        table = model.test( bmarks )

    errs = list()
    for row in table[ 1: ]:
        errs.append( row[ -1 ] )
    return avg( errs ), model, table

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

if cpuinfo.cpuinfo().is_AMD():
    flop_regs = [ "r533f00" ]
else:
    flop_regs = [ "r532010", "r538010" ]

data = dict()
with open( args[ 0 ] ) as fh:
    reader = csv.reader( fh )
    headers = reader.next()
    time_index = headers.index( "time" ) - 1
    for line in reader:
        bmark, line = line[ 0 ], line[ 1: ]
        if float( line[ time_index ] ) == 0:
            continue
        if not bmark in data:
            data[ bmark ] = list()
            for v in line:
                data[ bmark ].append( list() )
        for i in range( len( line ) ):
            data[ bmark ][ i ].append( float( line[ i ] ) )

bmarks = list( data.keys() )
bmarks.sort()

summaries = list()
if options.avg:
    summaries.append( ( avg, options.avg ) )
if options.conf:
    summaries.append( ( confidence, options.conf ) )
for summary, fname in summaries:
    with open( fname, 'w' ) as fh:
        writer = csv.writer( fh )
        writer.writerow( headers )
        for bmark in bmarks:
            row = [ bmark ]
            for col in data[ bmark ]:
                row.append( summary( col ) )
            writer.writerow( map( str, row ) )

if options.fit:
    model = dict()
    for bmark in bmarks:
        values = dict()
        for header, col in zip( headers[ 1: ], data[ bmark ] ):
            values[ header ] = avg( col )

        flops = 0
        for reg in flop_regs:
            flops += values[ reg ]
            del values[ reg ]
        values[ "flop" ] = flops
        values[ "ipc" ]   = values[ "instructions" ] / values[ "cycles" ]
        values[ "flop/cyc" ] = values[ "flop" ] / values[ "cycles" ]
        values[ "cache ref/cyc" ] = values[ "cache-references" ] / values[ "cycles" ]
        values[ "mem/cyc" ]   = values[ "cache-misses" ] / values[ "cycles" ]

        if options.target == "energy":
            del values[ "kwh" ]
        if options.target != "kwh":
            values[ "energy" ] = values[ "watts" ] * values[ "time" ]
        model[ bmark ] = values

    if options.model:
        varnames = options.model.split( "," )
    else:
        varnames = list( iter( model.values() ).next().keys() )
        varnames.sort()
        varnames.remove( options.target )
    if options.limit is None:
        options.limit = len( varnames )
    options.limit = min( options.limit, len( bmarks ) - 2 )

    modelsets = [ [] ]
    if options.brute:
        for i in range( options.limit ):
            modelsets.extend( enumsubsets( i + 1, varnames ) )
    else:
        if options.limit > 0:
            modelsets = enumsubsets( options.limit, varnames )

    best = None, None, None
    for modelset in modelsets:
        current = evalModel( model, options.target, modelset )
        if best[ 0 ] is None or current[ 0 ] < best[ 0 ]:
            best = current
    err, model, table = best

    if not options.leave_one_out:
        print model
        print ""

    if not options.no_table:
        table = format_table(
            table, [ str, "%g".__mod__, "%g".__mod__, "%3.1f%%".__mod__ ]
        )
        print "\n".join( map( "    ".join, table ) )
    print "\nAverage error: %3.1f%%" % err

