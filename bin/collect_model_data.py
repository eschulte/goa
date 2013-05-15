#!/usr/bin/python

import csv
import glob
from numpy.distutils import cpuinfo
from optparse import OptionParser
import os
import re
from subprocess import Popen, call, check_call
import subprocess
import sys
import time

parser = OptionParser( usage = "%prog [otions] number" )
parser.add_option( "--spec-config", help = "SPEC config file" )
parser.add_option( "--parsec", action = "store_true", help = "run parsec" )
parser.add_option( "--test", action = "store_true", help = "run test inputs" )
options, args = parser.parse_args()

if len( args ) < 1:
    parser.print_help()
    exit()
try:
    n = int( args[ 0 ] )
except:
    print "ERROR: number must be a number"
    exit( 1 )

root = os.path.abspath( os.path.dirname( sys.argv[ 0 ] ) )

def sleep( bmark, dotest ):
    duration = bmark[ 5: ]
    return lambda submit: [
        '/bin/sh', '-xc', submit + " sleep " + duration
    ]

def spec( bmark, dotest ):
    data = "test" if dotest else "ref"
    return lambda submit: [
        'runspec',
            '-a', 'run',
            '-c', options.spec_config,
            '--iterations', '1',
            '--noreportable',
            '-i', data,
            '--define', "SUBMIT_CMD=" + submit,
            bmark
    ]

def parsec( bmark, dotest ):
    data = "test" if dotest else "native"
    return lambda submit: [
        'parsecmgmt',
            '-a', 'run',
            '-p', "parsec." + bmark,
            '-i', data,
            '-s', submit
    ]

benchmarks = [
    ( "sleep2", sleep ),
]
if not options.test:
    benchmarks += [
        ( "sleep5", sleep ),
        ( "sleep10", sleep ),
        ( "sleep30", sleep ),
        ( "sleep60", sleep ),
    ]
if options.parsec:
    benchmarks += [
        ( "blackscholes", parsec ),
        ( "bodytrack", parsec ),
        ( "facesim", parsec ),
        # ( "canneal", parsec ),
        ( "ferret", parsec ),
        ( "fluidanimate", parsec ),
        ( "freqmine", parsec ),
        # ( "netstreamcluster", parsec ),
        # ( "streamcluster", parsec ),
        ( "raytrace", parsec ),
        ( "swaptions", parsec ),
        ( "vips", parsec ),
        ( "x264", parsec ),
    ]
if options.spec_config:
    benchmarks += [
        ( "400.perlbench", spec ),
        ( "401.bzip2", spec ),
        ( "403.gcc", spec ),
        ( "410.bwaves", spec ),
        ( "416.gamess", spec ),
        ( "429.mcf", spec ),
        ( "433.milc", spec ),
        ( "434.zeusmp", spec ),
        ( "435.gromacs", spec ),
        ( "436.cactusADM", spec ),
        ( "437.leslie3d", spec ),
        ( "444.namd", spec ),
        ( "445.gobmk", spec ),
#        ( "447.dealII", spec ),
        ( "450.soplex", spec ),
        ( "453.povray", spec ),
        ( "454.calculix", spec ),
        ( "456.hmmer", spec ),
        ( "458.sjeng", spec ),
        ( "459.GemsFDTD", spec ),
        ( "462.libquantum", spec ),
        ( "464.h264ref", spec ),
        ( "465.tonto", spec ),
        ( "470.lbm", spec ),
        ( "471.omnetpp", spec ),
        ( "473.astar", spec ),
        ( "481.wrf", spec ),
        ( "482.sphinx3", spec ),
        ( "483.xalancbmk", spec ),
        ( "998.specrand", spec ),
        ( "999.specrand", spec ),
    ]

def genDatfile( bmark, *keys ):
    keys = map( str, keys )
    oldfiles = os.path.join(
        os.getcwd(), ".".join( [ bmark ] + keys + [ "*", "dat" ] )
    )
    for fname in glob.glob( oldfiles ):
        print "deleting", fname
    return oldfiles.replace( "*", "$workload" )

def findDatfiles( bmark, pattern ):
    workload_pat = re.escape( pattern ).replace( "\$workload", "(\d+)" )
    workload_pat = re.compile( workload_pat )
    datfiles = list()
    for fname in glob.glob( pattern.replace( "$workload", "*" ) ):
        m = workload_pat.match( fname )
        if m:
            datfiles.append( ( bmark + ".w" + m.group( 1 ), fname ) )
        else:
            datfiles.append( ( bmark, fname ) )
    return datfiles

allkeys = list()
def readData( fname, *keys ):
    result = dict()
    with open( fname ) as fh:
        reader = csv.DictReader( fh )
        for line in reader:
            pass
        for key in keys:
            if key in line:
                if not key in allkeys:
                    allkeys.append( key )
                result[ key ] = line[ key ]
    os.remove( fname )
    return result

def runWU( bmark, i, run, *counters ):
    datfile = genDatfile( bmark, "power", i )

    time.sleep( 5 )
    check_call( run( wattsup + " -o " + datfile + " ttyUSB0 --" ) )

    result = dict()

    for bmark, fname in findDatfiles( bmark, datfile ):
        result[ bmark ] = readData( fname, *counters )
    return result
    
def runPerf( bmark, i, run, *counters ):
    datfile = genDatfile( bmark, "perf", i )
    cmd = "perf stat -o " + datfile + " -e " + ",".join( counters ) + " --"

    check_call( run( cmd ) )

    result = dict()
    for bmark, fname in findDatfiles( bmark, datfile ):
        result[ bmark ] = dict()
        with open( fname ) as fh:
            for line in fh:
                line = line.strip().split()
                if len( line ) >= 2 and \
                        line[ 0 ][ 0 ].isdigit() and \
                        line[ 1 ] in counters:
                    result[ bmark ][ line[ 1 ] ] = line[ 0 ].replace( ",", "" )
                    if not line[ 1 ] in allkeys:
                        allkeys.append( line[ 1 ] )
        os.remove( fname )
    return result

def merge( d1, d2 ):
    for key in d2:
        if key in d1 and type( d1[ key ] ) is dict:
            merge( d1[ key ], d2[ key ] )
        else:
            d1[ key ] = d2[ key ]

if cpuinfo.cpuinfo().is_AMD():
    flop_regs = [ "r533f00" ]
else:
    flop_regs = [ "r532010", "r538010" ]

wattsup = os.path.join( root, "wu.py" )
with open( "benchmarks.csv", 'w' ) as fh:
    writer = None
    for i in range( n ):
        for bmark, runcmd in benchmarks:
            run = runcmd( bmark, options.test )

            data = dict()
            merge( data, runWU( bmark, i, run, "time", "kwh", "watts" ) )
            merge( data, runPerf( bmark, i, run, "cycles", "instructions", *flop_regs ) )
            merge( data, runPerf( bmark, i, run, "cache-references", "cache-misses" ) )
            if writer is None:
                writer = csv.writer( fh )
                writer.writerow( [ "benchmark" ] + allkeys )

            for bmark in data:
                row = [ bmark ]
                for key in allkeys:
                    row.append( data[ bmark ][ key ] )
                writer.writerow( row )
            fh.flush()

