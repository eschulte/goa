#!/usr/bin/python

from optparse import OptionParser
import os
from select import select
import serial
import signal
from subprocess import call, Popen
import sys
import time

parser = OptionParser( usage = """%prog [options] device -- command [args...]

Collects usage data from an attached WattsUp?PRO meter."""
)
parser.add_option(
    "-b", "--bracket", metavar = "sec", type = int, default = 0,
    help = "number of seconds to wait before and after command"
)
parser.add_option(
    "-c", "--count", metavar = "N", type = int,
    help = "number of samples to take (implies external log style)"
)
parser.add_option(
    "-e", metavar = "counter", action = "append", default = [],
    help = "metric to record"
)
parser.add_option(
    "-i", action = "store_true", help = "record incremental counter values"
)
parser.add_option(
    "--list-counters", action = "store_true",
    help = "list counters instead of recording trace data"
)
parser.add_option(
    "--log-style", metavar = "style", default = "internal",
    help = "whether to store data internally (on WattsUp?) or externally"
)
parser.add_option( "-o", metavar = "file", help = "CSV file to generate" )
options, args = parser.parse_args()

class InvalidMessage( StandardError ):
    def __init__( self, *args ):
        StandardError.__init__( self, *args )

class WattsUp:
    def __init__( self, serialfile ):
        try:
            self.fh = serial.Serial( serialfile, 115200, timeout = 2 )
        except serial.serialutil.SerialException as e:
            self.closed = True
            raise e
        self.buf = ""
        self.closed = False

    def __del__( self ):
        if not self.closed:
            self.fh.close()
            self.closed = True

    def __enter__( self ):
        return self

    def __exit__( self, typ, val, trace ):
        self.__del__()

    def readReply( self, msg = None ):
        if msg:
            self.fh.write( msg )
            self.fh.flush()

        while True:
            pivot = self.buf.find( ";" )
            if pivot >= 0:
                break
            select( [ self.fh ], [], [] )
            size = self.fh.inWaiting()
            self.buf += self.fh.read( size )
        reply    = self.buf[ :pivot ]
        self.buf = self.buf[ ( pivot + 1 ): ]

        pivot = reply.find( "#" )
        if pivot == -1:
            raise InvalidMessage( reply )
        reply = reply[ ( pivot + 1 ): ].split( "," )
        c1, c2, nargs = reply[ :3 ]
        if len( reply ) != int( nargs ) + 3:
            raise InvalidMessage( reply )
        return ( c1, c2, reply[ 3: ] )

def I( x ):
    return x

def scale( factor ):
    return lambda x: x / factor

class avg:
    def __init__( self ):
        self.tot = 0
        self.num = 0

    def __call__( self, x ):
        self.tot += x
        self.num += 1
        return float( self.tot ) / float( self.num )

class last:
    def __init__( self ):
        self.last = None

    def __call__( self, x ):
        self.last = x
        return x

def processData( delta, conds, reply ):
    reply = [ float( v ) for c, v in zip( conds, reply ) if c ]
    reply.insert( 0, delta )
    data   = [ f( v ) for f, v in zip( inst, reply ) ]
    totals = [ f( v ) for f, v in zip( summ, data ) ]
    return data, totals

counters = {
    "watts":        ( "W",  ( scale( 10.0 ), avg() ) ),
    "volts":        ( "V",  ( scale( 10.0 ), avg() ) ),
    "amps":         ( "A",  ( scale( 1000.0 ), avg() ) ),
    "kwh":          ( "WH", ( scale( 10000.0 ), last() ) ),
    "power-factor": ( "PF", ( I, avg() ) ),
    "duty-cycle":   ( "DC", ( I, avg() ) ),
    "power-cycle":  ( "PC", ( I, last() ) ),
    "hertz":        ( "Hz", ( scale( 10.0 ), avg() ) ),
    "volt-amps":    ( "VA", ( scale( 10.0 ), avg() ) ),
}
revcounters = dict( [ ( x[ 1 ][ 0 ], x[ 0 ] ) for x in counters.items() ] )

if options.list_counters:
    print "Available counters:"
    names = list( counters.keys() )
    names.sort()
    for name in names:
        print "   ", name
    exit()
if options.count and options.log_style != "external":
    print >>sys.stderr, "ERROR: must use 'external' log style for counted samples"
    exit( 1 )
if options.log_style not in [ "internal", "external" ]:
    print >>sys.stderr, "ERROR: log style must be 'internal' or 'external'\n"
    parser.print_help()
    exit( 1 )

if len( args ) < 2:
    parser.print_usage()
    exit()

dev = args[ 0 ]
cmd = args[ 1: ]

if not os.path.exists( dev ):
    dev2 = os.path.join( "/dev", dev )
    if not os.path.exists( dev2 ):
        print >>sys.stderr, "ERROR: could not open device", dev
        exit( 1 )
    dev = dev2

enabled = dict()
if len( options.e ) == 0:
    enabled.update( counters.values() )
else:
    for c in options.e:
        if c in counters:
            enabled.update( [ counters[ c ] ] )
        else:
            print >>sys.stderr, "ERROR: unknown counter '%s'\n" % c
            parser.print_help()
            exit( 1 )

try:
    wu = WattsUp( dev )
except serial.serialutil.SerialException as e:
    print "ERROR:", e
    exit( 1 )

conds = list()
inst = [ I ]
summ = [ I ]
header = [ "time" ]
for field in wu.readReply( "#H,R,0;" )[ 2 ]:
    if field in enabled:
        conds.append( 1 )
        inst.append( enabled[ field ][ 0 ] )
        summ.append( enabled[ field ][ 1 ] )
        header.append( revcounters[ field ] )
    else:
        conds.append( 0 )
wu.readReply( "#C,W,%d,%s;" % ( len( conds ), ",".join( map( str, conds ) ) ) )

log = None
if options.o:
    log = open( options.o, 'w' )
elif options.i:
    log = sys.stdout
if log:
    print >>log, ",".join( header )

totals = [ 0.0 ] * len( header )
try:
    start = time.time()
    t1 = start
    p = None
    waiting = False
    status = 0

    if options.log_style == "external":
        reply = wu.readReply( "#L,W,3,E,1,1;" )
        if options.count:
            def cond():
                options.count -= 1
                return options.count >= 0
        else:
            cond = lambda: True
        while cond():
            if ( p is not None ) and ( p.poll() is not None ) and not waiting:
                waiting = True
                t1 = time.time()
                status = p.returncode
                p = None
            delta = time.time() - t1
            if ( p is None ) and ( not waiting ) and delta >= options.bracket:
                p = Popen( cmd )
            if waiting and delta >= options.bracket:
                break

            if reply[ 0 ] == "d":
                data, totals = processData( delta + t1 - start, conds, reply[2] )
                if options.i:
                    print >>log, ",".join( map( str, data ) )

            reply = wu.readReply()
    else:
        wu.readReply( "#R,W,0;" )
        wu.readReply( "#O,W,1,2;" )
        wu.readReply( "#L,W,3,I,1,1;" )
        time.sleep( options.bracket )
        p = Popen( cmd )
        status = p.wait()
        time.sleep( options.bracket )
except KeyboardInterrupt:
    if p is not None:
        p.send_signal( signal.SIGINT )

if options.log_style == "internal":
    print "reading data..."
    reply = wu.readReply( "#D,R,0;" )
    interval, count = map( int, reply[ 2 ][ 1:3 ] )
    for i in range( count ):
        reply = wu.readReply()
        if not reply[ 0 ] == "d":
            break
        delta = interval * ( i + 1 )
        data, totals = processData( delta, conds, reply[ 2 ] )
        if options.i:
            print >>log, ",".join( map( str, data ) )

if options.o:
    if not options.i:
        print >>log, ",".join( map( str, [ delta ] + totals ) )
else:
    w = max( map( len, header ) )
    fmt = "%%%ds: %%g" % w
    for h, t in zip( header, totals ):
        print fmt % ( h, t )
exit( status )

