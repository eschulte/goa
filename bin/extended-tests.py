#!/usr/bin/python2

from glob import glob
import math
from optparse import OptionParser
import os
import random
import re
import shutil
from subprocess import CalledProcessError, call, check_call, PIPE, Popen
import sys
import tarfile
import tempfile
import threading
import time

parser = OptionParser( usage = "%prog [options] benchmark executable [testid]" )
parser.add_option(
    "-g", "--generate", metavar = "N", type = int,
    help = "generate N additional tests"
)
parser.add_option(
    "--search", action = "store_true",
    help = "skip generated inputs that fail with the golden implementation"
)
parser.add_option(
    "--timeout", metavar = "sec", type = int,
    help = "do not generate tests that take longer than sec seconds"
)
parser.add_option(
    "-v", "--verbose", action = "store_true",
    help = "show diagnostic information for failed tests"
)
parser.add_option(
    "-s", "--skip", metavar = "SKIP", type = int,
    help = "skip test number SKIP"
)
options, args = parser.parse_args()

if len( args ) < 2 or len( args ) > 3:
    parser.print_help()
    exit( 1 )

benchmark  = args[ 0 ]
executable = args[ 1 ]
if len( args ) > 2:
    testid = int( args[ 2 ] )
else:
    testid = None

root = os.path.dirname( os.path.dirname( os.path.abspath( sys.argv[ 0 ] ) ) )

# Set up handles for verbose vs quiet running.

if options.verbose:
    stdout = sys.stdout
    stderr = sys.stderr
else:
    devnull = open( "/dev/null" )
    stdout = devnull
    stderr = devnull

# Implement timeouts via convenience wrappers around subprocess methods

if options.timeout is not None:
    def timeout( p ):
        try:
            p.kill()
        except Exception as e:
            print e

    def run( cmd, *args, **kargs ):
        if not "stdout" in kargs:
            kargs[ "stdout" ] = stdout
        if not "stderr" in kargs:
            kargs[ "stderr" ] = stderr
        p = Popen( cmd, *args, **kargs )
        t = threading.Timer( float( options.timeout ), timeout, [ p ] )
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

    def check_run( cmd, *args, **kargs ):
        returncode = run( cmd, *args, **kargs )
        if returncode != 0:
            raise CalledProcessError(
                returncode = returncode,
                cmd = cmd,
                output = None
            )
else:
    def run( cmd, *args, **kargs ):
        if not "stdout" in kargs:
            kargs[ "stdout" ] = stdout
        if not "stderr" in kargs:
            kargs[ "stderr" ] = stderr
        return call( cmd, *args, **kargs )

    def check_run( cmd, *args, **kargs ):
        if not "stdout" in kargs:
            kargs[ "stdout" ] = stdout
        if not "stderr" in kargs:
            kargs[ "stderr" ] = stderr
        check_call( cmd, *args, **kargs )

# Some simple utility functions

def mktemp( suffix = "" ):
    tmp = tempfile.NamedTemporaryFile( delete = False, suffix = suffix )
    tmp.close()
    return tmp.name

def find_input( root, suffix ):
    for d, dnames, fnames in os.walk( root ):
        for fname in fnames:
            if fname.endswith( suffix ):
                return os.path.join( d, fname )
    raise Exception( "could not find input in " + root )

def find_golden( benchmark ):
    bmark_dir = os.path.join( root, "benchmarks", benchmark )
    matches = list()
    for d, dnames, fnames in os.walk( bmark_dir ):
        for fname in fnames:
            if fname == benchmark + ".orig":
                matches.append( os.path.join( d, fname ) )
    if len( matches ) != 1:
        raise Exception( "couldn't find golden binary for " + benchmark )
    return matches[ 0 ]

def coin( p_heads = 0.5 ):
    return random.random() < p_heads

class GoldenFailure( StandardError ):
    pass

class TarballsMixin:
    def __init__( self ):
        if not hasattr( self.__class__, "unpacked" ):
            self.__class__.unpacked = dict()

    def _exit_hook( self ):
        unpacked = self.__class__.unpacked
        for key in unpacked.keys():
            shutil.rmtree( unpacked[ key ] )
            del unpacked[ key ]

    def _unpack( self, basedir, name ):
        if not name in self.__class__.unpacked:
            tarball = os.path.join( basedir, "inputs", "input_%s.tar" % name )
            with tarfile.open( tarball ) as tarball:
                tmp = tempfile.mkdtemp()
                tarball.extractall( tmp )
            self.__class__.unpacked[ name ] = tmp
        return self.__class__.unpacked[ name ]

class Benchmark:
    def __init__( self, bmark ):
        self.bmark   = bmark
        self.basedir = os.path.join(
            root, "benchmarks", "parsec-3.0", "pkgs", "apps", bmark
        )
        self.inputs  = list()
        self.outputs = list()

        input_dir = os.path.join( root, "etc", "additional-inputs", bmark )
        for fname in glob( os.path.join( input_dir, bmark + ".input.*" ) ):
            try:
                num = int( os.path.splitext( fname )[ 1 ][ 1: ] )
            except:
                continue
            while len( self.inputs ) <= num:
                self.inputs.append( None )
            self.inputs[ num ] = fname
        self.inputs = filter( lambda x: x is not None, self.inputs )

        for i in range( len( self.inputs ) ):
            golden = self.__get_golden_name( i )
            if os.path.exists( golden ):
                self.outputs.append( golden )
            else:
                self.outputs.append( None )

        if not hasattr( self.__class__, "entered" ):
            self.__class__.entered = 0

    def __enter__( self ):
        if self.__class__.entered == 0:
            self._enter_hook()
        self.__class__.entered += 1
        return self

    def _enter_hook( self ):
        pass

    def __exit__( self, typ, val, trace ):
        self.__class__.entered -= 1
        if self.__class__.entered == 0:
            self._exit_hook()

    def _exit_hook( self ):
        pass

    def __get_golden_name( self, i ):
        return os.path.join(
            root, "etc", "additional-inputs", self.bmark,
            self.bmark + ".golden.%d" % i
        )

    def _get_next_input_name( self ):
        i = len( self.inputs )
        fname = os.path.join(
            root, "etc", "additional-inputs", self.bmark,
            self.bmark + ".input.%d" % i
        )
        return fname

    def _get_tars( self ):
        tardir = os.path.join( self.basedir, "inputs" )
        tars = list()
        for fname in glob( tardir + "/*" ):
            fname = os.path.basename( fname )
            fname = fname.replace( "input_", "" ).replace( ".tar", "" )
            tars.append( fname )
        return tars

    def getNumInputs( self ):
        return len( self.inputs )

    def check( self, i ):
        if self.outputs[ i ] is None:
            golden = find_golden( self.bmark )
            try:
                obj = self.__class__( golden )
                with obj:
                    tmp = obj.run( i )
            except Exception as e:
                raise GoldenFailure( e )
            master = self.__get_golden_name( i )
            shutil.move( tmp, master )
            self.outputs[ i ] = master
        return self._check_impl( i )

class Blackscholes( TarballsMixin, Benchmark ):
    def __init__( self, executable ):
        TarballsMixin.__init__( self )
        Benchmark.__init__( self, "blackscholes" )
        self.executable = executable

    def generate( self, num_inputs ):
        nlines = 10000000
        for i in range( num_inputs ):
            x = random.uniform( 14, math.log( nlines, 2 ) )
            n = int( math.pow( 2, x ) )

            fname = self._get_next_input_name()
            with open( fname, 'w' ) as fh:
                for index in random.sample( xrange( nlines ), n ):
                    print >>fh, index
            self.inputs.append( fname )
            self.outputs.append( None )

    def run( self, i ):
        if options.verbose:
            print "Running %s (%d)" % ( self.executable, i )

        tmp = self._unpack( self.basedir, "native" )
        with open( find_input( tmp, ".txt" ) ) as fh:
            lines = fh.readlines()
        lines.pop( 0 )

        input_file = mktemp()
        try:
            with open( input_file, 'w' ) as out:
                with open( self.inputs[ i ] ) as fh:
                    for line in fh:
                        print >>out, lines[ int( line.strip() ) ]
            out = mktemp()
            try:
                check_run( [ self.executable, "1", input_file, out ] )
            except Exception as e:
                os.remove( out )
                raise e
        finally:
            os.remove( input_file )
        return out

    def _check_impl( self, i ):
        testfile = self.run( i )
        try:
            cmd = [ "diff", self.outputs[ i ], testfile ]
            return call( cmd, stdout = stdout, stderr = stderr ) == 0
        finally:
            os.remove( testfile )

class Bodytrack( TarballsMixin, Benchmark ):
    def __init__( self, executable ):
        TarballsMixin.__init__( self )
        Benchmark.__init__( self, "bodytrack" )
        self.executable = executable

    def generate( self, num_inputs ):
        for i in range( num_inputs ):
            fname = self._get_next_input_name()
            with open( fname, 'w' ) as fh:
                print >>fh, random.randrange( 4 ) + 1
                print >>fh, random.randrange( 1 + 261 )
                print >>fh, random.randrange( 1 + 4000 )
                print >>fh, random.randrange( 5 ) + 1
                print >>fh, 0
                print >>fh, random.randrange( 4 ) + 1
            self.inputs.append( fname )
            self.outputs.append( None )
                
    def run( self, i ):
        if options.verbose:
            print "Running %s (%d)" % ( self.executable, i )

        with open( self.inputs[ i ] ) as fh:
            n_cameras   = fh.next().strip()
            n_frames    = fh.next().strip()
            n_particles = fh.next().strip()
            n_layers    = fh.next().strip()
            thd_model   = fh.next().strip()
            n_threads   = fh.next().strip()

        dataset = self._unpack( self.basedir, "native" )
        tmp = tempfile.mkdtemp()
        try:
            tmpdata = os.path.join( tmp, "data" )
            shutil.copytree( dataset, tmpdata )
            tmpdata = os.path.join( tmpdata, glob( tmpdata + "/*" )[ 0 ] )

            cmd = [
                self.executable, tmpdata, n_cameras, n_frames,
                    n_particles, n_layers,
                    thd_model, n_threads, "1"
            ]
            check_run( cmd )

            cwd = os.getcwd()
            try:
                tar = tempfile.NamedTemporaryFile( suffix = ".tgz", delete = False )
                try:
                    os.chdir( tmpdata )
                    with tarfile.open( fileobj = tar, mode = "w:gz" ) as tarball:
                        if os.path.exists( "poses.txt" ):
                            tarball.add( "poses.txt" )
                        for fname in glob( "Result*.bmp" ):
                            tarball.add( fname )
                    tar.close()
                except Exception as e:
                    try:
                        tar.close()
                    except: pass
                    os.remove( tar.name )
            finally:
                os.chdir( cwd )
        finally:
            shutil.rmtree( tmp )
        return tar.name

    def _check_impl( self, i ):
        tarname = self.run( i )

        gooddir = tempfile.mkdtemp()
        try:
            with tarfile.open( self.outputs[ i ] ) as tarball:
                tarball.extractall( gooddir )
            testdir = tempfile.mkdtemp()
            try:
                with tarfile.open( tarname ) as tarball:
                    tarball.extractall( testdir )
                cmd = [ "diff", "-r", gooddir, testdir ]
                return call( cmd, stdout = stdout, stderr = stderr ) == 0
            finally:
                shutil.rmtree( testdir )
        finally:
            shutil.rmtree( gooddir )

class Facesim( Benchmark ):
    def __init__( self, executable ):
        Benchmark.__init__( self, "facesim" )
        self.executable = executable

    def generate( self, num_inputs ):
        tars = [ "simlarge", "native" ]
        for i in range( num_inputs ):
            fname = self._get_next_input_name()
        
class Ferret( TarballsMixin, Benchmark ):
    def __init__( self, executable ):
        TarballsMixin.__init__( self )
        Benchmark.__init__( self, "ferret" )
        self.executable = executable

    def generate( self, num_inputs ):
        tars = self._get_tars()
        for i in range( num_inputs ):
            fname = self._get_next_input_name()
            with open( fname, 'w' ) as fh:
                print >>fh, random.choice( tars )
                print >>fh, "corel"
                print >>fh, "lsh"
                print >>fh, "queries"
                print >>fh, random.randrange( 75 ) + 1
                print >>fh, random.randrange( 20 ) + 5
                # TODO: handle line reordering due to multithreading
                print >>fh, 1
            self.inputs.append( fname )
            self.outputs.append( None )

    def run( self, i ):
        if options.verbose:
            print "Running %s (%d)" % ( self.executable, i )

        with open( self.inputs[ i ] ) as fh:
            tarname   = fh.next().strip()
            metadir   = fh.next().strip()
            algorithm = fh.next().strip()
            imagedir  = fh.next().strip()
            args = list()
            for line in fh:
                args.append( line.strip() )

        tardir = self._unpack( self.basedir, tarname )
        metadir  = os.path.join( tardir, metadir )
        imagedir = os.path.join( tardir, imagedir )

        tmp = mktemp()
        cmd = [ self.executable, metadir, algorithm, imagedir ] + args + [ tmp ]
        try:
            check_run( cmd )
            
            out = mktemp()
            try:
                with open( out, 'w' ) as dst:
                    prefix_pat = re.compile( "^.*queries" )
                    with open( tmp ) as src:
                        for line in src:
                            line = re.sub( prefix_pat, "queries", line.strip() )
                            print >>dst, line
            except Exception as e:
                os.remove( out )
                raise e
        finally:
            os.remove( tmp )
        return out

    def _check_impl( self, i ):
        testfile = self.run( i )
        try:
            cmd = [ "diff", self.outputs[ i ], testfile ]
            return call( cmd, stdout = stdout, stderr = stderr ) == 0
        finally:
            os.remove( testfile )

class Fluidanimate( TarballsMixin, Benchmark ):
    def __init__( self, executable ):
        TarballsMixin.__init__( self )
        Benchmark.__init__( self, "fluidanimate" )
        self.executable = executable

    def generate( self, num_inputs ):
        tars = self._get_tars()
        for i in range( num_inputs ):
            fname = self._get_next_input_name()
            arg = int( math.pow( 500, random.random() ) )
            with open( fname, 'w' ) as fh:
                print >>fh, random.choice( tars )
                print >>fh, 1
                print >>fh, arg
            self.inputs.append( fname )
            self.outputs.append( None )

    def run( self, i ):
        if options.verbose:
            print "Running %s (%d)" % ( self.executable, i )

        with open( self.inputs[ i ] ) as fh:
            tarname  = fh.next().strip()
            nthreads = fh.next().strip()
            arg      = fh.next().strip()

        tardir = self._unpack( self.basedir, tarname )
        input_file = find_input( tardir, ".fluid" )

        tmp = mktemp()
        cmd = [ self.executable, nthreads, arg, input_file, tmp ]
        try:
            check_run( cmd )
        except Exception as e:
            os.remove( tmp )
            raise e
        return tmp

    def _check_impl( self, i ):
        testfile = self.run( i )
        try:
            cmd = [ "diff", self.outputs[ i ], testfile ]
            return call( cmd, stdout = stdout, stderr = stderr ) == 0
        finally:
            os.remove( testfile )

class Freqmine( TarballsMixin, Benchmark ):
    def __init__( self, executable ):
        TarballsMixin.__init__( self )
        Benchmark.__init__( self, "freqmine" )
        self.executable = executable

    def generate( self, num_inputs ):
        tars = self._get_tars()
        for i in range( num_inputs ):
            fname = self._get_next_input_name()
            if coin():
                arg = int( math.pow( 11000, random.random() ) )
            else:
                arg = random.random()
            with open( fname, 'w' ) as fh:
                print >>fh, random.choice( tars )
                print >>fh, arg
            self.inputs.append( fname )
            self.outputs.append( None )

    def run( self, i ):
        if options.verbose:
            print "Running %s (%d)" % ( self.executable, i )

        with open( self.inputs[ i ] ) as fh:
            tarname = fh.next().strip()
            arg     = fh.next().strip()

        tardir = self._unpack( self.basedir, tarname )
        input_file = find_input( tardir, ".dat" )

        tmp = mktemp()
        try:
            cmd = [ self.executable, input_file, arg, tmp ]
            env = dict( os.environ )
            env[ "OMP_NUM_THREADS" ] = "1"
            check_run( cmd, env = env )
        except Exception as e:
            os.remove( tmp )
            raise e
        return tmp

    def _check_impl( self, i ):
        testfile = self.run( i )
        try:
            cmd = [ "diff", self.outputs[ i ], testfile ]
            return call( cmd, stdout = stdout, stderr = stderr ) == 0
        finally:
            os.remove( testfile )

class Swaptions( Benchmark ):
    def __init__( self, executable ):
        Benchmark.__init__( self, "swaptions" )
        self.executable = executable

    def generate( self, num_inputs ):
        for i in range( num_inputs ):
            fname = self._get_next_input_name()
            nthreads = 1
            args = list()
            if coin():
                nthreads = random.randint( 1, 4 )
                args.append( ( "-nt", nthreads ) )

            ns = 0
            while ns < nthreads:
                ns = int( pow( 2, random.uniform( 0, 8 ) ) )
            args.append( ( "-ns", ns ) )

            if coin():
                args.append( ( "-sm", int( pow( 10, random.uniform( 0, 6 ) ) ) ) )
            if coin():
                args.append( ( "-sd", random.randrange( 2 ** 32 ) ) )
            random.shuffle( args )
            with open( fname, 'w' ) as fh:
                for key, value in args:
                    print >>fh, key, value
            self.inputs.append( fname )
            self.outputs.append( fname )

    def run( self, i ):
        if options.verbose:
            print "Running %s (%d)" % ( self.executable, i )

        cmd = [ self.executable ]
        with open( self.inputs[ i ] ) as fh:
            for line in fh:
                cmd.extend( line.split() )

        tmp = mktemp()
        try:
            with open( tmp, 'w' ) as out:
                check_run( cmd, stderr = out )
        except Exception as e:
            os.remove( tmp )
            raise e
        return tmp

    def _check_impl( self, i ):
        testfile = self.run( i )
        try:
            cmd = [ "diff", self.outputs[ i ], testfile ]
            return call( cmd, stdout = stdout, stderr = stderr ) == 0
        finally:
            os.remove( testfile )

class Vips( TarballsMixin, Benchmark ):
    def __init__( self, executable ):
        TarballsMixin.__init__( self )
        Benchmark.__init__( self, "vips" )
        self.executable = executable

    def generate( self, num_inputs ):
        info_args = [
            ( 1, "--help" ),
            ( 1, "--help-all" ),
            ( 1, "--help-vips" ),
            ( 1, "--list=all" ),
            ( 1, "--list=packages" ),
            ( 1, "--list=classes" ),
        ]
        for i in range( len( info_args ) ):
            if len( self.inputs ) <= i:
                fname = self._get_next_input_name()
                with open( fname, 'w' ) as fh:
                    print >>fh, info_args[ i ][ 0 ]
                    print >>fh, " ".join( info_args[ i ][ 1: ] )
                self.inputs.append( fname )
                self.outputs.append( None )

        master = find_golden( self.bmark )
        p = Popen( [ master, "--list=all" ], stdout = PIPE, stderr = stderr )
        output = p.communicate()[ 0 ]
        for i, line in enumerate( output.splitlines(), len( info_args ) ):
            op = line.split()[ 0 ]
            if len( self.inputs ) <= i:
                fname = self._get_next_input_name()
                with open( fname, 'w' ) as fh:
                    print >>fh, 2
                    print >>fh, "--usage=%s" % op
                self.inputs.append( fname )
                self.outputs.append( None )

        tars = self._get_tars()
        # TODO: find some ops we can test...
        ops = [ ]
        if len( ops ) > 0:
            for i in range( num_inputs ):
                fname = self._get_next_input_name()
                with open( fname, 'w' ) as fh:
                    print >>fh, 0
                    print >>fh, random.choice( tars )
                    print >>fh, random.choice( ops )
                self.inputs.append( fname )
                self.outputs.append( None )

    def run( self, i ):
        if options.verbose:
            print "Running %s (%d)" % ( self.executable, i )

        cmd = [ self.executable ]
        with open( self.inputs[ i ] ) as fh:
            channel = fh.next().strip()
            if channel == "0":
                tarname = fh.next().strip()
            opname = fh.next().strip()
        cmd.append( opname )

        if channel == "0":
            tardir = self._unpack( self.basedir, arname )
            input_file = find_input( tardir, ".v" )
            cmd.append( input_file )

        tmp = mktemp( ".v" )
        try:
            if channel == "1":
                with open( tmp, 'w' ) as out:
                    check_run( cmd, stdout = out )
            elif channel == "2":
                with open( tmp, 'w' ) as out:
                    check_run( cmd, stderr = out )
            if channel == "0":
                cmd.append( tmp )
                check_run( cmd )
                tmp2 = mktemp()
                try:
                    with open( tmp2, 'w' ) as out:
                        check_call(
                            [ "grep", "--binary-file=text", "-v", opname ],
                            stdout = out, stderr = stderr
                        )
                    t = tmp
                    tmp = tmp2
                    tmp2 = t
                finally:
                    os.remove( tmp2 )
        except Exception as e:
            os.remove( tmp )
            raise e
        return tmp

    def _check_impl( self, i ):
        testfile = self.run( i )
        try:
            cmd = [ "diff", self.outputs[ i ], testfile ]
            return call( cmd, stdout = stdout, stderr = stderr ) == 0
        finally:
            os.remove( testfile )

class X264( TarballsMixin, Benchmark ):
    def __init__( self, executable ):
        TarballsMixin.__init__( self )
        Benchmark.__init__( self, "x264" )
        self.executable = executable

    def generate( self, num_inputs ):
        extras = glob( os.path.join(
            root, "etc", "additional-inputs", self.bmark, "*.y4m" )
        )
        inputs = self._get_tars()
        for fname in extras:
            inputs.append( fname[ len( root ) + 1: ] )
        for i in range( num_inputs ):
            args = [
                ( [ "-I", "--keyint" ], random.randrange( 250 ) + 1 ),
                ( [ "-b", "--bframes" ], 2 ** random.randrange( 4 ) ),
                ( [ "--b-pyramid" ], ),
                ( [ "--no-cabac" ], ),
                ( [ "-r", "--ref" ], random.randrange( 10 ) + 1 ),
                ( [ "--interlaced" ], ),
                ( [ "-q", "--qp" ], random.randrange( 50 ) ),
                ( [ "-B", "--bitrate" ], random.uniform( 0.5, 4.0 ) ),
                ( [ "--crf" ], random.random() ),
                ( [ "--vbv-bufsize" ], random.randrange( 10 ) ),
                ( [ "--ratetol" ], random.uniform( 0, 2 ) ),
                ( [ "--ipratio" ], random.uniform( 0.5, 2 ) ),
                ( [ "--pbratio" ], random.uniform( 0.5, 2 ) ),
                ( [ "--aq-strength" ], random.uniform( 0.5, 1.5 ) ),
                ( [ "-p", "--pass" ], random.choice( [ 1, 2, 3 ] ) ),
                ( [ "--qcomp" ], random.random() ),
                ( [ "-A", "--partitions" ], random.sample( [ "p8x8", "p4x4", "b8x8", "i8x8", "i4x4" ], random.randrange( 5 ) + 1 ) ),
                ( [ "--direct" ], random.choice( [ "none", "spatial", "temporal", "auto" ] ) ),
                ( [ "-w", "--weightb" ], random.random() ),
                ( [ "--me" ], random.choice( [ "dia", "hex", "umh" ] ) ),
                ( [ "--merange" ], random.randrange( 20 ) + 1 ),
                ( [ "-m", "--subme" ], random.randrange( 9 ) + 1 ),
                ( [ "--mixed-refs" ], ),
                ( [ "-8", "--8x8dct" ], ),
                ( [ "--no-fast-pskip" ], ),
                ( [ "--no-dct-decimate" ], ),
                ( [ "--nr" ], random.randrange( 2 ) ),
                ( [ "--fps" ], random.uniform( 25, 35 ) ),
            ]

            fname = self._get_next_input_name()
            with open( fname, 'w' ) as fh:
                print >>fh, random.choice( inputs )
                print >>fh, random.choice( [ ".264", ".mkv" ] )
                for row in args:
                    if coin( 8.0 / len( args ) ):
                        switch = random.choice( row[ 0 ] )
                        row = map( str, row[ 1: ] )
                        print >>fh, " ".join( [ switch ] + list( row ) )
            self.inputs.append( fname )
            self.outputs.append( None )

    def run( self, i ):
        if options.verbose:
            print "Running %s (%d)" % ( self.executable, i )

        cleanup = list()
        try:
            passes = ( 0, 0 )
            cmd = [ self.executable ]
            with open( self.inputs[ i ] ) as fh:
                tarname = fh.next().strip()
                suffix  = fh.next().strip()
                for line in fh:
                    args = line.strip().split()
                    cmd.extend( args )
                    if args[ 0 ] == "--pass":
                        passes = ( int( args[ 1 ] ), len( cmd ) - 1 )
                        statfile = mktemp( ".log" )
                        cmd.extend( [ "--stats", statfile ] )
                        cleanup.append( statfile )

            if len( os.path.dirname( tarname ) ) > 0:
                input_file = os.path.join( root, tarname )
            else:
                tardir = self._unpack( self.basedir, tarname )
                input_file = find_input( tardir, ".y4m" )

            tmp = mktemp( suffix )
            try:
                cmd.extend( [ input_file, "-o", tmp, "--threads", "1" ] )
                if passes[ 0 ] > 0:
                    for i in range( passes[ 0 ] ):
                        cmd[ passes[ 1 ] ] = str( i + 1 )
                        check_run( cmd )
                else:
                    check_run( cmd )
            except Exception as e:
                os.remove( tmp )
                raise e
        finally:
            for fname in cleanup:
                if os.path.exists( fname ):
                    os.remove( fname )
        return tmp

    def _check_impl( self, i ):
        testfile = self.run( i )
        try:
            cmd = [ "diff", self.outputs[ i ], testfile ]
            return call( cmd, stdout = stdout, stderr = stderr ) == 0
        finally:
            os.remove( testfile )

ctors = {
    "blackscholes": Blackscholes,
    "bodytrack":    Bodytrack,
    "ferret":       Ferret,
    "fluidanimate": Fluidanimate,
    "freqmine":     Freqmine,
    "swaptions":    Swaptions,
    "vips":         Vips,
    "x264":         X264,
}
if not benchmark in ctors:
    print "tests for %s not implemented yet" % benchmark
    exit( 1 )

if options.generate is not None:
    generated = 0
    failures  = 0
    while generated < options.generate:
        golden = ctors[ benchmark ]( find_golden( benchmark ) )
        golden.generate( 1 )
        if options.search:
            n = golden.getNumInputs() - 1
            try:
                with golden:
                    start = time.time()
                    success = golden.check( n )
                    delta = ( time.time() - start ) / 2
                    if not success:
                        print "original program cannot reproduce results"
                        raise Exception(
                            "original program cannot reproduce results"
                        )
                    if options.timeout is not None and delta > options.timeout:
                        print "execution exceeded timeout (%f)" % delta
                        raise Exception(
                            "execution exceeded timeout (%f)" % delta
                        )
                generated += 1
            except Exception as e:
                if options.verbose:
                    print e
                os.remove( golden.inputs[ -1 ] )
                if golden.outputs[ -1 ] is not None and \
                        os.path.exists( golden.outputs[ -1 ] ):
                    os.remove( golden.outputs[ -1 ] )
                failures += 1
                if failures % 10 == 0:
                    print "found %d, rejected %d" % ( generated, failures )
            except KeyboardInterrupt as e:
                os.remove( golden.inputs[ -1 ] )
                exit( 1 )
        else:
            generated += 1
    print "Generated %d test inputs (%d skipped)" % ( generated, failures )

bmark = ctors[ benchmark ]( executable )

if bmark.getNumInputs() == 0:
    print "no inputs defined for", benchmark
exitcode = 0
with bmark:
    def runtest( i ):
        global exitcode
        try:
            status = bmark.check( i )
        except GoldenFailure as e:
            raise e
        except Exception as e:
            if options.verbose:
                print e
            status = False
        if status:
            print "%d: pass" % i
        else:
            print "%d: FAIL" % i
            exitcode = 1
    if testid is None:
        for i in range( bmark.getNumInputs() ):
            if options.skip is None or (not options.skip == i):
                runtest( i ) 
    else:
        runtest( testid )
    exit( exitcode )

