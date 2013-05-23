#!/usr/bin/env python3

import sys, os, io, subprocess
from functools import reduce

outfileName  = "asmout.s"
comp         = "g++"
included     = set()
bytesWritten = 0
allname      = "allsrc.cpp"

try:
  allsrc = io.open( allname, "w")
except Exception as e:
  print( "Can't create a file in the current directory. Aborting." )
  sys.exit( 2 )


def usage():
  global outfileName
  print(
    "USAGE:\n" +
    sys.argv[0] + " mainfile [asmfile] [compiler]\n"
    "\tmainfile : the source file with the main function\n"
    "\tasmfile  : the desired output assembly file\n"
    "\t           (if not given, the file will be " + outfileName + ")\n"
    "\tcompiler : the desired compiler (default g++) The compiler must support the -S and -o flags"
  )
  sys.exit( 1 )

def processSrc( fileName ):
  global outfileName, included, bytesWritten
  if fileName.split( '/' )[-1] in included: return
  included.add( fileName.split( '/' )[-1] )
  if not os.path.isfile( fileName ): return

  try:
    workingFile = open( fileName, encoding="ISO-8859-2" )
    lines = workingFile.readlines()
    for line in lines:
      quote = False
      angle = False
      if line.split('"')[0].strip() == "#include": quote = True
      if line.split( '<' )[0].strip() == "#include": angle = True
      if angle or quote:
        if quote: include = line.split('"')[1]
        if angle: include = line.split('<')[1].strip()[:-1]
        if True: #include.split( '/' )[-1] not in included:
          #  allsrc.write( "/* Begin: trying to include " + include + " */\n")
          # currentlyWritten = bytesWritten
          if angle: allsrc.write( '#include <' + include + '>\n' )
          if quote: 
            allsrc.write( '#include "' + include.split( '/' )[-1] + '"\n' )
            # print( 'adding ' + include + ' as ' + include.split( '/' )[-1] + '"\n')
          # if currentlyWritten == bytesWritten: 
          #   # no need to notify that #include <standard_includes.h> isn't in the currrent dir
          #   if quote: print( "Failed to include " + include + 
          #                    " in the file " + fileName + ".\n"
          #                    "Current directory: " + 
          #                    os.path.abspath('.') )
          #   allsrc.write( line )
          #allsrc.write( "/* End: trying to include " + include + " */\n")
          
      else:
        # uncomment the following line to print the original filename as a comment
        # every other line. (this can help in the debugging porcess.)
        # allsrc.write( "// " + fileName.split( '/' )[-1] + " //\n " )
        allsrc.write( line )
        bytesWritten += len( line )
    # if fileName.split('.')[-1][0] == 'h':
      # get the filename minus the .h__ extension. Multiple '.'s are OK
      # cname = reduce( lambda a, b : a + '.' + b, fileName.split('.')[:-1] )
      # processSrc( cname + '.c'   )
      # processSrc( cname + '.cpp' )
      # processSrc( cname + '.cxx' )
      # processSrc( cname + '.c++' )
    workingFile.close()
    #os.chdir( oldpath )
  except Exception as e:
    print( "Got an error while manipulating " + fileName + ". Error: " + str(e) )
  finally:
    pass

def main():
  global outfileName, comp, allsrc, allname
  args = sys.argv
  if len( args ) < 2 or len(args) > 4: usage()
  mainfile = args[1]

  if len( args ) > 2: outfileName = args[2]
  if len( args ) > 3: comp        = args[3]
  
  processSrc( mainfile )
  allsrc.close()

  # try to compile the monolithic .cpp file ... this may fail if macros are redefined 
  # or other collisions occur.
  if comp == "gcc": 
    print( "using gcc: " )
    subprocess.call( [comp, "-S", "-x", "c", allname, "-o", outfileName] )
  else:
    subprocess.call( [comp, "-S", allname, "-o", outfileName] )
    # uncomment the following line to remove allsource.cpp automatically
    #subprocess.call( ["rm", allname] )

if __name__ == '__main__':
	main()
