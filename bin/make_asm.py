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
    "\tmainfile : the source file with the main funtion\n"
    "\tasmfile  : the desired output assembly file\n"
    "\t           (if not given, the file will be " + outfileName + ")\n"
    "\tcompiler : the desired compiler (default g++) The compiler must support the -S and -o flags"
  )
  sys.exit( 1 )

def processSrc( fileName, oldpath ):
  global outfileName, included, bytesWritten
  if fileName.split( '/' )[-1] in included: return
  included.add( fileName.split( '/' )[-1] )
  if not os.path.isfile( fileName ): return

  try:
    workingFile = io.open( fileName )
    lines = workingFile.readlines()
    # need to change to the directory that the file is in so that we can process #includes that
    # are specifiec using relative addressing. i.e. #include "../../../some/other/dir/lib.h"
    os.chdir(os.path.split(os.path.abspath(workingFile.name))[0])
    print( "Changing directory to: " + os.path.abspath( '.' ) + " to process " + fileName )
    currentPath = os.path.abspath( '.' )
    for line in lines:
      quote = False
      angle = False
      if line.split('"')[0].strip() == "#include": quote = True
      if line.split( '<' )[0].strip() == "#include": angle = True
      if angle or quote:
        if quote: include = line.split('"')[1]
        if angle: include = line.split('<')[1].strip()[:-1]
        if include.split( '/' )[-1] not in included:
          allsrc.write( "/* Begin: trying to include " + include + " */\n")
          currentlyWritten = bytesWritten
          processSrc( include, currentPath )
          if currentlyWritten == bytesWritten: 
            if quote: print( "Failed to include " + include + 
                             " in the file " + fileName + ".\n"
                             "Current directory: " + 
                             os.path.abspath('.') )
            allsrc.write( line )
          allsrc.write( "/* End: trying to include " + include + " */\n")
          
      else:
        # allsrc.write( "// " + fileName.split( '/' )[-1] + " //\n " + line )
        allsrc.write( line )
        bytesWritten += len( line )
    if fileName.split('.')[-1][0] == 'h':
      # get the filename minus the .h__ extention. Multiple '.'s are OK
      cname = reduce( lambda a, b : a + '.' + b, fileName.split('.')[:-1] )
      processSrc( cname + '.c',   currentPath )
      processSrc( cname + '.cpp', currentPath )
      processSrc( cname + '.cxx', currentPath )
      processSrc( cname + '.c++', currentPath )
    workingFile.close()
    os.chdir( oldpath )
  except Exception as e:
    print( "The file " + fileName + " does not exist." )
  finally:
    pass

def main():
  global outfileName, comp, allsrc, allname
  args = sys.argv
  if len( args ) < 2 or len(args) > 4: usage()
  mainfile = args[1]

  if len( args ) > 2: outfileName = args[2]
  if len( args ) > 3: comp        = args[3]
  
  processSrc( mainfile, os.path.abspath( '.' ) )

  # cd back to the starting directory
  os.chdir(os.path.split(os.path.abspath(allsrc.name))[0])
  allsrc.close()

  print( "Changing directory to original: " + os.path.abspath( '.' ) )
  if comp == "gcc": 
    print( "using gcc: " )
    subprocess.call( [comp, "-S", "-x", "c", allname, "-o", outfileName] )
  else:
    subprocess.call( [comp, "-S", allname, "-o", outfileName] )
  #subprocess.call( ["rm", allname] )

if __name__ == '__main__':
	main()
