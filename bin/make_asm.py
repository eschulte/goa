#!/usr/bin/env python3

import sys, os, io, subprocess
from functools import reduce

outfileName = "asmout.s"

included = set()

try:
  allsrc = io.open( "allsrc.cpp", "w")
except Exception as e:
  print( "Cant create a file in the current directory. Aborting." )
  sys.exit( 2 )


def usage():
  global outfileName
  print(
    "USAGE:\n" +
    sys.argv[0] + " mainfile [asmfile]\n"
    "  Where mainfile is the source file with the main function and asmfile is the\n" +
    "  desired output assembly file. (If no argument is given a file named " + outfileName + "\n" +
    "  will be written to the current directory)"
  )
  sys.exit( 1 )

def processSrc( fileName ):
  global outfileName, included
  if fileName in included: return
  included.add( fileName )
  # print( "trying to process " + fileName )
  if not os.path.isfile( fileName ): 
    # print( "could not open " + fileName )
    return
  try:
    workingFile = io.open( fileName )
    lines = workingFile.readlines()
    for line in lines:
      if line.split('"')[0].strip() == "#include":
        include = line.split('"')[1]
        if include not in included:
          allsrc.write( "/* Begin: trying to include " + include + " */\n")
          processSrc( include )
          allsrc.write( "/* End: trying to include " + include + " */\n")
      else:
        allsrc.write( line )
    if fileName.split('.')[-1][0] == 'h':
      # get the filename minus the .h__ extention. Multiple '.'s are OK
      cname = reduce( lambda a, b : a + '.' + b, fileName.split('.')[:-1] )
      processSrc( cname + '.c' )
      processSrc( cname + '.cpp' )
      processSrc( cname + '.cxx' )
      processSrc( cname + '.c++' )

  except Exception as e:
    print( "The file " + fileName + " does not exist." )
  finally:
    pass

def main():
  global outfileName
  args = sys.argv
  if len( args ) < 2 or len(args) > 3: usage()
  mainfile = args[1]

  if len( args ) > 1: outfileName = args[1]
  
  processSrc( mainfile )

  # print( str(included) )
  allsrc.close()



if __name__ == '__main__':
	main()
