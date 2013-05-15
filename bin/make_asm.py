#!/usr/bin/env python3

import sys, os, io, subprocess
from functools import reduce

outfileName = "asmout.s"
comp = "g++"
included = set()

allname = "allsrc.cpp"

try:
  allsrc = io.open( allname, "w")
except Exception as e:
  print( "Cant create a file in the current directory. Aborting." )
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

def processSrc( fileName ):
  global outfileName, included
  if fileName in included: return
  included.add( fileName )
  if not os.path.isfile( fileName ): return
  try:
    workingFile = io.open( fileName )
    lines = workingFile.readlines()
    os.chdir(os.path.split(os.path.abspath(workingFile.name))[0])
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
    workingFile.close()
  except Exception as e:
    print( "The file " + fileName + " does not exist." )
  finally:
    pass

def main():
  global outfileName, comp, allsrc
  args = sys.argv
  if len( args ) < 2 or len(args) > 4: usage()
  mainfile = args[1]

  if len( args ) > 2: outfileName = args[2]
  if len( args ) > 3: comp        = args[3]

  processSrc( mainfile )
  allsrc.close()
  
  if comp == "gcc":
    print( "using gcc: " )
    subprocess.call( [comp, "-S", "-x", "c", allname, "-o", outfileName] )
  else:
    subprocess.call( [comp, "-S", allname, "-o", outfileName] )
  subprocess.call( ["rm", allname] )

if __name__ == '__main__':
	main()
