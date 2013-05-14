#!/usr/bin/env python3

import sys, os, io, subprocess

outfileName = "asmout.s"

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
  global outfileName
  try:
    workingFile = io.open( fileName )
    lines = workingFile.readlines()
    for line in lines:
      if line.split('"')[0] == "#include ":
        processSrc( line.split('"')[1])
      else:
        allsrc.write( line )

  except Exception as e:
    raise e
  finally:
    pass

def main():
  global outfileName
  args = sys.argv
  if len( args ) < 2 or len(args) > 3: usage()
  mainfile = args[1]

  if len( args ) > 1: outfileName = args[1]
  
  processSrc( mainfile )

  allsrc.close()

  # for root, dirs, files in os.walk(path):
  #   for f in files:
  #     genstats(root, f)
  # stats.close()


if __name__ == '__main__':
	main()
