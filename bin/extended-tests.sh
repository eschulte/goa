#!/bin/bash

# This script must live in optimization/bin so that the script can
# find the path to parsec. PARSEC 3.0 must be installed in 
# optimization/benchmarks/parsec-3.0
#
# The benchmark must have been previously built with parsecmgmt so
# that the original build can be used as an oracle

BIN_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PARSEC_DIR=$BIN_DIR/../benchmarks/parsec-3.0/
. $BIN_DIR/common

function usage()
{
  printf "USAGE:\n"
  printf "$0 <benchmark name> <path to executable to test>\n"
  printf "where <benchmark name> is one of:\n"
  for b in ${BENCHMARKS[@]}
  do
    echo -e "  $b"
  done  
}

if [[ $# -le 1 ]]; then
  usage
  exit 1
fi

BENCHMARK=$1
EXECUTABLE=$2


case $BENCHMARK in
  blackscholes)
  error "Test for $BENCHMARK not implemented yet!"
  ;;
  bodytrack)
  error "Test for $BENCHMARK not implemented yet!"
  ;;
  facesim)
  error "Test for $BENCHMARK not implemented yet!"
  ;;
  ferret)
  error "Test for $BENCHMARK not implemented yet!"
  ;;
  fluidanimate)
  error "Test for $BENCHMARK not implemented yet!"
  ;;
  freqmine)
  error "Test for $BENCHMARK not implemented yet!"
  ;;
  raytrace)
  error "Test for $BENCHMARK not implemented yet!"
  ;;
  swaptions)
  error "Test for $BENCHMARK not implemented yet!"
  ;;
  vips)
  error "Test for $BENCHMARK not implemented yet!"
  ;;
  x264)
  error "Test for $BENCHMARK not implemented yet!"
  ;;
  *)
    usage
    error "Invalid benchmark \"$2\"!"
    ;;
esac



