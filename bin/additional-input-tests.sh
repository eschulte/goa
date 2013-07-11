#!/bin/bash
 
# This script must live in optimization/bin so that the script can
# find the path to parsec. PARSEC 3.0 must be installed in 
# optimization/benchmarks/parsec-3.0
#
# The benchmark must have been previously built with parsecmgmt so
# that the original build can be used as an oracle
 
BIN_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PARSEC_DIR=$BIN_DIR/../benchmarks/parsec-3.0/
BASE=$BIN_DIR/../
INPUT_DIR=$BASE/etc/additional-inputs/
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
    X264_FLAGS=""
    X264_FLAGS+=" --quiet --qp 20 --partitions b8x8,i4x4 --ref 5"
    X264_FLAGS+=" --direct auto --b-pyramid --weightb --mixed-refs"
    X264_FLAGS+=" --no-fast-pskip --me umh --subme 7"
    X264_FLAGS+=" --analyse b8x8,i4x4 --threads 1"
    if [[ ! -d $INPUT_DIR/x264 ]]; then
      if [[ ! -e $BASE/benchmarks/x264/x264 ]]; then
        $BIN_DIR/mgmt output x264
      fi
      mkdir $INPUT_DIR/x264
      curl http://media.xiph.org/video/derf/y4m/claire_qcif-5.994Hz.y4m -o $INPUT_DIR/x264/x264.input.0.y4m
      curl http://media.xiph.org/video/derf/y4m/bus_qcif_7.5fps.y4m     -o $INPUT_DIR/x264/x264.input.1.y4m
      curl http://media.xiph.org/video/derf/y4m/foreman_qcif_7.5fps.y4m -o $INPUT_DIR/x264/x264.input.2.y4m  
      curl http://media.xiph.org/video/derf/y4m/soccer_qcif_15fps.y4m   -o $INPUT_DIR/x264/x264.input.3.y4m
      curl http://media.xiph.org/video/derf/y4m/bus_cif_15fps.y4m       -o $INPUT_DIR/x264/x264.input.4.y4m
      for n in {0..4}; do
        echo "Making gold standard output $INPUT_DIR/x264/x264.golden.$n"
        $BASE/benchmarks/x264/x264 $X264_FLAGS $INPUT_DIR/x264/x264.input.$n.y4m -o $INPUT_DIR/x264/x264.golden.$n >/dev/null 2>&1        
      done 
    fi
    for n in {0..4}; do
      printf "Test $n: "
      $EXECUTABLE $X264_FLAGS $INPUT_DIR/x264/x264.input.$n.y4m -o $INPUT_DIR/x264/x264.tstout.$n >/dev/null 2>&1
      diff $INPUT_DIR/x264/x264.golden.$n $INPUT_DIR/x264/x264.tstout.$n >/dev/null 2>&1
      if [[ $? = 0 ]]; then
        printf "PASS\n"
      else
        printf "FAIL\n"
        printf "tried: diff $INPUT_DIR/x264/x264.golden.$n $INPUT_DIR/x264/x264.tstout.$n\n"
      fi
    done
  ;;
  *)
    usage
    error "Invalid benchmark \"$1\"!"
    ;;
esac