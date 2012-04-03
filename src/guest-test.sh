#!/bin/bash
#
# Copyright (C) 2012  Eric Schulte
#
# Usage:
#   guest-test.sh /tmp/variant.s
#
# Commentary:
#   Takes an asm fft file, compiles it, runs it and returns results.
#
# Code:
var=$1
if [ -z "$1" ];then
    echo "The first argument must be the path to an ASM FFT file."
    exit 1
else
    # compile
    pushd /home/bacon/graphite/tests/benchmarks/fft
    cp $var ./
    rm -f fft fft.o
    gcc -I/home/bacon/graphite/common/user \
        -I/home/bacon/graphite/common/misc \
        -c -Wall -O2 -DTARGET_X86_64 -std=c99 \
        -c -o fft.o fft.s || exit 1
    popd
    # run
    pushd /home/bacon/graphite
    make fft_bench_test
fi
