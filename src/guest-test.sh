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
LIMIT="/home/bacon/bin/limit"
lock="/tmp/lockfile"
quit_locked(){ echo busy; exit 1; }
quit_free(){ rm -f $lock; exit 0; }

# atomic check and set on the VM lock
(umask 222; echo $$ >$lock) 2>/dev/null || quit_locked;

var=$1
if [ -z "$1" ];then
    echo "The first argument must be the path to an ASM FFT file."
else
    (
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
        rm -f output_files/s
        $LIMIT make fft_bench_test
    ) 2>/dev/null
fi
quit_free
