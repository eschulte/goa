#!/bin/sh
#
# takes an asm file and runs it
#
if [ -z "$1" ];then
    echo "one argument required"; exit 1;
else
    # compile
    pushd /home/bacon/graphite/tests/benchmarks/fft
    cp $1 ./
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
