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
debug(){ echo $1; }

# atomic check and set on the VM lock
(umask 222; echo $$ >$lock) 2>/dev/null || quit_locked;

var=$1
if [ -z "$1" ];then
    echo "The first argument must be the path to an ASM FFT file."
else
    (
        debug "compiling"
        pushd /home/bacon/graphite/tests/benchmarks/fft
        rm -f fft*
        mv $var ./fft.s
        gcc -I/home/bacon/graphite/common/user \
            -I/home/bacon/graphite/common/misc \
            -c -Wall -O2 -DTARGET_X86_64 -std=c99 \
            -c -o fft.o fft.s || exit 1
        g++ fft.o -o fft -static \
            -u CarbonStartSim \
            -u CarbonStopSim \
            -u pthread_create \
            -u pthread_join \
            -L/home/bacon/graphite/lib -los-services \
            -L/home/bacon/graphite/os-services-25032-gcc.4.0.0-linux-ia32_intel64/intel64 \
            -L/home/bacon/graphite/contrib/orion \
            -L/home/bacon/graphite/lib -pthread \
            -lcarbon_sim -los-services \
            -lboost_filesystem-mt -lboost_system-mt -pthread -lorion || exit 1
        rm -f ~/graphite/output_files/sim.out
        popd
        pushd ~/graphite/
        debug "running"
        GRAPHITE_HOME="/home/bacon/graphite" \
            $LIMIT ./tools/spawn.py 1 carbon_sim.cfg \
            /home/bacon/pin/intel64/bin/pinbin \
            -mt -t ./lib/pin_sim \
            -c carbon_sim.cfg \
            --general/total_cores=64 \
            --general/num_processes=1 \
            --general/enable_shared_mem=true \
            -- \
            ./tests/benchmarks/fft/fft -p64 -m16
        popd
    )# 2>/dev/null
fi
quit_free
