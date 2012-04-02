#!/bin/sh
#
# Usage:
#   test.sh variant.s
#
# Note:
#   This does not follow the normal test script format but rather it;
#    1. takes the path to a .s asm file
#    2. copies that file to a VM
#    3. runs the resulting program in Graphite in the VM
#    4. returns the full set of Graphite debug information in a format
#       readable by the common lisp `read-from-string' function.

## read in the file to test
var=$1

## TODO: find a free remote machine
# remote="foo"

## TODO: run remotely and collect output and return value
# scp $var $remote:/tmp/
# output=$(ssh $remote evaluate $var)
# success=$?

## TODO: if sucessfull return the execution metrics as lisp
sed_cmd=$(cat <<EOF
s/:/./;
s/^/(/;
s/$/)/;
s/ \+/ /g;
s/Start time/start/;
s/Initialization finish time/init-finish/;
s/Overall finish time/finish/;
s/Total time with initialization/time-w-init/;
s/Total time without initialization/time-wo-init/;
s/Overall transpose time/trans-time/;
s/Overall transpose fraction/trans-fraction/;
EOF
)
# if [ $success -eq 0 ];then
#     ## parse the results into something readable from lisp
#     clean=$(echo "$output"|sed -n '/FFT with Blocking Transpose/,$p'|grep " : "|sed "$sed_cmd")
#     echo="'($clean)"
#     exit 0;
# else
#     exit 1;
# fi
