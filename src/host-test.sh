#!/bin/bash
#
# Copyright (C) 2012  Eric Schulte
#
# Usage:
#   host-test.sh variant.s
#
# Commentary:
#   This does not follow the normal test script format but rather it;
#    1. takes the path to a .s asm file
#    2. copies that file to a VM
#    3. runs the resulting program in Graphite in the VM
#    4. returns the full set of Graphite debug information in a format
#       readable by the common lisp `read-from-string' function.
#
# Code:
REMOTES=("tune")
. $(dirname $0)/REMOTES # allow host-specific remote files
pick_remote(){ echo ${REMOTES[$RANDOM % ${#REMOTES[@]}]}; }

var=$1
guest_test="/home/bacon/bin/guest-test.sh"
cmd="$guest_test /tmp/$(basename $var)"
id="../data/id_rsa"

## run remotely and collect output and return value
output="busy"
while [ "$output" = "busy" ]; do
    remote=$(pick_remote)
    scp -i $id -P $remote $var bacon@localhost:/tmp/ >/dev/null
    output=$( ssh -t -i $id -p $remote bacon@localhost "$cmd" 2>/dev/null )
    success=$?
    if [ "$output" = "busy" ];then sleep 1; fi
done

## if successful return the execution metrics as lisp
sed_cmd=$(cat <<EOF
s/://;
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
if [ $success -eq 0 ];then
    ## parse and print the results
    echo "$output" \
        |sed -n '/FFT with Blocking Transpose/,$p' \
        |egrep " : +[.0-9]+$"|sed "$sed_cmd"
    exit 0
else
    exit 1
fi
