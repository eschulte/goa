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
REMOTES=("2222")
. $(dirname $0)/REMOTES # allow host-specific remote files
pick_remote(){ echo ${REMOTES[$RANDOM % ${#REMOTES[@]}]}; }

if [ -z "$1" ];then echo "requires an argument"; exit 1;fi
var=$1
guest_test="/home/bacon/bin/guest-test.sh"
cmd="$guest_test /tmp/$(basename $var)"
id="../data/id_rsa"
output_path="graphite/output_files/sim.out"

## run remotely and collect output and return value
output="busy"
while [ "$output" = "busy" ]; do
    remote=$(pick_remote)
    scp -i $id -P $remote $var bacon@localhost:/tmp/ >/dev/null
    output=$( ssh -t -i $id -p $remote bacon@localhost "$cmd" 2>/dev/null )
    success=$?
    if [ "$output" = "busy" ];then sleep 1; fi
done

## if successful collect the output file
output=$(scp -i $id -P $remote bacon@localhost:$output_path /dev/stdout)

## return the execution metrics as lisp
sed_cmd=$(cat <<EOF
s/^ \+//;
s/ \+| \+/ /g;
s/(//g;
s/)//g;
s/\([a-zA-Z]\) \([a-zA-Z]\)/\1-\2/g;
EOF
)
if [ $success -eq 0 ];then
    # collecting the "Network model 2" stats
    echo "$output" \
        |sed -n '/Network model 2/,/Network model 3/p' \
        |grep -v 'Network model'|grep -v 'Activity Counters' \
        |sed "$sed_cmd"
    exit 0
else
    exit 1
fi
