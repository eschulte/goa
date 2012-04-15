#!/bin/bash
#
# Copyright (C) 2012  Eric Schulte
#
# Usage:
#   run-all.sh command args...
#
# Code:
REMOTES=("2222")
. $(dirname $0)/REMOTES # allow host-specific remote files
for remote in ${REMOTES[@]};do
    ssh -i data/id_rsa -p $remote bacon@guest "$@"
done
