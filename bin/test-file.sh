#!/bin/bash
#
# Usage: test.sh EXECUTABLE [TEST] [LIMIT]
#
# EXECUTABLE -- the executable for the sorter to test
#
# TEST -------- the specific test to run
#
#               default: run all tests and print the number of tests
#                        which passed.  If this is neutral, then exit
#                        with 0.
#
# LIMIT ------- optionally specify limit script
#
#               default: use the limit located next to this script
#
prog="$(dirname $1)/$(basename $1)"
if [ -z $3 ];then limit=$(dirname $0)"/limit";
             else limit="$3"; fi

run_helper(){
    diff <($limit $prog <(echo $1)) <(echo $2|tr ' ' '\n') >/dev/null 2>/dev/null; }

run(){
    case $1 in
        0) run_helper "1 4 56 2 43 8 76 12 43 7" "1 2 4 7 8 12 43 43 56 76";;
        1) run_helper "87686876 9879789 4756456 4534657 45354 9878 123 2 1" "1 2 123 9878 45354 4534657 4756456 9879789 87686876";;
        2) run_helper "94 38 42 30 19 90 42" "19 30 38 42 42 90 94";;
        3) run_helper "4844 8783" "4844 8783";;
        4) run_helper "1" "1";;
        5) run_helper "0" "0";;
        6) run_helper "7 6 4 9 7 9 2 9 6 3" "2 3 4 6 6 7 7 9 9 9";;
        7) run_helper "804 683 473 892 689 422 365 896 871 384 80 101 817 419 460 419 837 627 681 92 566 935 69 768 727 442 252 878 948 987 158 973 977 461 747 715 108 658 185 908 2 792 376 277 383 402 356 724 287 20 112 424 624 888 791 447 831 961 94 540 298 655 908 350 75 90 864 627 82 82 554 891 674 759 609 630 965 469 707 346 727 629 829 689 70 219 788 425 318 935 113 280 700 411" "2 20 69 70 75 80 82 82 90 92 94 101 108 112 113 158 185 219 252 277 280 287 298 318 346 350 356 365 376 383 384 402 411 419 419 422 424 425 442 447 460 461 469 473 540 554 566 609 624 627 627 629 630 655 658 674 681 683 689 689 700 707 715 724 727 727 747 759 768 788 791 792 804 817 829 831 837 864 871 878 888 891 892 896 908 908 935 935 948 961 965 973 977 987";;
        8) run_helper "50384 49152 94598 45069 34709 28075 41004 14862 8268 58155 72379 60325 63673 36390 11362 43942 78637 12491 40124 54376" "8268 11362 12491 14862 28075 34709 36390 40124 41004 43942 45069 49152 50384 54376 58155 60325 63673 72379 78637 94598";;
        9) run_helper "0 0 0 1 1 1 0 1 1 1" "0 0 0 0 1 1 1 1 1 1";;
    esac; }

if [ -z "$2" ];then
    count=0
    for i in $(seq 0 9);do
        if $(run $i);then count=$(expr $count + 1);fi
    done
    echo "$count"
    exit 0
else
    if $(run $2);then exit 0;fi
fi
exit 1
