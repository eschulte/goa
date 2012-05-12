#!/bin/sh
restart(){
BASE="/nfs/adaptive/eschulte/research/epr/optimization";
num=$(ls -1tr $BASE/results/runtime-2/|tail -1|sed 's/biased-pop-\(.*\)\.store/\1/')
cat <<EOF > re-run.lisp
(require :software-evolution)
(in-package :software-evolution)
(load "optimize.lisp")
(setf *dir* "../results/runtime-2/")
(setf *note-level* 1)
(setf *tsize* 4)
(advise-thread-pool-size 46)
(let ((last $num))
  (setf *pop* (restore (file-for-run last)))
  (loop for n from last to 10000 do (note 1 "saving population ~d" n)
       (store *pop* (file-for-run n))
       (note 1 "generating population ~d" (1+ n))
       (setf *pop* (biased-step *pop*))))
EOF
sbcl --load re-run.lisp &
sleep 3000;
}

restart;
while true;do
    if [ $(printf "%.0f" $(uptime|awk '{print $12}')) -lt 5 ];then
        killall -s 9 sbcl;
        sleep 300;
        restart;
    fi
    sleep 300;
done
