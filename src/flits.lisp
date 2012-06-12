;;; flits.lisp --- analysis of the flits run

;; Copyright (C) 2012  Eric Schulte

;;; Code:
(require :software-evolution)
(in-package :software-evolution)
(load "./analyze.lisp")

(setf *dir* "../results/flits/")

(defvar *flits* "Mean run-flits.")

(defvar *raw-flits* "Raw run-flits.")

#+save-from-full-pops
(progn
  (setf *raw-flits*
        (loop :for n :upto 5 :collect
           (let ((pop (restore (file-for-run n))))
             (dolist (var pop) (apply-output var (raw-output var)))
             (let ((pop (remove-if-not #'total-flits-sent pop)))
               (mapcar #'mean (mapcar #'total-flits-sent pop))))))
  (store *raw-flits* "../results/flits.raw-flits.store"))

#+grab-from-full-pops
(setf *raw-flits* (restore "../results/flits.raw-flits.store"))

#+analysis
(setf *flits*
      (mapcar
       (lambda (generation)
         (list (float (mean generation))
               (float (standard-deviation generation))))
       (delete nil *raw-flits*)))
