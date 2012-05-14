;;; runtime-2.lisp --- analysis of the runtime-2 run

;; Copyright (C) 2012  Eric Schulte

;;; Code:
(require :software-evolution)
(in-package :software-evolution)
(load "./analyze.lisp")

(setf *dir* "../results/runtime-2/")

(defvar *times* "Mean run-times.")

(defvar *raw-times* "Raw run-times.")

#+save-from-full-pops
(progn
  (setf *raw-times*
        (loop :for n :upto 611 :collect
           (let ((pop (restore (file-for-run n))))
             (dolist (var pop) (apply-output var (raw-output var)))
             (let ((pop (remove-if-not #'time-wo-init pop)))
               (mapcar #'time-wo-init pop)))))
  (store *raw-times* "../results/runtime-2.raw-times.store"))

#+grab-from-full-pops
(setf *raw-times* (restore "../results/runtime-2.raw-times.store"))

(setf *times*
      (mapcar
       (lambda (generation)
         (list (float (mean generation))
               (float (standard-deviation generation))))
       (delete nil *raw-times*)))
