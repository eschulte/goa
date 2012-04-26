;;; pll-2.lisp --- analysis of the pll-2 run

;; Copyright (C) 2012  Eric Schulte

;;; Code:
(require :software-evolution)
(in-package :software-evolution)
(load "./analyze.lisp")

(setf *dir* "../results/pll-2/")

(defvar *times* nil "Mean running times")

(setf *times*
      (loop :for n :upto 300 :collect
         (let ((pop (restore (file-for-run n))))
           (dolist (var pop) (apply-output var (raw-output var)))
           (let ((pop (remove-if-not #'time-wo-init pop)))
             (if (null pop)
                 nil
                 (float (mean (mapcar #'time-wo-init pop))))))))
