;;; packet-min.lisp --- analysis of the pll-2 run

;; Copyright (C) 2012  Eric Schulte

;;; Code:
(require :software-evolution)
(in-package :software-evolution)
(load "./analyze.lisp")

(setf *dir* "../results/packets-min/")

(defvar *packets* "Mean packets transmitted.")

(setf *packets*
      (loop :for n :upto 78 :collect
         (let ((pop (restore (file-for-run n))))
           (dolist (var pop) (apply-output var (raw-output var)))
           (let ((pop (remove-if-not #'total-packets-sent pop)))
             (if (null pop)
                 nil
                 (float (mean (mapcar #'time-wo-init pop))))))))
