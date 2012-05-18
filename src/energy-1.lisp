;;; runtime-2.lisp --- analysis of the runtime-2 run

;; Copyright (C) 2012  Eric Schulte

;;; Code:
(require :software-evolution)
(in-package :software-evolution)
(load "./analyze.lisp")

(setf *dir* "../results/energy-1/")

(defvar *powers* "Mean power consumption stats.")

(defvar *raw-powers* "Raw power consumption stats.")

#+save-from-full-pops
(progn
  (setf *raw-powers*
        (loop :for n :upto 100 :collect
           (let ((pop (restore (file-for-run n))))
             (dolist (var pop) (apply-output var (raw-output var)))
             (let ((pop (remove-if-not #'static-power pop)))
               (mapcar #'static-power pop)))))
  (store *raw-powers* "../results/energy-1.raw-powers.store"))

#+grab-from-full-pops
(setf *raw-times* (restore "../results/runtime-2.raw-times.store"))

(setf *powers*
      (mapcar
       (lambda (generation)
         (let ((real (mapcar #'mean generation)))
           (list (float (mean real))
                 (float (standard-deviation real)))))
       (delete nil *raw-powers*)))
