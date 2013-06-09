;;; debug.lisp --- functions for interactive debugging

;; Copyright (C) 2013  Eric Schulte

;;; Code:
(in-package :optimize)
(require :hu.dwim.debug)

(defun setup ()
  (setf *orig* (from-file (make-instance 'asm-perf)
                          "benchmarks/bzip2/bzip2.s"))
  (setf *benchmark* "bzip2")
  (run *orig*))


;;; Memory analysis
(defvar *bad* nil)

(let ((vector-cnt 0)
      (cons-cnt 0)
      (other-cnt 0))
  (sb-vm::map-allocated-objects
   (lambda (obj type size)
     (declare (ignorable type size))
     (typecase obj
       (asm-perf (format t "main object"))
       ((SIMPLE-VECTOR 256)
        (incf vector-cnt)
        (when (= vector-cnt 10)
          (setf *bad* obj)))
       (cons (incf cons-cnt))
       (t (incf other-cnt))))
   :dynamic)
  (list vector-cnt cons-cnt other-cnt))
