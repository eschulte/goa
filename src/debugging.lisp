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
(defvar *perf* nil)

(let ((counter 0) (asm-counter 0))
  (sb-vm::map-allocated-objects
   (lambda (obj type size)
     (declare (ignorable type size))
     (typecase obj
       (asm-perf (incf asm-counter)
                 (when (= asm-counter 200) (setf *perf* obj)))
       ((SIMPLE-VECTOR 256))
       (cons (incf counter) (when (= counter 3000000) (setf *bad* obj)))
       (t)))
   :dynamic)
  asm-counter)
