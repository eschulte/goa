;;; freqmine-extended.lisp --- correct bugs introduced by freqmine optimization

;; Copyright (C) 2013  Eric Schulte

;;; Commentary:

;; This file combines optimized versions of freqmine which don't pass
;; held-out tests with the original version of freqmine which passes
;; held-out tests but doesn't execute efficiently.  The goal is to
;; find a version of freqmine which is both widely correct and
;; efficient.
;;
;; There are two parts to this file.  First a fitness function is
;; defined which combines efficiency and held-out test cases and gives
;; equal fitness to the original program and the optimized variants.
;; Second a population is build of copies of both the original and the
;; optimized versions of the program.

;;; Code:
(in-package :optimize)
(defvar orig-energy 668.3218)
(defvar opt-energy 601.47565)
(defvar extended-test-penalty (/ (- orig-energy opt-energy) 6))

;; 1. redefine test to use the extended tests as well as the model.
(defun test (asm)
  (note 4 "extended testing ~S~%" (edits asm))
  (or (ignore-errors
        (let ((model (or (progn
                           (unless (stats asm) (setf (stats asm) (run asm)))
                           (note 4 "stats:~%~S~%" (stats asm))
                           (when (<= (aget :error (stats asm)) *max-err*)
                             (apply-model *model* (stats asm))))
                         infinity))
              (extended
               (multiple-value-bind (out err errno)
                   (shell "extended-tests.py ~a ~a -l \"limit\" -s 8"
                          *benchmark* (phenome asm))
                 (declare (ignorable err errno))
                 (* (count-if {scan "FAIL"} (split-sequence #\Newline out))
                    extended-test-penalty))))
          (+ model extended)))
      infinity))

;; 2. populate with a mix of the best individuals and the original
(let* ((base "/nfs/adaptive/data/opt/results/amd/freqmine/")
       (optimized
        (mapc (lambda (opt) (setf (fitness opt) (test opt)))
              (mapcar [#'restore {format nil "~a/best-~d.store" base}]
                      '(243200 222720 169728 155136 153600 152832 151552
                        151296 149504 148224)))))
  ;; 1/2 copies of the original
  (loop :for i :below (floor (/ *max-population-size* 2)) :do
     (push (copy *orig*) *population*))
  ;; 1/2 copies of optimizations
  (loop :until (>= (length *population*) *max-population-size*) :do
     (let ((opt (pop optimized)))
       (push (copy opt) *population*)
       (setf optimized (append optimized (list opt))))))
