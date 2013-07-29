;;; breed-back.lisp --- breed back with the original to correct optimize bugs

;; Copyright (C) 2013  Eric Schulte

;;; Commentary:

;; This file combines optimized versions of the benchmark with the
;; original in a mixed population.  This population is then evolved
;; against a mixture of held-out tests and the original optimization
;; workload.  The goal is to find a version which is both widely
;; correct and efficient.

;;; Code:
(in-package :optimize)
(defvar orig-energy 668.3218)
(defvar opt-energy 601.47565)
(defvar extended-test-penalty (/ (- orig-energy opt-energy) 2))


;;; Data
(defvar fast-extra-tests
  '((x264 (:extended 4 5 6 8 9 10) (:additional 0 1 2)))
  "Extra tests with runtimes under 5 seconds.")


;;; Running

;; 1. the fitness function should include extended tests
(assert *fitness-function* (*fitness-function*)
        "Fitness function should be set and should incorporate extended tests.")

;; 2. populate with a mix of the best individuals and the original
(progn
  ;; using the range representation
  (setf
   *rep* (coerce (mapcar {aget :line} (genome *orig*)) 'vector)
   *orig* (to-asm-range *orig*)
   (reference *orig*) (copy *rep*)
   *rep* 'asm)

  ;; 1/2 copies of the original
  (setf (fitness *orig*) (test *orig*))
  (loop :for i :below (floor (/ *max-population-size* 2)) :do
     (push (copy *orig*) *population*))

  ;; 1/2 copies of optimizations
  (let* ((base #| base of the optimized individuals |#)
         (optimized
          (mapc (lambda (opt) (setf (fitness opt) (test opt)))
                (mapcar [#'restore {format nil "~a/best-~d.store" base}]
                        '(#| list of optimized individuals |#)))))
    (loop :until (>= (length *population*) *max-population-size*) :do
       (let ((opt (pop optimized)))
         (push (copy opt) *population*)
         (setf optimized (append optimized (list opt)))))))
