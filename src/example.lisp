;;; example.lisp --- Example file demonstrating program optimization

;; Copyright (C) 2013  Eric Schulte

;;; Commentary:

;; Starting with an initial software object (saved to `*orig*'),
;; generate a population of variant implementations and then evolve to
;; optimize some fitness function (defined in `test').

;; 1. Write a test script.  The script should print output the
;;    following format, where each line contains both a value before
;;    the command and a HW counter or metric after the comma.
;;
;;    0,error
;;    363.339573,task-clock
;;    6,context-switches
;;    5,cpu-migrations
;;    526,page-faults
;;    968046375,etc...
;;
;; 2. Parse the original individual from a text assembly file, and
;;    save to the `*orig*' variable.
;;
;; 3. Define `*test-fmt*' to run the test script
;;
;; 4. Define a fitness function to calculate a scalar fitness based on
;;    the metrics returned from the test script.
;;
;; 5. Evaluate the individual, define GP parameters, and build the
;;    population.
;;
;; 6. Run `evolve' to begin evolving variants of the original.  There
;;    are many optional arguments to this function, and this function
;;    may be run in many separate threads concurrently.

;;; Code:
(load "src/optimize.lisp")
(in-package :optimize)

;; (2)
(setf *script* "./bin/example-test ~a")

;; (3)
(setf *orig* (from-file (make-instance 'asm-perf) "path/to/variant.s"))

;; (4)
(defun test (asm)
  ;; if the variant has not yet been run, then run and save the
  ;; metrics to `stats'
  (unless (stats asm) (setf (stats asm) (test asm)))
  ;; fitness function combining HW counters
  (assert nil nil "Implement a fitness function HERE."))

;; (5)
(setf
 ;; set the fitness of the original individual
 (fitness *orig*) (multi-obj *orig*)
 ;; set the maximum population size
 *max-population-size* (expt 2 9)
 ;; specify that lower fitness values are better
 *fitness-predicate* #'<
 ;; fill the population with copies of the original
 *population* (loop :for n :below *max-population-size* :collect (copy *orig*)))

;; (6)
(evolve #'test)
