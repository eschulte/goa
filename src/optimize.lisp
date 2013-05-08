;;; optimize.lisp --- optimize metrics in a population of software variants

;; Copyright (C) 2012  Eric Schulte

;;; Commentary:

;; Starting with an initial software object, generate a population of
;; variant implementations and then evolve to optimize some metric
;; such as fastest execution, least communication, lowest energy
;; consumption etc...

;;; Code:
(mapcar #'require '(:software-evolution :cl-store :split-sequence :cl-ppcre))
(defpackage :optimize
  (:use :common-lisp :software-evolution :software-evolution-utility
        :alexandria :metabang-bind :curry-compose-reader-macros)
  (:shadow :type :magic-number))
(in-package :optimize)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))

(defclass asm-perf (asm)
  ((stats :initarg :stats :accessor stats :initform nil)))

;;; Models -- for now just in this file, could easily be read from the
;;;           command line
(defvar intel-energy-model
  ;; TODO: update with actual coefficients
  '((:instructions     . 1.0)
    (:r533f00          . 1.0)
    (:cache-references . 1.0)
    (:cache-misses     . 1.0))
  "HW counters and coefficients in the intel energy model.")

;;; Utility functions
(defvar infinity
  #+sbcl
  SB-EXT:DOUBLE-FLOAT-POSITIVE-INFINITY
  #-(or sbcl)
  (error "must specify a positive infinity value"))

(defvar *path*   nil "Path to Assembly file.")
(defvar *script* nil "Path to test script.")
(defvar *orig*   nil "Original version of the program to be run.")
(defvar *test-fmt* nil "Set to the string used to run the test shell script.")
(defvar *function* nil "Fitness function.")
(defvar *threads*  4   "Number of cores to use.")
(defvar *evals* (expt 2 20) "Maximum number of test evaluations.")
(defvar *max-err* 0 "Maximum allowed error.")
(defvar *model* nil "HW counter model to optimized.")
(setf   *work-dir* "sh-runner/work/")

(defun parse-stdout (stdout)
  (mapcar (lambda-bind ((val key))
            (cons (make-keyword (string-upcase key))
                  (or (ignore-errors (parse-number val))
                      infinity)))
          (mapcar {split-sequence #\,}
                  (split-sequence #\Newline
                                  (regex-replace-all ":HG" stdout "")
                                  :remove-empty-subseqs t))))

(defun run (asm)
  (with-temp-file (bin)
    (phenome asm :bin bin)
    (multiple-value-bind (stdout stderr errno) (shell *test-fmt* bin)
      (declare (ignorable stderr))
      (cons `(:exit . ,errno)
            (ignore-errors (parse-stdout stdout))))))

(defun test (asm)
  (unless (stats asm) (setf (stats asm) (run asm)))
  (or (ignore-errors
        (when (<= (aget :error (stats asm)) *max-err*)
          (let ((stats (stats asm)))
            (reduce (lambda-bind ((acc (hw . cf))) (+ acc (* cf (aget hw stats))))
                    *model* :initial-value 0))))
      infinity))
