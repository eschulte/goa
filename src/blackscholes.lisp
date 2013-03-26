(mapcar #'require '(:software-evolution :memoize :cl-store))
(defpackage :bs-opt
  (:use :common-lisp :alexandria :metabang-bind :curry-compose-reader-macros
        :software-evolution :software-evolution-utility
        :split-sequence :memoize :cl-store :elf :cl-ppcre)
  (:shadow :type :magic-number))
(in-package :bs-opt)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))

(defvar infinity
  #+sbcl
  SB-EXT:DOUBLE-FLOAT-POSITIVE-INFINITY
  #-(or sbcl)
  (error "must specify a positive infinity value"))

(defvar *test-fmt* "./bin/bs-test ~a -n 1 -t 12000 -r -p"
  "Script used to evaluate variants.
Take the path to a blackscholes executable, and returns the difference
between it's output and the oracle output.")

(defvar *orig* (from-file (make-instance 'asm :linker "g++")
                          "data/blackscholes/asms/bs-g++-O0.s"))

(defvar *max-err* (/ 83096.7 (expt 10 3)) ;; 83096.7 is sum of all outputs
  "Maximum error allowed, 3 orders of magnitude below total output.")

(defun parse-stdout (stdout)
  (mapcar (lambda-bind ((val key))
            (cons (intern (string-upcase key))
                  (or (ignore-errors (parse-number val))
                      infinity)))
          (mapcar {split-sequence #\,}
                  (cdr (split-sequence #\Newline
                                       (regex-replace-all ":HG" stdout "")
                                       :remove-empty-subseqs t)))))

(defun test (asm)
  (software-evolution::with-temp-file (bin)
    (phenome asm :bin bin)
    (multiple-value-bind (stdout stderr errno) (shell *test-fmt* bin)
      (declare (ignorable stderr))
      (if (zerop errno)
          (parse-stdout stdout)
          `((error . ,infinity))))))

(defun neutralp (asm)
  (when-let ((err (cdr (assoc 'error (fitness asm)))))
    (and (numberp err)
         (< err *max-err*))))

(defvar *neutral* nil)

(loop :for step :from 3 :to 10 :do
   (let ((prev (copy-tree *neutral*)))
     (setf *neutral* nil)
     (loop :until (>= (length *neutral*) 100) :as i :from 0 :do
        (let ((new (copy (random-elt prev))))
          (mutate new)
          (setf (fitness new) (test new))
          (format t "~S/~S edits:~S error:~S~%"
                  i (length *neutral*) (edits new) (aget 'error (fitness new)))
          (when (neutralp new) (push new *neutral*))))
     (store *neutral* (format nil "results/bs-neut/~d.store" step))))
