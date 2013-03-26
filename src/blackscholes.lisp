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

(defvar *max-err* (/ 83096.7 (expt 10 6))
  "Maximum error allowed, 1 millionth of the output.")

(defun parse-stdout (stdout)
  (mapcar (lambda-bind ((val key))
            (cons (intern (string-upcase key))
                  (ignore-errors (parse-number val))))
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

(loop :until (> (length *neutral*) 99) :do
   (let ((new (copy *orig*)))
     (mutate new)
     (setf (fitness new) (test new))
     (when (neutralp new) (push new *neutral*))))
