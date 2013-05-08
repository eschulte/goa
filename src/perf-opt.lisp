(mapcar #'require '(:software-evolution :memoize :cl-store))
(defpackage :perf-opt
  (:use :common-lisp :alexandria :metabang-bind :curry-compose-reader-macros
        :software-evolution :software-evolution-utility
        :split-sequence :memoize :cl-store :elf :cl-ppcre)
  (:shadow :type :magic-number))
(in-package :perf-opt)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))

(defclass asm-perf (asm)
  ((stats :initarg :stats :accessor stats :initform nil)))

(defclass cil-perf (cil)
  ((stats :initarg :stats :accessor stats :initform nil)))

(defvar infinity
  #+sbcl
  SB-EXT:DOUBLE-FLOAT-POSITIVE-INFINITY
  #-(or sbcl)
  (error "must specify a positive infinity value"))

(defvar *test-fmt* nil
  "Set to the string used to run the test shell script.")

(defun parse-stdout (stdout)
  (mapcar (lambda-bind ((val key))
            (cons (make-keyword (string-upcase key))
                  (or (ignore-errors (parse-number val))
                      infinity)))
          (mapcar {split-sequence #\,}
                  (split-sequence #\Newline
                                  (regex-replace-all ":HG" stdout "")
                                  :remove-empty-subseqs t))))

(defun test (asm)
  (with-temp-file (bin)
    (phenome asm :bin bin)
    (multiple-value-bind (stdout stderr errno) (shell *test-fmt* bin)
      (declare (ignorable stderr))
      (cons `(:exit . ,errno)
            (ignore-errors (parse-stdout stdout))))))
