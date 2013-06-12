;;; optimize.lisp --- optimize metrics in a population of software variants

;; Copyright (C) 2012  Eric Schulte

;;; Commentary:

;; Starting with an initial software object, generate a population of
;; variant implementations and then evolve to optimize some metric
;; such as fastest execution, least communication, lowest energy
;; consumption etc...

;;; Code:
(mapcar (lambda (pkg)
          (handler-case (require pkg)
            (error (e)
              (declare (ignorable e))
              (format t "missing dependency on ~S~%" pkg)
              (format t "install with (ql:quickload ~S)~%" pkg)
              #+sbcl (sb-ext:exit :code 1)
              #+ccl (ccl:quit)
              #-(or ccl sbcl) (error "not implemented"))))
        '(:software-evolution :cl-store :split-sequence :cl-ppcre
          :bordeaux-threads))
(defpackage :optimize
  (:use :common-lisp :software-evolution :software-evolution-utility
        :alexandria :metabang-bind :curry-compose-reader-macros
        :cl-store :split-sequence :cl-ppcre :bordeaux-threads)
  (:shadow :type :magic-number))
(in-package :optimize)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))

(defun error-out ()
  #+sbcl (sb-ext:exit :code 1)
  #+ccl (format t "done~%")
  #-(or ccl sbcl) (error "not implemented"))

(defclass asm-perf (asm)
  ((stats :initarg :stats :accessor stats :initform nil)))

;;; Models
(defvar intel-sandybridge-energy-model
  '((5.007e-15 :cycles)
    (1.774e-16 :instructions)
    (2.787e-16 :r532010 :r538010)
    (2.374e-14 :cache-references)
    (1.464e-14 :cache-misses))
  "HW counters and coefficients for the Intel Sandybridge energy model.")

(defvar amd-opteron-energy-model
  '(( 4.411e-14 :cycles)
    ( 2.235e-15 :instructions)
    (-8.531e-16 :r533f00)
    (-1.256e-14 :cache-references)
    ( 3.679e-13 :cache-misses))
  "HW counters and coefficients in the AMD Opteron energy model.")

;;; Utility functions
(defvar infinity
  #+sbcl
  SB-EXT:DOUBLE-FLOAT-POSITIVE-INFINITY
  #+ccl
  CCL::DOUBLE-FLOAT-POSITIVE-INFINITY
  #-(or sbcl ccl)
  (error "must specify a positive infinity value"))

(defvar *path*   nil "Path to Assembly file.")
(defvar *script* "./bin/run" "Script used to test benchmark application.")
(defvar *size*   nil "size of input for fitness evaluation")
(defvar *res-dir* nil "Directory in which to save results.")
(defvar *orig*   nil "Original version of the program to be run.")
(defvar *benchmark* nil "Name of the benchmark.")
(defvar *period* nil "Period at which to run `checkpoint'.")
(defvar *threads*  1   "Number of cores to use.")
(defvar *evals* (expt 2 18) "Maximum number of test evaluations.")
(defvar *max-err* 0 "Maximum allowed error.")
(defvar *model* nil "HW counter model to optimized.")
(setf *max-population-size* (expt 2 9)) ;; Default max pop size
(setf *fitness-predicate* #'<)
(setf *tournament-size* 4)

(defun arch ()
  (let ((cpuinfo "/proc/cpuinfo"))
    (if (probe-file cpuinfo)
        (block nil
          (with-open-file (in cpuinfo)
            (loop :for line = (read-line in nil) :while line :do
               (cond ((scan "Intel" line) (return :intel))
                     ((scan "AMD" line) (return :amd))))))
        :darwin)))

(defun parse-stdout (stdout)
  (mapcar (lambda-bind ((val key))
            (cons (make-keyword (string-upcase key))
                  (or (ignore-errors (parse-number val)) infinity)))
          (mapcar {split-sequence #\,}
                  (split-sequence #\Newline
                                  (regex-replace-all ":HG" stdout "")
                                  :remove-empty-subseqs t))))

(defun run (asm)
  (with-temp-file (bin)
    (multiple-value-bind (info exit)
        (phenome asm :bin bin)
      (unless (zerop exit)
        (note 5 "ERROR [~a]: ~a" exit info)
        (error "error [~a]: ~a" exit info)))
    (note 4 "running ~S~%" (edits asm))
    (multiple-value-bind (stdout stderr errno)
        (if (null *size*)
          (shell "~a ~a ~a -p" *script* *benchmark* bin)
          (shell "~a ~a ~a -s ~a -p" *script* *benchmark* bin *size*))
      (declare (ignorable stderr))
      (cons `(:exit . ,errno) (ignore-errors (parse-stdout stdout))))))

(defun test (asm)
  (note 4 "testing ~S~%" (edits asm))
  (or (ignore-errors
        (unless (stats asm) (setf (stats asm) (run asm)))
        (note 4 "stats:~%~S~%" (stats asm))
        (when (<= (aget :error (stats asm)) *max-err*)
          (let ((stats (stats asm)))
            (reduce (lambda-bind (acc (cf . cntrs))
                      (+ acc (* cf (reduce #'+ (mapcar {aget _ stats} cntrs)))))
                    *model* :initial-value 0))))
      infinity))

(defun checkpoint ()
  (note 1 "checkpoint after ~a fitness evaluations~%" *fitness-evals*)
  #+sbcl (sb-ext:gc :force t :full t)
  ;; save the best of the entire population
  (store (extremum *population* *fitness-predicate* :key #'fitness)
         (make-pathname :directory *res-dir*
                        :name (format nil "best-~a" *fitness-evals*)
                        :type "store"))
  ;; write out and clean the list of saved edits
  (with-open-file
      (out (make-pathname :directory *res-dir* :name "edits" :type "lisp")
           :direction :output
           :if-exists :append
           :if-does-not-exist :create)
    (format out "~&~{~s~^~%~}~%" (prog1 *consolidated-edits*
                                   (setf *consolidated-edits* nil))))
  ;; write out population stats
  (let ((fits  (mapcar #'fitness *population*))
        (sizes (mapcar [#'length #'genome] *population*))
        (stats (make-pathname :directory *res-dir* :name "stats" :type "txt")))
    (flet ((stats (s) (list (apply #'min s) (median s) (apply #'max s))))
      (with-open-file (out stats
                           :direction :output
                           :if-exists :append
                           :if-does-not-exist :create)
        (format out "~&~{~a~^ ~}~%"
                (mapcar (lambda (num) (if (= num infinity) "inf" num))
                        (mapcar #'float
                                `(,*fitness-evals*
                                  ,@(stats fits)
                                  ,@(stats sizes)))))))))


;;; Simple command line helpers
(defun throw-error (&rest args)
  (apply #'note 0 args)
  (error-out))

(defmacro getopts (&rest forms)
  (let ((arg (gensym)))
    `(loop :for ,arg = (pop args) :while ,arg :do
        (cond
          ,@(mapcar (lambda-bind ((short long . body))
                      `((or (string= ,arg ,short) (string= ,arg ,long)) ,@body))
                    forms)))))
