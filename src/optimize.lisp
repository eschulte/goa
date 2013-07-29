;;; optimize.lisp --- optimize metrics in a population of software variants

;; Copyright (C) 2012  Eric Schulte

;;; Commentary:

;; Starting with an initial software object, generate a population of
;; variant implementations and then evolve to optimize some metric
;; such as fastest execution, least communication, lowest energy
;; consumption etc...

;;; Code:
(in-package :optimize)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))


;;; Optimization Software Objects
(defclass asm-perf (asm)
  ((stats :initarg :stats :accessor stats :initform nil)))

(defclass asm-light (light asm)
  ((stats :initarg :stats :accessor stats :initform nil)))

(defun to-asm-light (asm)
  (with-slots (flags linker genome) asm
    (make-instance 'asm-light
      :flags flags
      :linker linker
      :genome (mapcar {aget :line} genome))))

(defclass asm-range (range asm)
  ((stats :initarg :stats :accessor stats :initform nil)))

(defun to-asm-range (asm)
  (with-slots (flags linker genome) asm
    (make-instance 'asm-range
      :flags flags
      :linker linker)))

(defmethod copy ((asm asm-range))
  (with-slots (genome linker flags reference) asm
    (make-instance (type-of asm)
      :fitness (fitness asm)
      :genome (copy-tree genome)
      :linker linker
      :flags flags
      :reference reference)))


;;; Configuration Fitness and Runtime
(defvar *path*      nil        "Path to Assembly file.")
(defvar *script*   "./bin/run" "Script used to test benchmark application.")
(defvar *size*      nil        "size of input for fitness evaluation")
(defvar *res-dir*   nil        "Directory in which to save results.")
(defvar *orig*      nil        "Original version of the program to be run.")
(defvar *benchmark* nil        "Name of the benchmark.")
(defvar *period*    nil        "Period at which to run `checkpoint'.")
(defvar *threads*   1          "Number of cores to use.")
(defvar *evals*    (expt 2 18) "Maximum number of test evaluations.")
(defvar *max-err*   0          "Maximum allowed error.")
(defvar *model*     nil        "HW counter model to optimized.")
(setf *max-population-size* (expt 2 9)
      *fitness-predicate* #'<
      *cross-chance* 2/3
      *tournament-size* 2
      *tournament-eviction-size* 2)

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
                  (ignore-errors (parse-number val))))
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
    (note 4 "running ~S~%" asm)
    (multiple-value-bind (stdout stderr errno)
        (if (null *size*)
            (shell "~a ~a ~a -p"       *script* *benchmark* bin)
            (shell "~a ~a ~a -s ~a -p" *script* *benchmark* bin *size*))
      (declare (ignorable stderr) (ignorable errno))
      (ignore-errors (parse-stdout stdout)))))

(defun apply-model (model stats)
  "Apply MODEL to STATS."
  (flet ((key-to-sym (keyword)
           (if (keywordp keyword)
               (intern (string-upcase (symbol-name keyword)) :optimize)
               keyword)))
    (let ((*error-output* (make-broadcast-stream))
          (*standard-output* (make-broadcast-stream))
          (expr `(let ,(mapcar (lambda (pair)
                                 (list (key-to-sym (car pair)) (cdr pair)))
                               stats)
                   ,model)))
      (values (eval expr) expr))))

(defvar infinity
  #+sbcl
  SB-EXT:DOUBLE-FLOAT-POSITIVE-INFINITY
  #+ccl
  CCL::DOUBLE-FLOAT-POSITIVE-INFINITY
  #-(or sbcl ccl)
  (error "must specify a positive infinity value"))

(defun test (asm)
  (note 4 "testing ~S~%" asm)
  (or (ignore-errors
        (unless (stats asm) (setf (stats asm) (run asm)))
        (note 4 "stats:~%~S~%" (stats asm))
        (when (<= (aget :error (stats asm)) *max-err*)
          (apply-model *model* (stats asm))))
      infinity))

(defun checkpoint ()
  (note 1 "checkpoint after ~a fitness evaluations~%" *fitness-evals*)
  #+sbcl (sb-ext:gc :force t :full t)
  ;; save the best of the entire population
  (store (extremum *population* *fitness-predicate* :key #'fitness)
         (make-pathname :directory *res-dir*
                        :name (format nil "best-~a" *fitness-evals*)
                        :type "store"))
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


;;; Simple helpers
(defun throw-error (&rest args)
  (apply #'note 0 args)
  (quit))

(defmacro getopts (&rest forms)
  (let ((arg (gensym)))
    `(loop :for ,arg = (pop args) :while ,arg :do
        (cond
          ,@(mapcar (lambda-bind ((short long . body))
                      `((or (and ,short (string= ,arg ,short))
                            (and ,long  (string= ,arg ,long)))
                        ,@body))
                    forms)))))

(defun covariance (a b)
  (declare (optimize speed))
  (let ((ma (mean a))
        (mb (mean b))
        (total 0))
    (mapc (lambda (al bl) (incf total (* (- al ma) (- bl mb)))) a b)
    (/ total (- (length a) 1))))
