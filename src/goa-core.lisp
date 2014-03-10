;;; goa-core.lisp --- optimize metrics in a population of software variants

;; Copyright (C) 2012-2013  Eric Schulte

;;; Commentary:

;; Starting with an initial software object, generate a population of
;; variant implementations and then evolve to optimize some metric
;; such as fastest execution, least communication, lowest energy
;; consumption etc...

;;; Code:
(in-package :goa)
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
      :genome (lines genome))))

(defclass asm-range (sw-range asm)
  ((stats :initarg :stats :accessor stats :initform nil)))

(defun to-asm-range (asm)
  (with-slots (flags linker genome) asm
    (make-instance 'asm-range
      :flags flags
      :linker linker
      :genome (list (cons 0 (1- (length genome))))
      :reference (coerce (lines asm) 'vector))))

(defmethod from-file ((asm asm-range) file)
  (setf (lines asm) (split-sequence #\Newline (file-to-string file)))
  asm)

(defmethod copy ((asm asm-range))
  (with-slots (genome linker flags reference) asm
    (make-instance (type-of asm)
      :fitness (fitness asm)
      :genome (copy-tree genome)
      :linker linker
      :flags flags
      :reference reference)))


;;; Configuration Fitness and Runtime
(defvar *script*    nil        "Script used to test benchmark application.")
(defvar *path*      nil        "Path to Assembly file.")
(defvar *rep*       nil        "Program representation to use.")
(defvar *mcmc*      nil        "Whether MCMC search should be used.")
(defvar *res-dir*   nil        "Directory in which to save results.")
(defvar *orig*      nil        "Original version of the program to be run.")
(defvar *period*    nil        "Period at which to run `checkpoint'.")
(defvar *threads*   1          "Number of cores to use.")
(defvar *evals*    (expt 2 18) "Maximum number of test evaluations.")
(defvar *max-err*   0          "Maximum allowed error.")
(defvar *fitness-function* 'fitness "Fitness function.")
(setf *max-population-size* (expt 2 9)
      *fitness-predicate* #'<
      *cross-chance* 2/3
      *tournament-size* 2
      *tournament-eviction-size* 2)
(defvar *git-version* nil "Used in optimize version string.")

(defun arch ()
  (let ((cpuinfo "/proc/cpuinfo"))
    (if (probe-file cpuinfo)
        (with-open-file (in cpuinfo)
          (loop :for line = (read-line in nil) :while line :do
             (cond ((scan "Intel" line) (return-from arch :intel))
                   ((scan "AMD" line)   (return-from arch :amd)))))
        :darwin)))

(defun parse-stdout (stdout)
  (mapcar (lambda-bind ((val key . rest))
            (declare (ignorable rest))  ; don't care about extra fields
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
    (multiple-value-bind (stdout stderr errno) (shell *script* bin)
      (declare (ignorable stderr))
      (remove-duplicates
       (append (or (ignore-errors (list (cons :fitness (parse-number stdout))))
                   (ignore-errors (parse-stdout stdout)))
               (list (cons :exit errno) (cons :error 0)))
       :key #'car :from-end t))))

(defun apply-fitness-function (fitness-function stats)
  "Apply FITNESS-FUNCTION to STATS."
  (flet ((key-to-sym (keyword)
           (if (keywordp keyword)
               (intern (string-upcase (symbol-name keyword)) :goa)
               keyword)))
    (let ((*error-output* (make-broadcast-stream))
          (*standard-output* (make-broadcast-stream))
          (expr `(let ,(mapcar (lambda (pair)
                                 (list (key-to-sym (car pair)) (cdr pair)))
                               stats)
                   ,fitness-function)))
      (values (eval expr) expr))))

(defvar infinity
  #+sbcl
  SB-EXT:DOUBLE-FLOAT-POSITIVE-INFINITY
  #+ccl
  CCL::DOUBLE-FLOAT-POSITIVE-INFINITY
  #-(or sbcl ccl)
  (error "must specify a positive infinity value"))

(declaim (inline worst))
(defun worst ()
  (cond ((equal #'< *fitness-predicate*) infinity)
        ((equal #'> *fitness-predicate*) 0)))

(defun test (asm)
  (note 4 "testing ~S~%" asm)
  (or (ignore-errors
        (unless (stats asm) (setf (stats asm) (run asm)))
        (note 4 "stats:~%~S~%" (stats asm))
        (when (and (zerop (aget :exit (stats asm)))
                   (<= (aget :error (stats asm)) *max-err*))
          (apply-fitness-function *fitness-function* (stats asm))))
      (worst)))

(defun checkpoint ()
  (note 1 "checkpoint after ~a fitness evaluations" *fitness-evals*)
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


;;; Helpers
(defun quit (&optional (errno 0))
  #+sbcl (sb-ext:exit :code errno)
  #+ccl  (ccl:quit errno))

(defun better (orig new)
  "Return the fraction improvement of new over original."
  (/ (abs (- orig new)) orig))

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


;;; Command line optimization driver
(defvar *checkpoint-funcs* (list #'checkpoint)
  "Functions to record checkpoints.")

(defun store-final-population ()
  #+sbcl (sb-ext:gc :force t)
  (store *population* (make-pathname :directory *res-dir*
                                     :name "final-pop"
                                     :type "store")))

(defun store-final-best ()
  (store (extremum *population* *fitness-predicate* :key #'fitness)
         (make-pathname :directory *res-dir*
                        :name "final-best"
                        :type "store")))

(defvar *final-funcs*
  (list #'store-final-population #'store-final-best)
  "Functions to run at the end of optimization.")


;;; Genome Annotations
(defun asm-disassemble (bin func)
  (ignore-errors ;; TODO: debug parse-integer errors thrown within
    (let ((raw (shell "gdb --batch --eval-command=\"disassemble ~a\" ~a"
                      func bin))
          (rx "[ \t]*0x([a-zA-Z0-9]+)[ \t]*<\\+[0-9]+>:.*"))
      (remove nil
        (mapcar (lambda (line)
                  (multiple-value-bind (all matches) (scan-to-strings rx line)
                    (when all
                      (read-from-string (format nil "#x~a" (aref matches 0))))))
                (split-sequence #\Newline raw))))))

(defun perf-annotations (script)
  ;; Note: another option could use
  ;;       perf report --stdio -i perf.data --sort srcline
  (remove nil
    (mapcar (lambda (line)
              (multiple-value-bind (all matches)
                  (scan-to-strings "([0-9\.]+) +:[ \\t]+([a-fA-F0-9]+):" line)
                (when all
                  (cons (read-from-string (format nil "#x~a"
                                                  (aref matches 1)))
                        (parse-number (aref matches 0))))))
            (split-sequence #\Newline (shell script)))))

(defun genome-addrs (asm &key bin &aux func-addrs)
  (let ((my-bin (or bin (phenome asm))))
    (unwind-protect
         (mapcar
          (lambda (l)
            (multiple-value-bind (all matches)
                (scan-to-strings "^([^\\.][a-zA-Z0-9_]*):" (aget :line l))
              (if all
                  (prog1 nil
                    (setf func-addrs (asm-disassemble my-bin (aref matches 0))))
                  (when func-addrs (pop func-addrs)))))
          (genome asm))
      (when (not bin) (delete-file my-bin)))))

(defun genome-anns (asm annotations &key bin)
  (let ((my-bin (or bin (phenome asm))))
    (unwind-protect
         (mapcar {aget _ annotations}
                 (genome-addrs asm :bin my-bin))
      (when (not bin) (delete-file my-bin)))))

(defun smooth (list)
  (declare (optimize speed))
  (mapcar (lambda (b3 b2 b1 o a1 a2 a3)
            (+ (* 0.006 (+ b3 a3))
               (* 0.061 (+ b2 a2))
               (* 0.242 (+ b1 a1))
               (* 0.383 o)))
          (append         (cdddr list) '(0 0 0))
          (append         (cddr  list) '(0 0))
          (append         (cdr   list) '(0))
          (append          list)
          (append '(0)     list)
          (append '(0 0)   list)
          (append '(0 0 0) list)))

(defun widen (list radius)
  "Widen the maximum values in LIST by RADIUS."
  (apply #'mapcar #'max (loop :for i :from (* -1 radius) :to radius :collect
                           (if (< i 0)
                               (append (drop (* i -1) list)
                                       (make-list (* i -1) :initial-element 0))
                               (append (make-list i :initial-element 0)
                                       (butlast list i))))))

(defun apply-annotations (asm annotations &key smooth widen flat bin loc)
  "Apply annotations to the genome of ASM.
Keyword argument SMOOTH will `smooth' the annotations with a Gaussian
blur.  Keyword argument WIDEN will `widen' the annotations.  If both
SMOOTH and WIDEN are given, widening is applied first.  Keyword
argument FLAT will produce flat annotations simply indicating if the
instruction was executed or not. LOCS indicates that annotations are
organized by line of code in the ASM file, and are not addresses."
  (when loc
    (assert (= (length annotations) (length (genome asm))) (annotations)
            "Annotations length ~d != genome length ~d"
            (length annotations) (length (genome asm))))
  (when widen
    (assert (numberp widen) (widen)
            "widen arg to `apply-annotations' isn't numeric: ~a"
            widen))
  (setf (genome asm)
        (mapcar
         (lambda (ann element)
           (cons (cons :annotation ann) element))
         ((lambda (raw)
            ((lambda (raw) (if smooth (smooth raw) raw))
             (if widen
                 (widen raw widen)
                 raw)))
          (mapcar (if flat
                      (lambda (ann) (if ann 1 0))
                      (lambda (ann) (or ann 0)))
                  (if loc
                      annotations
                      (genome-anns asm annotations :bin bin))))
         (genome asm))))


;;; Annotated lighter weight range representation
(defclass ann-range (asm-range)
  ((anns    :initarg :anns    :accessor anns    :initform nil)
   (ann-ref :initarg :ann-ref :accessor ann-ref :initform nil)))

(defun to-ann-range (asm)
  (with-slots (flags linker genome) asm
    (make-instance 'ann-range
      :flags flags
      :linker linker
      :genome (list (cons 0 (1- (length genome))))
      :reference (coerce (lines asm) 'vector)
      :anns (list (cons 0 (1- (length genome))))
      :ann-ref (coerce (mapcar {aget :annotation} genome) 'vector))))

(defmethod annotations ((asm asm)) (mapcar {aget :annotation} (genome asm)))

(defmethod annotations ((ann ann-range))
  (mapcan (lambda-bind ((start . end))
            (mapcar {aref (ann-ref ann)}
                    (loop :for i :from start :to end :collect i)))
          (anns ann)))

(defmethod copy ((asm ann-range))
  (with-slots (genome linker flags reference anns ann-ref) asm
    (make-instance (type-of asm)
      :fitness (fitness asm)
      :genome (copy-tree genome)
      :linker linker
      :flags flags
      :reference reference
      :anns anns
      :ann-ref ann-ref)))
