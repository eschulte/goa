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
  (declare (cl-user::optimize speed))
  (let ((ma (mean a))
        (mb (mean b))
        (total 0))
    (mapc (lambda (al bl) (incf total (* (- al ma) (- bl mb)))) a b)
    (/ total (- (length a) 1))))


;;; Command line optimization driver
(defvar *help* "Usage: opt TEST-SCRIPT ASM-FILE [OPTIONS...]
 Optimize the assembly code in ASM-FILE against TEST-SCRIPT.

TEST-SCRIPT takes an executable, returns a numeric fitness.
   ASM-FILE is a text file of assembler code.

Options:
 -c,--cross-chance NUM - crossover chance (default 2/3)
 -C,--config FILE ------ read configuration from FILE
 -e,--eval SEXP -------- evaluate S-expression SEXP
 -E,--max-error NUM ---- maximum allowed error (default 0)
 -f,--fit-evals NUM ---- max number of fitness evaluations
                         default: 2^18
 -F,--fit-func FLAGS --- fitness function
                         default: output of TEST-SCRIPT
 --fit-pred PRED ------- fitness predicate (#'< or #'>)
                         default: #'<, minimize fit-func
 -g,--gc-size ---------- ~a
                         default: ~:d
 -l,--linker LINKER ---- linker to use
 -L,--lflags FLAGS ----- flags to use when linking
 -m,--mut-rate NUM ----- mutation rate (default 1)
 -M,--mcmc ------------- run MCMC search instead of GP
 -p,--pop-size NUM ----- population size
                         default: 2^9
 -P,--period NUM ------- period (in evals) of checkpoints
                         default: max-evals/(2^10)
 -r,--res-dir DIR ------ save results to dir
                         default: program.opt/
 -R,--rep REP ---------- use REP program representation
                         asm, light, or range (default)
 -s,--evict-size NUM --- eviction tournament size
                         default: 2
 -t,--threads NUM ------ number of threads
 -T,--tourny-size NUM -- selection tournament size
                         default: 1 (i.e., random selection)
 -v,--verbose NUM ------ verbosity level 0-4
 -V,--version ---------- print version and exit
 -w,--work-dir DIR ----- use an sh-runner/work directory~%")

(defvar *version*
  (format nil
          #+ccl "optimize version ~a using Clozure Common Lisp (CCL)~%"
          #+sbcl "optimize version ~a using Steel Bank Common Lisp (SBCL)~%"
          (eval-when (:compile-toplevel :load-toplevel :execute)
            (let ((raw (shell "git describe --always")))
              (subseq raw 0 (1- (length raw)))))))

(defvar *checkpoint-funcs* (list #'checkpoint)
  "Functions to record checkpoints.")

(defvar *mcmc* nil "Use MCMC search instead of GP.")

(defvar *rep* 'range
  "The type of program representation to use during optimization.")

(defun do-evolve ()
  (evolve #'test :max-evals *evals*
          :period *period*
          :period-fn (lambda () (mapc #'funcall *checkpoint-funcs*))))

(defun optimize (&optional (args *arguments*))
  (in-package :optimize)
  (setf *note-level* 1)
  (flet ((arg-pop () (pop args)))
    ;; Set default GC threshold
    #+ccl (ccl:set-lisp-heap-gc-threshold (expt 2 30))
    #+sbcl (setf (sb-ext:bytes-consed-between-gcs) (expt 2 24))

    ;; check command line arguments
    (when (or (<= (length args) 2)
              (string= (subseq (car args) 0 2) "-h")
              (string= (subseq (car args) 0 3) "--h")
              (string= (car args) "-V")
              (string= (car args) "--version"))
      (if (or (string= (car args) "-V")
              (string= (car args) "--version"))
          (progn (format t *version*) (quit))
          (format t *help*
                  #+ccl "space left after a full GC pass"
                  #+sbcl "bytes consed between every GC pass"
                  #+ccl (ccl:lisp-heap-gc-threshold)
                  #+sbcl (sb-ext:bytes-consed-between-gcs)))
      (quit))

    ;; process command line arguments
    (setf
     *script* (arg-pop)
     *path* (arg-pop)
     *orig* (from-file (make-instance 'asm-perf) *path*)
     *res-dir* (append (pathname-directory *path*)
                       (list (concatenate 'string
                               (pathname-name *path*) ".opt"))))

    ;; process command line options
    (getopts
     ("-c" "--cross-chance" (setf *cross-chance* (parse-number (arg-pop))))
     ("-C" "--config"    (load (arg-pop)))
     ("-e" "--eval"      (eval (read-from-string (arg-pop))))
     ("-E" "--max-err"   (setf *max-err* (read-from-string (arg-pop))))
     ("-f" "--fit-evals" (setf *evals* (parse-integer (arg-pop))))
     ("-F" "--fit-func"  (throw-error "Fitness Functions are not implemented."))
     (nil  "--fit-pred"  (setf *fitness-predicate* (read (arg-pop))))
     ("-g" "--gc-size"
           #+ccl  (ccl:set-lisp-heap-gc-threshold (parse-integer (arg-pop)))
           #+sbcl (setf (sb-ext:bytes-consed-between-gcs)
                        (parse-integer (arg-pop))))
     ("-l" "--linker"    (setf (linker *orig*) (arg-pop)))
     ("-L" "--lflags"    (setf (flags *orig*)
                               (split-sequence #\Space (arg-pop)
                                               :remove-empty-subseqs t)))
     ("-m" "--mut-rate"  (setf *mut-rate* (parse-number (arg-pop))))
     ("-M" "--mcmc"      (setf *mcmc* t))
     ("-p" "--pop-size"  (setf *max-population-size*
                               (parse-integer (arg-pop))))
     ("-P" "--period"    (setf *period* (parse-integer (arg-pop))))
     ("-r" "--res-dir"   (setf *res-dir*
                               (let ((dir (arg-pop)))
                                 (pathname-directory
                                  (if (string= (subseq dir (1- (length dir)))
                                               "/")
                                      dir (concatenate 'string dir "/"))))))
     ("-R" "--rep"       (setf *rep* (intern (string-upcase (arg-pop)))))
     ("-s" "--evict-size" (setf *tournament-eviction-size*
                                (parse-integer (arg-pop))))
     ("-t" "--threads"   (setf *threads* (parse-integer (arg-pop))))
     ("-T" "--tourny-size" (setf *tournament-size* (parse-integer (arg-pop))))
     ("-v" "--verbose"   (let ((lvl (parse-integer (arg-pop))))
                           (when (>= lvl 4) (setf *shell-debug* t))
                           (setf *note-level* lvl)))
     ("-w" "--work-dir"  (setf *work-dir* (arg-pop))))
    (unless *period* (setf *period* (ceiling (/ *evals* (expt 2 10)))))

    ;; directories for results saving and logging
    (unless (ensure-directories-exist (make-pathname :directory *res-dir*))
      (throw-error "Unable to make result directory `~a'.~%" *res-dir*))
    (let ((log-name (make-pathname :directory *res-dir*
                                   :name "optimize"
                                   :type "log")))
      (if (probe-file log-name)
          (throw-error "Log file already exists ~S.~%" log-name)
          (push
           #+ccl (open log-name :direction :output :sharing :external)
           #-ccl (open log-name :direction :output)
           *note-out*)))

    (unless *model*
      (setf *model* (case (arch)
                      (:intel 'intel-sandybridge-power-model)
                      (:amd   'amd-opteron-power-model))))
    (when (symbolp *model*) (setf *model* (eval *model*)))

    ;; write out version information
    (note 1 *version*)

    ;; write out configuration parameters
    (note 1 "Parameters:~%~S~%"
          (mapcar (lambda (param)
                    (cons param (eval param)))
                  '(*path*
                    *benchmark*
                    *size*
                    (linker *orig*)
                    (flags *orig*)
                    *threads*
                    *mcmc*
                    *mut-rate*
                    *cross-chance*
                    *evals*
                    *tournament-size*
                    *work-dir*
                    *max-err*
                    *max-population-size*
                    *model*
                    *period*
                    *rep*
                    *note-level*
                    *res-dir*)))

    ;; Convert the program to the specified representation
    (case *rep*
      (light
       (setf *orig* (to-asm-light *orig*)))
      (range
       (setf *rep* (coerce (mapcar {aget :line} (genome *orig*)) 'vector))
       (setf *orig* (to-asm-range *orig*))
       (setf (reference *orig*) *rep*))
      (asm)
      (t (throw-error "representation ~S is not recognized" *rep*)))

    ;; Run optimization
    (unless (fitness *orig*)
      (note 1 "Evaluating the original.")
      (setf (fitness *orig*) (test *orig*)))
    (note 1 "~S~%" `((:orig-stats   . ,(stats *orig*))
                     (:orig-fitness . ,(fitness *orig*))))

    ;; sanity check
    (when (= (fitness *orig*) infinity)
      (throw-error "Original program has no fitness!"))

    ;; actually perform the optimization
    (if *mcmc*
        (progn
          (when (> *threads* 1)
            (throw-error "Multi-threaded MCMC is not supported."))
          (note 1 "Starting MCMC search")
          (setf *population* (list *orig*))
          (mcmc *orig* #'test :max-evals *evals*
                :every-fn
                (lambda (new)
                  (when (funcall *fitness-predicate* new (car *population*))
                    (setf *population* (list new))))
                :period *period*
                :period-fn (lambda () (mapc #'funcall *checkpoint-funcs*))))
        (progn
          ;; populate population
          (unless *population* ;; don't re-populate an existing population
            (note 1 "Building the Population")
            #+ccl (ccl:egc nil)
            (setf *population* (loop :for n :below *max-population-size*
                                  :collect (copy *orig*)))
            #+ccl (ccl:egc t))

          ;; run optimization
          (note 1 "Kicking off ~a optimization threads" *threads*)

          (let (threads)
            ;; kick off optimization threads
            (loop :for n :below *threads* :do
               (push (make-thread #'do-evolve) threads))
            ;; wait for all threads to return
            (mapc #'join-thread threads))))

    #+sbcl (sb-ext:gc :force t)
    (store *population* (make-pathname :directory *res-dir*
                                       :name "final-pop"
                                       :type "store"))
    (store (extremum *population* *fitness-predicate* :key #'fitness)
           (make-pathname :directory *res-dir*
                          :name "final-best"
                          :type "store"))

    (note 1 "done after ~a fitness evaluations~%" *fitness-evals*)
    (note 1 "results saved in ~a~%" *res-dir*)
    (close (pop *note-out*))))
