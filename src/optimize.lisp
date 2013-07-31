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

(defmethod from-file ((asm asm-range) file)
  (to-asm-range (from-file (make-instance 'asm-perf) file)))

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
(defvar *fitness-function* nil "Fitness function.")
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
    (multiple-value-bind (stdout stderr errno) (shell *script* bin)
      (declare (ignorable stderr) (ignorable errno))
      (ignore-errors (parse-stdout stdout)))))

(defun apply-fitness-function (fitness-function stats)
  "Apply FITNESS-FUNCTION to STATS."
  (flet ((key-to-sym (keyword)
           (if (keywordp keyword)
               (intern (string-upcase (symbol-name keyword)) :optimize)
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

(defun test (asm)
  (note 4 "testing ~S~%" asm)
  (or (ignore-errors
        (unless (stats asm) (setf (stats asm) (run asm)))
        (note 4 "stats:~%~S~%" (stats asm))
        (when (and (zerop (aget :exit (stats asm)))
                   (<= (aget :error (stats asm)) *max-err*))
          (apply-fitness-function *fitness-function* (stats asm))))
      infinity))

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
(defvar *checkpoint-funcs* (list #'checkpoint)
  "Functions to record checkpoints.")

(defun optimize (&optional (args *arguments*))
  (in-package :optimize)
  (let ((help "Usage: opt TEST-SCRIPT ASM-FILE [OPTIONS...]
 Optimize the assembly code in ASM-FILE against TEST-SCRIPT.

TEST-SCRIPT:
  Command line used to evaluate executables.  If the test
  script contains the substring \"~~a\" it will be replaced
  with the name of the executable, otherwise the executable
  will be appended to the end of the test script.  The script
  should return a single numeric fitness or multiple metrics
  in csv format.

ASM-FILE:
  A text file of assembler code or (if using the \".store\"
  extension) a serialized software object.

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
        (version
         (format nil
          #+ccl "optimize version ~a using Clozure Common Lisp (CCL)~%"
          #+sbcl "optimize version ~a using Steel Bank Common Lisp (SBCL)~%"
          (eval-when (:compile-toplevel :load-toplevel :execute)
            (let ((raw (shell "git describe --always")))
              (subseq raw 0 (1- (length raw)))))))
        (do-evolve
            (lambda ()
              (evolve #'test :max-evals *evals*
                      :period *period*
                      :period-fn (lambda () (mapc #'funcall *checkpoint-funcs*)))))
        (*rep* 'range) linker flags)
    (setf *note-level* 1)
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
          (progn (format t version) (quit))
          (format t help
                  #+ccl "space left after a full GC pass"
                  #+sbcl "bytes consed between every GC pass"
                  #+ccl (ccl:lisp-heap-gc-threshold)
                  #+sbcl (sb-ext:bytes-consed-between-gcs)))
      (quit))

    ;; process mandatory command line arguments
    (setf *script* (let ((script (pop args)))
                     (if (scan "~a" script)
                         script
                         (format nil "~a ~~a" script)))
          *path*   (pop args))

    (when (string= (pathname-type (pathname *path*)) "store")
      (setf *orig* (restore *path*)))

    ;; process command line options
    (getopts
     ("-c" "--cross-chance" (setf *cross-chance* (parse-number (pop args))))
     ("-C" "--config"    (load (pop args)))
     ("-e" "--eval"      (eval (read-from-string (pop args))))
     ("-E" "--max-err"   (setf *max-err* (read-from-string (pop args))))
     ("-f" "--fit-evals" (setf *evals* (parse-integer (pop args))))
     ("-F" "--fit-func" (setf *fitness-function* (read-from-string (pop args))))
     (nil "--fit-pred" (setf *fitness-predicate* (read-from-string (pop args))))
     ("-g" "--gc-size"
           #+ccl  (ccl:set-lisp-heap-gc-threshold (parse-integer (pop args)))
           #+sbcl (setf (sb-ext:bytes-consed-between-gcs)
                        (parse-integer (pop args))))
     ("-l" "--linker"    (setf linker (pop args)))
     ("-L" "--lflags"    (setf flags (split-sequence #\Space (pop args)
                                                     :remove-empty-subseqs t)))
     ("-m" "--mut-rate"  (setf *mut-rate* (parse-number (pop args))))
     ("-M" "--mcmc"      (setf *mcmc* t))
     ("-p" "--pop-size"  (setf *max-population-size*
                               (parse-integer (pop args))))
     ("-P" "--period"    (setf *period* (parse-integer (pop args))))
     ("-r" "--res-dir"   (setf *res-dir*
                               (let ((dir (pop args)))
                                 (pathname-directory
                                  (if (string= (subseq dir (1- (length dir)))
                                               "/")
                                      dir (concatenate 'string dir "/"))))))
     ("-R" "--rep"       (setf *rep* (intern (string-upcase (pop args)))))
     ("-s" "--evict-size" (setf *tournament-eviction-size*
                                (parse-integer (pop args))))
     ("-t" "--threads"   (setf *threads* (parse-integer (pop args))))
     ("-T" "--tourny-size" (setf *tournament-size* (parse-integer (pop args))))
     ("-v" "--verbose"   (let ((lvl (parse-integer (pop args))))
                           (when (>= lvl 4) (setf *shell-debug* t))
                           (setf *note-level* lvl)))
     ("-w" "--work-dir"  (setf *work-dir* (pop args))))
    (unless *period* (setf *period* (ceiling (/ *evals* (expt 2 10)))))
    (unless *orig*
      (setf *orig* (from-file (make-instance (case *rep*
                                               (asm 'asm-perf)
                                               (light 'asm-light)
                                               (range 'asm-range)))
                              *path*)))
    (when linker (setf (linker *orig*) linker))
    (when flags  (setf (flags  *orig*) flags))

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

    ;; write out version information
    (note 1 version)

    ;; write out configuration parameters
    (note 1 "Parameters:~%~S~%"
          (mapcar (lambda (param)
                    (cons param (eval param)))
                  '(*path*
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
                    *period*
                    *rep*
                    *note-level*
                    *res-dir*)))

    ;; Run optimization
    (unless (fitness *orig*)
      (note 1 "Evaluating the original.")
      (setf (fitness *orig*) (test *orig*)))
    (note 1 "~S~%" `((:orig-stats   . ,(stats *orig*))
                     (:orig-fitness . ,(fitness *orig*))))

    ;; sanity check
    (when (= (fitness *orig*) infinity)
      (throw-error "Original program has no fitness!"))

    ;; save the original
    (store *orig* (make-pathname :directory *res-dir*
                                 :name "original"
                                 :type "store"))

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
               (push (make-thread do-evolve) threads))
            ;; wait for all threads to return
            (mapc #'join-thread threads))))

    ;; finish up
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
