;;; run-optimize.lisp --- command line optimization driver

;; Copyright (C) 2013  Eric Schulte

;;; Code:
(in-package :optimize)

(defvar *help* "Usage: optimize program.s benchmark-name [OPTIONS...]
 Optimize the assembly code of a benchmark program

Options:
 -C,--config FILE ------ read configuration from FILE
 -c,--cross-p ---------- crossover chance
 -E,--max-error NUM ---- maximum allowed error
 -e,--eval SEXP -------- evaluate S-expression SEXP
 -F,--fit-evals NUM ---- max number of fitness evals
                         default: 2^18
 -f,--flags FLAGS ------ flags to use when linking
 -g,--gc-size ---------- ~a
                         default: ~:d
 -l,--linker LINKER ---- linker to use
 -M,--mcmc ------------- run MCMC search instead of GP
 -m,--mut-p NUM -------- mutation chance
 -o,--model NAME ------- model name
 -P,--period NUM ------- period (in evals) of checkpoints
                         default: max-evals/(2^10)
 -p,--pop-size NUM ----- population size
                         default: 2^9
 -R,--rep REP ---------- use REP program representation
                         asm, light, or range (default)
 -r,--res-dir DIR ------ save results to dir
                         default: program.opt/
 -s,--size SIZE -------- input size test,tiny,small,medium,large
 -T,--tourny-size NUM -- tournament size
                         default: 4
 -t,--threads NUM ------ number of threads
 -V,--version ---------- print version and exit
 -v,--verbose NUM ------ verbosity level 0-4
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

(defun do-evolve ()
  (evolve #'test :max-evals *evals*
          :period *period*
          :period-fn (lambda () (mapc #'funcall *checkpoint-funcs*))))

(defun main (&optional (args *arguments*))
  (in-package :optimize)
  (setf *note-level* 1)
  (flet ((arg-pop () (pop args)))
    (let ((rep 'range) mcmc)

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
       *path* (arg-pop)
       *benchmark* (arg-pop)
       *orig* (from-file (make-instance 'asm-perf) *path*)
       *res-dir* (append (pathname-directory *path*)
                         (list (concatenate 'string
                                 (pathname-name *path*) ".opt"))))

      ;; process command line options
      (getopts
       ("-C" "--config"    (load (arg-pop)))
       ("-c" "--cross-p"   (setf *cross-chance* (parse-number (arg-pop))))
       ("-E" "--max-err"   (setf *max-err* (read-from-string (arg-pop))))
       ("-e" "--eval"      (eval (read-from-string (arg-pop))))
       ("-F" "--fit-evals" (setf *evals* (parse-integer (arg-pop))))
       ("-f" "--flags"     (setf (flags *orig*) (list (arg-pop))))
       ("-g" "--gc-size"
             #+ccl  (ccl:set-lisp-heap-gc-threshold (parse-integer (arg-pop)))
             #+sbcl (setf (sb-ext:bytes-consed-between-gcs)
                          (parse-integer (arg-pop))))
       ("-l" "--linker"    (setf (linker *orig*) (arg-pop)))
       ("-M" "--mcmc"      (setf mcmc t))
       ("-m" "--mut-p"     (setf *mut-chance* (parse-number (arg-pop))))
       ("-o" "--model"     (setf *model* (intern (string-upcase (arg-pop)))))
       ("-P" "--period"    (setf *period* (parse-integer (arg-pop))))
       ("-p" "--pop-size"  (setf *max-population-size*
                                 (parse-integer (arg-pop))))
       ("-r" "--res-dir"   (setf *res-dir*
                                 (let ((dir (arg-pop)))
                                   (pathname-directory
                                    (if (string= (subseq dir (1- (length dir)))
                                                 "/")
                                        dir (concatenate 'string dir "/"))))))
       ("-R" "--rep"       (setf rep (intern (string-upcase (arg-pop)))))
       ("-s" "--size"      (setf *size* (arg-pop)))
       ("-T" "--tourny-size" (setf *tournament-size* (parse-integer (arg-pop))))
       ("-t" "--threads"   (setf *threads* (parse-integer (arg-pop))))
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
                      mcmc
                      *evals*
                      *tournament-size*
                      *work-dir*
                      *max-err*
                      *max-population-size*
                      *model*
                      *period*
                      rep
                      *note-level*
                      *res-dir*)))

      ;; Convert the program to the specified representation
      (case rep
        (light
         (setf *orig* (to-asm-light *orig*)))
        (range
         (setf rep (coerce (mapcar {aget :line} (genome *orig*)) 'vector))
         (setf *orig* (to-asm-range *orig*))
         (setf (reference *orig*) rep))
        (asm)
        (t (throw-error "representation ~S is not recognized" rep)))

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
      (if mcmc
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
      (close (pop *note-out*)))))
