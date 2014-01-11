;;; goa.lisp --- Genetic Optimization Algorithm executable

;; Copyright (C) 2012-2013  Eric Schulte

;;; Commentary:

;; Starting with an initial software object, generate a population of
;; variant implementations and then evolve to optimize some metric
;; such as fastest execution, least communication, lowest energy
;; consumption etc...

;;; Code:
(in-package :optimize)

;;; Main GOA executable
(defun goa (args)
  (in-package :optimize)
  (let ((help "Usage: ~a TEST-SCRIPT ASM-FILE [OPTIONS...]
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
  extension) a serialized assembly software object.

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
 -S,--random-state FILE - load random state from FILE
 -t,--threads NUM ------ number of threads
 -T,--tourny-size NUM -- selection tournament size
                         default: 1 (i.e., random selection)
 -v,--verbose NUM ------ verbosity level 0-4
 -V,--version ---------- print version and exit
 -w,--work-dir DIR ----- use an sh-runner/work directory~%")
        (self (pop args))
        (version
         (format nil
          #+ccl "optimize version ~a using Clozure Common Lisp (CCL)~%"
          #+sbcl "optimize version ~a using Steel Bank Common Lisp (SBCL)~%"
          *git-version*))
        (do-evolve
            (lambda ()
              #+ccl (note 1 "check in") ;; for ccl `*terminal-io*' sharing
              (evolve #'test :max-evals *evals*
                      :period *period*
                      :period-fn (lambda () (mapc #'funcall *checkpoint-funcs*)))))
        (*rep* 'range) random-state linker flags)
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
          (format t help self
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
     ("-S" "--random-state" (setf random-state (restore (pop args))))
     ("-t" "--threads"   (setf *threads* (parse-integer (pop args))))
     ("-T" "--tourny-size" (setf *tournament-size* (parse-integer (pop args))))
     ("-v" "--verbose"   (let ((lvl (parse-integer (pop args))))
                           (when (>= lvl 4) (setf *shell-debug* t))
                           (setf *note-level* lvl)))
     ("-w" "--work-dir"  (setf *work-dir* (pop args))))
    (setf *random-state* (or random-state (make-random-state t)))
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
                    *script*
                    *fitness-function*
                    *fitness-predicate*
                    (linker *orig*)
                    (flags *orig*)
                    *threads*
                    *mcmc*
                    *mut-rate*
                    *cross-chance*
                    *evals*
                    *tournament-size*
                    *tournament-eviction-size*
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
    (when (= (fitness *orig*) (worst))
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

          (let (threads
;;; see http://ccl.clozure.com/ccl-documentation.html#Background-Terminal-Input
                #+ccl
                (*default-special-bindings*
                 (list (cons '*terminal-io*
                             (make-two-way-stream
                              (make-string-input-stream "y")
                              (two-way-stream-output-stream
                               *terminal-io*))))))
            ;; kick off optimization threads
            (loop :for n :below *threads* :do
               (push (make-thread do-evolve :name (format nil "opt-~d" n))
                     threads))
            ;; wait for all threads to return
            (mapc #'join-thread threads))))

    ;; finish up
    (mapc #'funcall *final-funcs*)
    (note 1 "done after ~a fitness evaluations~%" *fitness-evals*)
    (note 1 "results saved in ~a~%" *res-dir*)
    (close (pop *note-out*))))
