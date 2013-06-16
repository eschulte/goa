;;; run-optimize.lisp --- command line optimization driver

;; Copyright (C) 2013  Eric Schulte

;;; Code:
(in-package :optimize)

(defvar *help* "Usage: optimize program.s benchmark-name [OPTIONS...]
 Optimize the assembly code of a benchmark program

Options:
 -c,--config FILE ------ read configuration from FILE
 -E,--max-error NUM ---- maximum allowed error
 -e,--eval SEXP -------- evaluate S-expression SEXP
 -F,--fit-evals NUM ---- max number of fitness evals
                         default: 2^18
 -f,--flags FLAGS ------ flags to use when linking
 -L,--light ------------ use lighter genome representation
 -l,--linker LINKER ---- linker to use
 -m,--model NAME ------- model name
 -P,--period NUM ------- period (in evals) of checkpoints
                         default: max-evals/(2^10)
 -p,--pop-size NUM ----- population size
                         default: 2^9
 -r,--res-dir DIR ------ save results to dir
                         default: program.opt/
 -s,--size SIZE -------- input size test,tiny,small,medium,large
 -T,--tourny-size NUM -- tournament size
                         default: 4
 -t,--threads NUM ------ number of threads
 -v,--verbose NUM ------ verbosity level 0-4
 -w,--work-dir DIR ----- use an sh-runner/work directory~%")

(defvar *checkpoint-funcs* (list #'checkpoint)
  "Functions to record checkpoints.")

(defun do-optimize ()
  (evolve #'test :max-evals *evals*
          :period *period*
          :period-fn (lambda () (mapc #'funcall *checkpoint-funcs*))))

(setf *note-level* 1)

(defun main (&optional (args *arguments*))
  (in-package :optimize)
  (flet ((arg-pop () (pop args)))

    ;; check command line arguments
    (when (or (when (<= (length args) 2)
                (format t "Insufficient command line arguments~%~%") t)
              (string= (subseq (car args) 0 2) "-h")
              (string= (subseq (car args) 0 3) "--h"))
      (format t *help*)
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
     ("-c" "--config"    (load (arg-pop)))
     ("-E" "--max-err"   (setf *max-err* (read-from-string (arg-pop))))
     ("-e" "--eval"      (eval (read-from-string (arg-pop))))
     ("-F" "--fit-evals" (setf *evals* (parse-integer (arg-pop))))
     ("-f" "--flags"     (setf (flags *orig*) (list (arg-pop))))
     ("-L" "--light"     (setf *orig* (to-asm-light *orig*)))
     ("-l" "--linker"    (setf (linker *orig*) (arg-pop)))
     ("-m" "--model"     (setf *model* (intern (string-upcase (arg-pop)))))
     ("-P" "--period"    (setf *period* (parse-integer (arg-pop))))
     ("-p" "--pop-size"  (setf *max-population-size*
                               (parse-integer (arg-pop))))
     ("-r" "--res-dir"   (setf *res-dir*
                               (let ((dir (arg-pop)))
                                 (pathname-directory
                                  (if (string= (subseq dir (1- (length dir)))
                                               "/")
                                      dir (concatenate 'string dir "/"))))))
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
                      (:intel 'intel-sandybridge-energy-model)
                      (:amd   'amd-opteron-energy-model))))
    (when (symbolp *model*) (setf *model* (eval *model*)))

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
                    *tournament-size*
                    *evals*
                    *work-dir*
                    *max-err*
                    *max-population-size*
                    *model*
                    *period*
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

    ;; populate population
    (unless *population* ;; only if it hasn't already been populated
      (note 1 "Building the Population")
      #+ccl (ccl:set-lisp-heap-gc-threshold (expt 2 30))
      #+ccl (ccl:egc nil)
      (setf *population* (loop :for n :below *max-population-size*
                            :collect (copy *orig*)))
      #+ccl (ccl:egc t))

    ;; run optimization
    (note 1 "Kicking off ~a optimization threads" *threads*)

    (let (threads)
      ;; kick off optimization threads
      (loop :for n :below *threads* :do
         (push (make-thread #'do-optimize) threads))
      ;; wait for all threads to return
      (mapc #'join-thread threads))

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
    (close (pop *note-out*))
    (quit)))

