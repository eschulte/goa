(load "src/perf-opt.lisp")
(in-package :perf-opt)

(defvar *test-fmt* "../../bin/bs-test ~a -n 1 -t 12000 -r -p"
  "Script used to evaluate variants.
Take the path to a blackscholes executable, and returns the difference
between it's output and the oracle output.")

(defvar *orig* (from-file (make-instance 'asm-perf :linker "g++")
                          "benchmarks/blackscholes/src/blackscholes.s"))

(defvar *output-size* 83096.7
  "Sum of all output from a correct run of the original.")

(defvar *max-err* (/ *output-size* (expt 10 3))
  "Maximum error allowed, 3 orders of magnitude below total output.")

(defun neutralp (asm)
  (when-let ((err (cdr (assoc :error (stats asm)))))
    (and (zerop (cdr (assoc :exit (stats asm))))
         (numberp err)
         (< err *max-err*))))

(defun multi-obj (asm)
  (unless (stats asm) (setf (stats asm) (test asm)))
  (or (ignore-errors
        (when (and (neutralp asm)
                   (aget :instructions (stats asm))
                   (aget :error (stats asm)))
          (let ((err (aget :error (stats asm))))
            (+ (* (aget :instructions (stats asm))
                  (+ 1 (/ err *max-err*)))
               (length (genome asm))))))
      infinity))


;;; Optimization
(setf *work-dir* "sh-runner/work/")

(setf
 (fitness *orig*) (multi-obj *orig*)
 *max-population-size* (expt 2 10)
 *tournament-size* 4
 *fitness-predicate* #'<
 *population* (loop :for n :below *max-population-size* :collect (copy *orig*)))

(defvar *inc-counter* 0
  "To only save the population a fraction of the time stats are saved.")

(loop :for i :from 0 :to 7 :do
   (sb-thread:make-thread
    (lambda ()
      (evolve
       #'multi-obj
       :filter (lambda (var) (< (fitness var) (* 10 (fitness *orig*))))
       :period (expt 2 9)
       :period-func
       (lambda ()
         ;; free memory before these memory-hog operations
         (sb-ext:gc :force t)
         ;; save stats on the run to a file
         (let ((log (format nil "~a/stats" *base*))
               (multi-obj (mapcar #'multi-obj *population*))
               (error (mapcar [{aget :error} #'stats] *population*))
               (instrs (mapcar [{aget :instructions} #'stats] *population*))
               (length (mapcar [#'length #'genome] *population*)))
           (flet ((stats (samp)
                    (list (mean samp) (apply #'min samp) (apply #'max samp))))
             (with-open-file (out log :direction :output :if-exists :append)
               (format out "~&~{~a~^ ~}~%"
                       (mapcar #'float
                               `(,*fitness-evals*
                                 ,@(stats multi-obj)
                                 ,@(stats error)
                                 ,@(stats instrs)
                                 ,@(stats length)))))))
         (when (zerop (mod *inc-counter* 8))
           ;; store the population in a file
           (store *population*
                  (format nil "~a/~d-pop.store" *base* *fitness-evals*)))
         (incf *inc-counter*))))
    :name (format nil "opt-~d" i)))
