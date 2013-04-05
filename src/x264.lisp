;;; neutral.lisp --- generate neutral versions of x264
(load "src/perf-opt.lisp")
(in-package :perf-opt)

(defvar *test-fmt* "./bin/x264-test ~a -p -d"
  "Script used to evaluate variants.")

(defvar *asm-lib* "./data/libx264-asm.a")

(defvar *flags*
  `("-D_GNUCC" ,*asm-lib*
    "-L/usr/lib32" "-L/usr/lib" "-L/usr/lib32" "-L/usr/lib"
    "-lm" "-lpthread" "-s"))

(defvar *orig* (from-file (make-instance 'cil-perf
                            :compiler "cilly" :flags *flags*)
                          "data/x264_comb.c"))

(defun neutralp (asm)
  (zerop (cdr (assoc :exit (stats asm)))))

(defun multi-obj (cil)
  (unless (stats cil) (setf (stats cil) (test cil)))
  (or (ignore-errors
        (when (neutralp cil)
          (+ (aget :instructions (stats cil))
             (length (genome cil)))))
      infinity))


;;; Artificial Selection
#+run
(progn

(defvar *base* "results/x264-1" "Where to store incremental results.")

(setf *work-dir* "sh-runner/work/")

(defvar *stat-mod* 0
  "To only save the population a fraction of the time stats are saved.")

(setf
 (fitness *orig*) (multi-obj *orig*)
 *max-population-size* (expt 2 8)
 *tournament-size* 4
 *fitness-predicate* #'<
 *population* (loop :for n :below *max-population-size* :collect (copy *orig*)))

(loop :for i :from 1 :to 7 :do
   (sb-thread:make-thread
    (lambda ()
      (evolve
       #'multi-obj
       :filter #'neutralp
       :period (expt 2 7)
       :period-func
       (lambda ()
         ;; free memory before these memory-hog operations
         (sb-ext:gc :force t)
         ;; save stats on the run to a file
         (let ((log (format nil "~a/stats" *base*))
               (fitness (mapcar #'fitness *population*))
               (instrs (mapcar [{aget :instructions} #'stats] *population*))
               (length (mapcar [#'length #'genome] *population*)))
           (flet ((stats (samp)
                    (list (mean samp) (apply #'min samp) (apply #'max samp))))
             (with-open-file (out log :direction :output :if-exists :append)
               (format out "~&~{~a~^ ~}~%"
                       (mapcar #'float
                               `(,*fitness-evals*
                                 ,@(stats fitness)
                                 ,@(stats instrs)
                                 ,@(stats length)))))))
         (when (zerop (mod *inc-counter* 8))
           ;; store the population in a file
           (store *population*
                  (format nil "~a/~d-pop.store" *base* *fitness-evals*)))
         (incf *inc-counter*))))
    :name (format nil "opt-~d" i)))
)
