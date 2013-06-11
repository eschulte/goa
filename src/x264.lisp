;;; neutral.lisp --- generate neutral versions of x264
(load "src/perf-opt.lisp")
(in-package :perf-opt)

(defvar *test-fmt* "../../bin/x264-test ~a -p -d"
  "Script used to evaluate variants.")

(defvar *asm-lib* "../../data/libx264-asm.a")

(defvar *flags*
  `("-D_GNUCC" ,*asm-lib*
    "-L/usr/lib32" "-L/usr/lib" "-L/usr/lib32" "-L/usr/lib"
    "-lm" "-lpthread" "-s"))

(defvar *orig* (from-file (make-instance 'cil-perf
                            :compiler "cilly" :flags *flags*)
                          "data/x264_comb.c"))

(defvar *stat-mod* 0
  "To only save the population a fraction of the time stats are saved.")

(defun neutralp (asm)
  (zerop (cdr (assoc :exit (stats asm)))))

(defun multi-obj (cil)
  (or (ignore-errors
        (unless (stats cil) (setf (stats cil) (test cil)))
        (when (neutralp cil)
          (let ((orig-instrs 1882872477)
                (orig-length 2628540))
            (+ (/ (aget :instructions (stats cil)) orig-instrs)
               (/ (length (genome cil)) orig-length)))))
      infinity))

(defvar *base* "results/x264-2" "Where to store incremental results.")

(defun checkpoint ()
  ;; free memory before these memory-hog operations
  (sb-ext:gc :force t)
  ;; save stats on the run to a file
  (let ((log (format nil "~a/stats" *base*))
        (fitness (mapcar #'fitness *population*))
        (instrs (remove nil
                  (mapcar [{aget :instructions} #'stats] *population*)))
        (length (mapcar [#'length #'genome] *population*)))
    (flet ((stats (samp)
             (if (null samp)
                 (list 0 0 0)
                 (list (mean samp) (apply #'min samp) (apply #'max samp)))))
      (with-open-file (out log :direction :output :if-exists :append)
        (format out "~&~{~a~^ ~}~%"
                (mapcar #'float
                        `(,*fitness-evals*
                          ,@(stats fitness)
                          ,@(stats instrs)
                          ,@(stats length)))))))
  (when (zerop (mod *stat-mod* 8))
    ;; store the population in a file
    (store *population*
           (format nil "~a/~d-pop.store" *base* *fitness-evals*)))
  (incf *stat-mod*))


;;; Artificial Selection
#+run
(progn

(setf *work-dir* "sh-runner/work/")

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
       :period-fn #'checkpoint
       ))
    :name (format nil "opt-~d" i)))
)
