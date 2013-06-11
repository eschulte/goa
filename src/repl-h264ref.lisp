(in-package :optimize)

(setf
 *orig* (from-file (make-instance 'asm-perf) "benchmarks/h264ref/h264ref.s")
 *benchmark* "h264ref"
 (flags *orig*) '("-lm" "-O3")
 *model* (case (arch)
           (:intel 'intel-sandybridge-energy-model)
           (:amd   'amd-opteron-energy-model))
 *model* (eval *model*)
 (fitness *orig*) (test *orig*)
 *max-population-size* (expt 2 4)
 *population* (loop :for n :below *max-population-size*
                 :collect (copy *orig*))
 *evals* (expt 2 10))

(defun safe-test (asm)
  (let ((used (/ (sb-vm::dynamic-usage) (sb-ext:dynamic-space-size))))
    (format t "~f ~s~%" used *fitness-evals*)
    (if (> used 1/2)
        (prog1 infinity (setf *running* nil))
        (test asm))))

(defun proactive-gc ()
  (ignore-errors
    (let ((used (/ (sb-vm::dynamic-usage) (sb-ext:dynamic-space-size))))
      (when (> used 1/2) (sb-ext:gc :force t :full t)))))

(loop :for i :below 4 :do
   (sb-thread:make-thread (lambda ()
                            (evolve #'test
                                    :max-evals *evals*
                                    :every-fn #'proactive-gc))))
