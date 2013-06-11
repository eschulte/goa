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
 *evals* (expt 2 13))

(loop :for i :below 4 :do
   (sb-thread:make-thread (lambda () (evolve #'test :max-evals *evals*))))
