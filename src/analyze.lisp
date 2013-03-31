;;; analyze.lisp --- analyze saved populations of software variants

;; Copyright (C) 2012  Eric Schulte

;;; Commentary:

;; Helper functions for analyzing saved run results.

;;; Code:
(mapcar #'require '(:cl-store :cl-fad :lhstats))
(in-package :bs-opt)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))

(defvar *stats* nil "List of statistics read from a run.")

(defvar *pops* nil "Hold all pops from a run.")

(defvar *evals*
  '(256 512 768 1024 1280 1536 1792 2048 2304 2560 2816 3072 3328 3584 3840 4096
    4352 4608 4864 5120 5376 5632 5888 6144 6400 6656 6912 7168 7424 7680 7936
    8192 8448 8704 8960 9216 9472 9728 9984 10240 10496 10752 11008 11264 11520
    11776 12032 12288 12544 12800 13056 13312 13568 13824 14080 14336 14592
    14848 15104 15360 15616 15872 16128 16384 16640 16896 17152 17408 17664
    17920 18176 18432 18688))

(loop :for eval :in *evals* :do
   (let ((pop (restore (format nil "results/bs-evo/pop-~d.store" eval))))
     (mapcar #'bs-opt::clean-stats pop)
     (push (cons eval pop) *pops*)))

(defun stat-summary (sample)
  (mapcar [#'float {funcall _ sample}]
          (list #'length
                #'mean
                #'sd
                {apply #'min}
                {percentile _ 25}
                {percentile _ 50}
                {percentile _ 75}
                {apply #'max})))

;; print stats across all populations
(with-open-file (out "/tmp/err.data" :direction :output)
  (loop :for eval :in *evals* :do
     (format out "~a ~{~a~^ ~}~%" eval
             (stat-summary (remove bs-opt::infinity
                             (remove nil
                               (mapcar [{bs-opt::aget 'bs-opt::error} #'bs-opt::stats]
                                       (cdr (assoc eval *pops*)))))))))
