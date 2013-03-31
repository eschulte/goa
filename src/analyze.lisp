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

(defvar *run* nil "Hold all pops from a run.")

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

(defun pop-means (stat)
  (mapcar [#'mean {mapcar {aget stat}} #'cdr] (sort *run* #'< :key #'car)))

(defun stat-to-file (stat file)
  (with-open-file (out file :direction :output)
    (mapc [{format out "~a~%"} #'float] (pop-means stat))))

#+laptop-analysis
(progn
(cl-fad:walk-directory "results/bs-evo-t4/"
   (lambda (file)
     (multiple-value-bind (whole matches)
         (scan-to-strings "pop-([0-9]+)-stats" (pathname-name file))
       (unless (null matches)
         (push (cons (parse-number (aref matches 0)) (restore file))
               *run*)))))

(loop :for eval :in *evals* :do
   (let ((pop (restore (format nil "results/bs-evo/pop-~d.store" eval))))
     (mapcar #'bs-opt::clean-stats pop)
     (push (cons eval pop) *pops*)))

(mapc {apply #'stat-to-file}
      '((:fit   "results/t4-fit.data")
        (:size  "results/t4-size.data")
        (:error "results/t4-error.data")
        (:instr "results/t4-instr.data")))

;; most fit at beginning of run
(extremum (cdar *run*) #'< :key {aget :fit})
;; ((:FIT . 1.4960707e8) (:EDITS . 2) (:SIZE . 1103) (:ERROR . 0) (:INSTR . 149605972))

;; most fit at end of run
(extremum (cdar (last *run*)) #'< :key {aget :fit})
;; ((:FIT . 6281291.5) (:EDITS . 6) (:SIZE . 1109) (:ERROR . 4.75942) (:INSTR . 5939967))

(caar (last *run*)) ;; => 254135
)
