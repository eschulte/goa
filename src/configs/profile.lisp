;;; profile.lisp --- profile using the SBCL statistical profiler

;; Copyright (C) 2013  Eric Schulte

;;; Commentary:

;; Load this file with the -C option to collect profiling information
;; of the execution of the optimization process itself and report
;; profiling output at the end of the run.

;;; Code:
(in-package :optimize)
(require :sb-sprof)
(sb-sprof:start-profiling :max-samples (* 2 50000))

(defun finish-profiling-and-report ()
  (sb-sprof:stop-profiling)
  (sb-sprof:report
   :stream (apply #'make-broadcast-stream
                  (mapcar (lambda (stream)
                            (if (equalp stream t) *standard-output* stream))
                          *note-out*))))

(push *final-funcs* #'finish-profiling-and-report)
