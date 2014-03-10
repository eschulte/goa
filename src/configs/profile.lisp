;;; profile.lisp --- profile using the SBCL statistical profiler

;; Copyright (C) 2013  Eric Schulte

;;; Commentary:

;; Load this file with the -C option to collect profiling information
;; of the execution of the optimization process itself and report
;; profiling output at the end of the run.

;;; Code:
(in-package :goa)
(require :sb-sprof)

(defvar *start-time* (get-universal-time))
(sb-sprof:start-profiling :max-samples (* 2 50000))

(defun finish-profiling-and-report ()
  (sb-sprof:stop-profiling)
  (note 1 "Profiling results:")
  (mapc {format _ "Wall clock run time: ~d seconds~%"
         (- (get-universal-time) *start-time*)} *note-out*)
  (sb-sprof:report
   :stream (apply #'make-broadcast-stream
                  (mapcar (lambda (stream)
                            (if (equalp stream t) *standard-output* stream))
                          *note-out*))))

(push #'finish-profiling-and-report *final-funcs*)
