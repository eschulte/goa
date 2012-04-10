;;; analyze.lisp --- analyze saved populations of software variants

;; Copyright (C) 2012  Eric Schulte

;;; Commentary:

;; Helper functions for analyzing saved run results.

;;; Code:
(require :software-evolution)
(in-package :software-evolution)
(load "./optimize.lisp")

(defvar *stats* nil "List of statistics read from a run.")

(defun getter (key) (lambda (it) (cdr (assoc key it))))

(defun read-run (&key (steps 100) &aux results)
  (dotimes (n steps) (push (restore (file-for-run n)) results))
  (nreverse results))

(defun stat (run &key (field 'time-wo-init) (func #'mean))
  (mapcar (lambda (gen) (funcall func (mapcar (getter field) gen))) run))
