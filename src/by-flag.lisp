;;; dist-conf.lisp --- configuration file for by-flag sub-pop optimization

;; Copyright (C) 2013  Eric Schulte

;; run with the optimize script with something like the following
;; optimize bzip2.s bzip2 -e "(defvar optimize::*port* 4000)" -c dist-conf.lisp

;;; Commentary:

;; The following code assumes that the compiler flag has been saved
;; into the `flag' variable.

;;; Code:
(in-package :optimize)
(load "src/dist-conf.lisp")

(defvar ports
  '((O0 . 4000)
    (O1 . 4001)
    (O2 . 4002)
    (O3 . 4003)
    (Os . 4004))
  "Map compiler flags to ports for socket communication.")

;; Mark the genome in `*orig*' with the compiler flag used.
(setf (genome *orig*)
      (mapcar (lambda (l) (cons (list :flag flag) l)) (genome *orig*)))

;; Set the results directory.
(setq *res-dir* (format nil "~a/~a" *res-dir* flag))

;; Begin listening for shared individuals on flag-specified port.
(let ((address (format nil "tcp://localhost:~d" (aget flag ports))))
  (sb-thread:make-thread (lambda () (accept address)))
  (note 1 "listening on ~a~%" address))

;; Periodically share individuals with a neighbor.
(let ((original-checkpoint #'checkpoint))
  (defun checkpoint ()
    (funcall original-checkpoint)
    ;; share an individual with a neighbor
    (share (tournament)
           (format nil "tcp://*:~d"
                   (random-elt
                    (mapcar #'cdr (remove-if [{equal flag} #'car] ports)))))))
