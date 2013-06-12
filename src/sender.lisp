;;; sender.lisp --- to test zeromq variant distribution

;; Copyright (C) 2013  Eric Schulte

;;; Commentary:

;; In one terminal, run the receiver.
;;
;;   optimize benchmarks/prog/prog.s prog -e "(defvar optimize::port 3333)" \
;;       -c src/receiver.lisp
;;
;; In a different terminal, send variants to the receiver
;;
;;   optimize benchmarks/prog/prog.s prog -e "(defvar optimize::port 3333)" \
;;       -c src/sender.lisp

;;; Code:
(in-package :optimize)
(load "src/dist-conf")

(let ((address (format nil "tcp://*:~d" port)))
  (format t "sharing an individual with ~S~%" address)
  (share *orig* address))

(format t "done~%")

(error-out)
