;;; sender.lisp --- to test zeromq variant distribution

;; Copyright (C) 2013  Eric Schulte

;;; Commentary: (pending)

;;; Code:
(in-package :optimize)
(require :zeromq)

(defvar *data* (to-bytes *orig*))

;; apparently this is the largest message size that zmq can send
;; (make-instance 'zmq:msg :data (make-array '(415968)))

(assert (<= (length *data*) 415968) (*orig*)
        "program is too large to share ~d>~d"
        (length *data*) 415968)

(zmq:with-context (context 1)
  (zmq:with-socket (sender context zmq:push)
    (zmq:bind sender "tcp://lo:6666")
    (zmq:send sender (make-instance 'zmq:msg :data *data*))))

(format t "done~%")

(sb-ext:exit)
