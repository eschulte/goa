;;; dist-conf.lisp --- configuration file for distributed sub-pop evolution

;; Copyright (C) 2013  Eric Schulte

;; run with the optimize script with something like the following
;; optimize bzip2.s bzip2 -e "(defvar optimize::*port* 4000)" -c dist-conf.lisp

;;; Code:
(in-package :optimize)


;;; code for individual distribution
(require :zeromq)

(defun accept (address)
  "Accept and `incorporate' any incoming individuals on ADDRESS.
ADDRESS should be of the form \"tcp://localhost:6666\"."
  (zmq:with-context (context 1)
    (zmq:with-socket (reciever context zmq:pull)
      (zmq:connect reciever address)
      (loop (handler-case
                (let ((msg (make-instance 'zmq:msg)))
                  (zmq:recv reciever msg)
                  (let ((data (zmq:msg-data-as-array msg)))
                    (format t "received a message[~d]~%" (length data))
                    (incorporate (from-bytes data))))
              (error (e) "~&zmq error ~a~%" e))))))

(defun share (software address)
  "Push SOFTWARE to ADDRESS.
ADDRESS should be of the form \"tcp://*:6666\"."
  (let ((data (to-bytes software)))
    (assert (<= (length data) 415968) (*orig*)
            "program is too large to share ~d>~d"
            (length data) 415968)
    (zmq:with-context (context 1)
      (zmq:with-socket (sender context zmq:push)
        (zmq:bind sender address)
        (zmq:send sender (make-instance 'zmq:msg :data data))))))
