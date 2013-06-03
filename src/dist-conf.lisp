;;; dist-conf.lisp --- configuration file for distributed sub-pop evolution

;; Copyright (C) 2013  Eric Schulte

;; run with the optimize script with something like the following
;; optimize bzip2.s bzip2 -e "(defvar optimize::*port* 4000)" -c dist-conf.lisp

;;; Commentary:

;; Requires version 3 of zeromq and the latest common lisp zeromq
;; bindings, available at the following urls respectively.
;; 
;; http://zguide.zeromq.org/
;; http://repo.or.cz/w/cl-zmq.git

;;; Code:
(in-package :optimize)


;;; code for individual distribution
(require :zeromq)

(defvar *max-msg-size* (expt 2 20))

(defun accept (address)
  "Accept and `incorporate' any incoming individuals on ADDRESS.
ADDRESS should be of the form \"tcp://localhost:6666\"."
  (zmq:with-context (ctx)
    (zmq:with-socket (s ctx :pull)
      (zmq:connect s address)
      (zmq:setsockopt s :maxmsgsize *max-msg-size*)
      ;; In the case of superfluous zmq system call errors
      ;; (handler-case (error (e) "~&zmq error ~a~%" e))
      (loop (let ((msg (make-instance 'zmq:msg)))
              (zmq:msg-recv s msg)
              (let* ((data (zmq:msg-data-as-array msg))
                     (ind (when (not (zerop (length data)))
                            (ignore-errors
                              (from-bytes
                               (gzip-stream:gunzip-sequence data))))))
                (if ind
                    (progn (incorporate ind)
                           (note 1 "~a: WORK message ~d~%"
                                 address (length data)))
                    (note 1 "~a: FAIL message ~d~%"
                          address (length data)))))))))

(defun share (software address)
  "Push SOFTWARE to ADDRESS.
ADDRESS should be of the form \"tcp://*:6666\"."
  (let ((data (gzip-stream:gzip-sequence (to-bytes software))))
    (assert (< (length data) *max-msg-size*) (data *max-msg-size*)
            "message data length (~d) exceeds max allowable size (~d)"
            (length data) *max-msg-size*)
    (note t "sharing ~d with ~a" (length data) address)
    (zmq:with-context (ctx)
      (zmq:with-socket (s ctx :push)
        (zmq:setsockopt s :maxmsgsize *max-msg-size*)
        (zmq:bind s address)
        (zmq:msg-send s (make-instance 'zmq:msg :data data))))))
