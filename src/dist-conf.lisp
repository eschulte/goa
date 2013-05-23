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
The address should look like \"tcp://*:1234\"."
  ;; http://zguide.zeromq.org/lisp:wuclient
  (zmq:with-context (context 1)
    (zmq:with-socket (subscriber context zmq:sub)
      (zmq:connect subscriber address)
      (zmq:setsockopt subscriber zmq:subscribe "")
      (loop
         (let ((msg (make-instance 'zmq:msg)))
           ;; Wait for next msg from client
           (zmq:recv subscriber msg)
           (let ((raw (zmq:msg-data-as-array msg)))
             (format t "received message: ~S -> ~S~%" msg raw)
             (when (> (length raw) 0)
               (incorporate (from-bytes raw)))))))))

(defun share (software address)
  "Publish SOFTWARE with ADDRESS."
  ;; http://zguide.zeromq.org/lisp:wuserver
  (zmq:with-context (context 1)
    (zmq:with-socket (publisher context zmq:pub)
      (zmq:bind publisher address)
      (zmq:bind publisher "ipc://variant.ipc")
      (zmq:send publisher (make-instance 'zmq:msg :data (to-bytes software))))))


;;; Share individuals every checkpoint
;; (defvar *neighbors*
;;   '("tcp://192.168.1.42:5000"
;;     "tcp://192.168.1.42:5001"
;;     "tcp://192.168.1.42:5002"
;;     "tcp://192.168.1.42:5003"
;;     "tcp://192.168.1.42:5004"))

;; (let ((check *checkpoint-func*))
;;   (defun sharing-checkpoint ()
;;     (funcall check)
;;     (mapc (lambda (neighbor) (share (tounament) neighbor)) *neighbors*)))

;; (setf *checkpoint-func* #'sharing-checkpoint)

(sb-thread:make-thread (lambda () (accept (format nil "tcp://*:~d" *port*))))

(format t "started listening thread~%")

(format t "now to connect~%")

(share *orig* (format nil "tcp://localhost:~d" *port*))

(format t "shared an individual~%")

(format t "population is now of size ~d~%"
        (length *population*))

(sb-ext:exit)
