(in-package :optimize)

;; run with e.g.,
;; optimize bzip2.s bzip2 -e "(defvar optimize::*port* 4000)" -c dist-conf.lisp

(format t "my port is ~d~%" *port*)

(sb-ext:exit)


;;; code for individual distribution
(require :zmq)

(defun accept (address)
  "Accept and `incorporate' any incoming individuals on ADDRESS.
The address should look like \"tcp://*:1234\"."
  (flet ((raw-bytes (msg)
           (let ((data (zmq:msg-data-as-is msg))
                 (bytes (make-array '(0)
                                    :element-type '(unsigned-byte 8)
                                    :fill-pointer 0 :adjustable t)))
             (loop :for i :from 0 :to (1- (zmq:msg-size msg))
                :do (vector-push-extend (cffi:mem-ref data :char i) bytes))
             bytes)))
    (zmq:with-context (context 1)
      (zmq:with-socket (socket context zmq:upstream)
        (zmq:bind socket address)
        (loop
           (handler-case
               (let ((msg (make-instance 'zmq:msg)))
                 ;; Wait for next msg from client
                 (zmq:recv socket msg)
                 (let ((raw (raw-bytes msg)))
                   (when (> (length raw) 0)
                     (incorporate (from-bytes raw)))))
             (error (e) "~&zmq error ~a~%" e)))))))

(defun share (individual address)
  "Broadcast INDIVIDUAL with ADDRESS."
  (zmq:with-context (context 1)
    (zmq:with-socket (socket context zmq:req)
      (zmq:connect socket address)
      (zmq:send socket (make-instance 'zmq:msg :data (to-bytes individual))))))


;;; Share individuals every checkpoint
(defvar *neighbors*
  '("tcp://192.168.1.42:5000"
    "tcp://192.168.1.42:5001"
    "tcp://192.168.1.42:5002"
    "tcp://192.168.1.42:5003"
    "tcp://192.168.1.42:5004"))

(let ((check *checkpoint-func*))
  (defun sharing-checkpoint ()
    (funcall check)
    (mapc (lambda (neighbor) (share (tounament) neighbor)) *neighbors*)))

(setf *checkpoint-func* #'sharing-checkpoint)

(sb-thread:make-thread (lambda () (accept (format nil "tcp://*:~d" *port*))))
