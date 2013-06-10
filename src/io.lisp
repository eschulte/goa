;;; io.lisp --- socket communication

;; Copyright (C) 2013  Eric Schulte

;;; Commentary:

;; Uses iolib instead of ZMQ.  This seems to be a little bit more reliable.

;;; Code:
(in-package :optimize)
(require :iolib)
(use-package :iolib)

(defvar *port* 3000 "Default port.")

(defun share (software &key (host "localhost") (port *port*))
  "Push SOFTWARE to the server listening on PORT at HOST."
  (handler-case
      (with-open-socket (socket :connect :active :type :stream)
        (connect socket (lookup-hostname host) :port port :wait t)
        (note 1 "sharing ~S with ~A:~A" software host port)
        (handler-case (progn (store software socket) (finish-output socket))
          (socket-connection-reset-error ()
            (note 1 "server-error: connection reset"))
          (hangup ()
            (note 1 "server-error: hangup"))
          (end-of-file ()
            (note 1 "server-error: end-of-file"))
          (error (e)
            (note 1 "transmission-error: ~S" e))))
    (error (e)
      (note 1 "connection-error: ~S" e))))

(defun accept (&key (port *port*) (add-fn #'incorporate))
  "Accept and incorporate any incoming individuals on PORT.
ADD-FN will be called to incorporate received objects into the
`*population*', if not specified the `incorporate' function is used."
  (with-open-socket (server :connect :passive :type :stream)
    ;; Bind the socket to all interfaces with specified port.
    (bind-address server +ipv4-unspecified+ :port port :reuse-addr t)

    ;; Start listening on the server socket
    (listen-on server :backlog 5)
    (note 1 "listening on ~a~%" port)

    ;; Keep accepting connections forever.
    (loop :while *running* :do
       (handler-case
           (with-accept-connection (client server :wait t)

             ;; When we get a new connection, show who it is from.
             (multiple-value-bind (who rport) (remote-name client)
               (note 1 "connection from ~A:~A" who rport))

             ;; Process data from the client.
             (handler-case (progn (funcall add-fn (restore client))
                                  (note 1 "incorporated"))
               (error (e) (note 1 "accept failed: ~S" e))))
         (error (e) (note 1 "bind failed: ~S" e))))))
