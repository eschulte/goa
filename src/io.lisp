;;; io.lisp --- socket communication

;; Copyright (C) 2013  Eric Schulte

;;; Commentary:

;; Uses iolib instead of ZMQ.  This seems to be a little bit more reliable.

;;; Code:
(in-package :optimize)
(require :iolib)
(use-package :iolib)

(defvar *port* 3000 "Default port.")

(defun share (&key (host "localhost") (port *port*))
  (with-open-socket (socket :connect :active :type :stream)
    (connect socket (lookup-hostname host) :port port :wait t)
    (handler-case (progn (format socket "poop poop poop~%")
                         (finish-output socket))

      (socket-connection-reset-error ()
        (format t "Got connection reset. Server went away!"))

      (hangup ()
        (format t "Got hangup. Server closed connection on write!~%"))

      (end-of-file ()
        (format t "Got end-of-file. Server closed connection on read!~%")))))

(defun accept (&key (port *port*))
  (with-open-socket (server :connect :passive :type :stream)
    ;; Bind the socket to all interfaces with specified port.
    (bind-address server +ipv4-unspecified+ :port port :reuse-addr t)

    ;; Start listening on the server socket
    (listen-on server :backlog 5)
    (format t "Listening on socket bound to: ~A:~A~%"
            (local-host server)
            (local-port server))
    ;; Keep accepting connections forever.
    (loop
       (format t "Waiting to accept a connection...~%")

       ;; Using with-accept-connection, when this form returns it will
       ;; automatically close the client connection.
       (with-accept-connection (client server :wait t)

         ;; When we get a new connection, show who it is from.
         (multiple-value-bind (who rport) (remote-name client)
           (format t "Got a connnection from ~A:~A!~%" who rport))

         ;; Process data from the client.
         (format t "reading from connection")
         (let ((input (read-line client nil nil)))
           (format t "read ~S~%" input))))))
