;;; helpers.lisp --- REPL helpers

;; Copyright (C) 2013  Eric Schulte

;;; Code:
(load "src/optimize.lisp")
(in-package :optimize)

(defun feedgnuplot (list &key domain lines histogram)
  (with-open-stream (input (make-string-input-stream
                            (format nil "~{~{~a~^ ~}~^~%~}~%"
                                    (mapcar (lambda (el)
                                              (if (listp el) el (list el)))
                                            list))))
    (trivial-shell:shell-command
     (format nil "feedgnuplot --exit ~a ~a ~a"
             (if domain "--domain" "")
             (if lines  "--lines"  "")
             (if histogram (format nil "--histogram ~a"
                                   (if (numberp histogram) histogram 0)) ""))
     :input input)))
