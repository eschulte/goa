;;; helpers.lisp --- REPL helpers

;; Copyright (C) 2013  Eric Schulte

;;; Code:
(load "src/optimize.lisp")
(in-package :goa)

(defun feedgnuplot (list &key domain lines histogram)
  (let ((proc
         (#+ccl ccl:run-program
          #+sbcl sb-ext:run-program
          "/usr/bin/feedgnuplot"
          `(,@(when domain '("--domain"))
              ,@(when lines  '("--lines"))
              ,@(when histogram
                      (list "--exit" "--histogram"
                            (format nil "~d"
                                    (if (numberp histogram) histogram 0)))))
          :input :stream :wait nil)))
    (with-open-stream (feed
                       #+ccl (ccl:external-process-input-stream proc)
                       #+sbcl (sb-ext:process-input proc))
      (format feed "~{~{~a~^ ~}~^~%~}~%" (mapcar (lambda (el)
                                                   (if (listp el) el (list el)))
                                                 list)))
    proc))
