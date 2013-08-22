;;; objread.lisp --- read and display a software object

;; Copyright (C) 2013  Eric Schulte

;;; Code:
(in-package :optimize)

(defun objread (args)
  (in-package :optimize)
  (flet ((arg-pop () (pop args)))
    (let ((help "Usage: ~a object.store [OPTIONS...]
 manipulate a stored software object

Options:
 -h,--help ------------- print this help message and exit
 -l,--link FILE -------- link an executable to FILE
 -s,--stats ------------ write the stats to STDOUT
 -f,--fitness ---------- write the fitness to STDOUT
 -a,--annotations ------ write the annotations to STDOUT
 -g,--genome ----------- write the genome to STDOUT
 -G,--genome-string ---- write the genome string to STDOUT
 -E,--eval LISP -------- eval LISP with `obj' bound~%")
          (self (pop args)))
      (when (or (not args)
                (string= (subseq (car args) 0 2) "-h")
                (string= (subseq (car args) 0 3) "--h"))
        (format t help self) (quit))

      (let ((obj (restore (arg-pop))))
        (getopts
         ("-l" "--link"   (phenome obj :bin (arg-pop)))
         ("-s" "--stats"
               (mapc (lambda-bind ((counter . count))
                       (format t "~a ~a~%"
                               (string-downcase (symbol-name counter))
                               count))
                     (stats obj)))
         ("-f" "--fitness" (format t "~&~a~%" (fitness obj)))
         ("-a" "--annotations"
               (format t "~{~{~a~^ ~}~^~%~}~%"
                       (indexed (annotations obj))))
         ("-g" "--genome" (format t "~&~S~%" (genome obj)))
         ("-G" "--genome-string"
               (format t "~&~a~%" (genome-string obj)))
         ("-E" "--eval"
               (let ((form `(lambda (obj) ,(read-from-string (arg-pop)))))
                 (format t "~&~a~%" (funcall (eval form) obj)))))))))
