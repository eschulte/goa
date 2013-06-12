(load "src/optimize.lisp")
(in-package :optimize)

(defvar *help* "Usage: ~a object.store [OPTIONS...]
 manipulate a stored software object

Options:
 -h,--help ------------- print this help message and exit
 -l,--link FILE -------- link an executable to FILE
 -e,--edits ------------ write the edits to STDOUT
 -s,--stats ------------ write the stats to STDOUT
 -g,--genome ----------- write the genome to STDOUT
 -E,--eval LISP -------- eval LISP with `obj' bound~%")

(defun main (args)
  (in-package :optimize)
  (flet ((arg-pop () (pop args)))
    (let ((bin-path (arg-pop)))
      (when (or (not args)
                (string= (subseq (car args) 0 2) "-h")
                (string= (subseq (car args) 0 3) "--h"))
        (format t *help* bin-path)
        (error-out))

      (let ((best (restore (arg-pop))))
        (getopts
         ("-l" "--link"   (phenome best :bin (arg-pop)))
         ("-e" "--edits"  (format t "~&~S~%" (edits best)))
         ("-s" "--stats"
               (mapc (lambda-bind ((counter . count))
                       (format t "~a ~a~%"
                               (string-downcase (symbol-name counter))
                               count))
                     (stats best)))
         ("-g" "--genome" (format t "~&~S~%" (genome best)))
         ("-E" "--eval"
               (let ((form `(lambda (obj) ,(read-from-string (arg-pop)))))
                 (format t "~&~S~%" (funcall (eval form) best)))))))))
