(in-package :optimize)

(defun objread (&optional (args *arguments*))
  (in-package :optimize)
  (flet ((arg-pop () (pop args)))
    (let ((help "Usage: objread object.store [OPTIONS...]
 manipulate a stored software object

Options:
 -h,--help ------------- print this help message and exit
 -l,--link FILE -------- link an executable to FILE
 -e,--edits ------------ write the edits to STDOUT
 -s,--stats ------------ write the stats to STDOUT
 -g,--genome ----------- write the genome to STDOUT
 -G,--genome-string ---- write the genome string to STDOUT
 -E,--eval LISP -------- eval LISP with `obj' bound~%"))
      (when (or (not args)
                (string= (subseq (car args) 0 2) "-h")
                (string= (subseq (car args) 0 3) "--h"))
        (format t help) (quit))

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
         ("-G" "--genome-string"
               (format t "~&~a~%" (genome-string best)))
         ("-E" "--eval"
               (let ((form `(lambda (obj) ,(read-from-string (arg-pop)))))
                 (format t "~&~a~%" (funcall (eval form) best)))))))))
