;;; objread.lisp --- read and display a software object

;; Copyright (C) 2013  Eric Schulte

;;; Code:
(in-package :goa)

(defun objread (args)
  (in-package :goa)
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

      ;; only want this when running objread from the command line
      (let ((*error-output* (make-broadcast-stream)))
        (defmethod slot-missing (class obj name op &optional new)
          (format *error-output* "; slot-missing: (~s ~s)~%"
                  name (class-name class))))

      (let* ((path (arg-pop))
             (exe (make-pathname :directory (pathname-directory path)
                                 :name (pathname-name path)))
             (obj (restore path)))
        ;; default behavior should be to compile an executable
        (when (null args)
          (push exe args)
          (push "-l" args))
        (getopts
         ("-l" "--link" 
               (multiple-value-bind (output errno)
                   (phenome obj :bin (or (arg-pop) exe))
                 (format t "~a~&" output)
                 (quit errno)))
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
