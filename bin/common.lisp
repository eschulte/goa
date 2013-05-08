(load "src/optimize.lisp")
(in-package :optimize)

(defun arg-pop () (pop sb-ext:*posix-argv*))
(arg-pop) ;; pop SBCL off the argument list

(setf *note-level* 1)

(defun throw-error (&rest args)
  (apply #'note 0 args)
  (sb-ext:exit :code 1))

(defmacro getopts (&rest forms)
  (let ((arg (gensym)))
    `(loop :for ,arg = (pop sb-ext:*posix-argv*) :while ,arg :do
        (cond
          ,@(mapcar (lambda-bind ((short long . body))
                      `((or (string= ,arg ,short) (string= ,arg ,long)) ,@body))
                    forms)))))
