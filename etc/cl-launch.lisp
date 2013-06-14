#-quicklisp
(let ((quicklisp-init (merge-pathnames "lisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
