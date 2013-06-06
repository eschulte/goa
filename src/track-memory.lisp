;;; track-memory.lisp --- augment `checkpoint' to print incremental memory usage

;;; Commentary:

;; Load this file with the -c option to optimize to add memory usage
;; debug information to the checkpoint function.

;;; Code:
(in-package :optimize)

(defun memory-checkpoint ()
  (with-open-file
      (out (make-pathname :directory *res-dir* :name "memory" :type "txt")
           :direction :output
           :if-exists :append
           :if-does-not-exist :create)
    (format out "~&~%;;----------------------------------------~%~S~%"
            (list *fitness-evals*
                  (length (to-bytes *consolidated-edits*))
                  (length (to-bytes (car *population*)))
                  (sb-vm::type-breakdown :dynamic))))
  (checkpoint))

(setf *checkpoint-func* #'memory-checkpoint)
