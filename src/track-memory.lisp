;;; track-memory.lisp --- augment `checkpoint' to print incremental memory usage

;;; Commentary:

;; Load this file with the -c option to optimize to add memory usage
;; debug information to the checkpoint function.

;;; Code:
(require :optimize)

(let ((original-checkpoint #'checkpoint))
  (defun checkpoint ()
    (flet ((space ()
             (let ((*standard-output* *note-out*))
               (room)
               (format *note-out* "~S~%"
                       (list *fitness-evals*
                             (length (to-bytes *consolidated-edits*))
                             (length (to-bytes (car *population*))))))))
      (space) (funcall original-checkpoint) (space))))
