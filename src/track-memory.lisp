;;; track-memory.lisp --- augment `checkpoint' to print incremental memory usage

;;; Commentary:

;; Load this file with the -c option to optimize to add memory usage
;; debug information to the checkpoint function.

;;; Code:
(in-package :optimize)

(defun memory-checkpoint ()
  (flet ((space ()
           (let ((*standard-output* *note-out*))
             (room)
             (format *note-out* "~S~%"
                     (list *fitness-evals*
                           (length (to-bytes *consolidated-edits*))
                           (length (to-bytes (car *population*))))))))
    (space) (checkpoint) (space)))

(setf *checkpoint-func* #'memory-checkpoint)
