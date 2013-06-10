;;; track-memory.lisp --- augment `checkpoint' to print incremental memory usage

;;; Commentary:

;; Load this file with the -c option to optimize to add memory usage
;; debug information to the checkpoint function.

;;; Code:
(in-package :optimize)

(defun memory-checkpoint ()
  (with-open-file
      (out (make-pathname :directory *res-dir* :name "memory" :type "lisp")
           :direction :output
           :if-exists :append
           :if-does-not-exist :create)
    (format out "~&~S~%"
            (list *fitness-evals*
                  (sb-vm::DYNAMIC-USAGE)
                  (sb-vm::type-breakdown :dynamic)))
    (handler-case
        (let ((*standard-output* out))
          (format *standard-output* "~&#|~%")
          (sb-vm::memory-usage :print-spaces t :print-summary nil)
          (sb-vm:instance-usage :dynamic :top-n 200)
          (format *standard-output* "~&|#~%"))
      (error (e) (note 1 "track-memory error: ~S" e))))
  ;; when interactive, quit running if dynamic space usage is too high
  #+nil ;; this throws the same error as room
  (when (> (/ (sb-vm::DYNAMIC-USAGE) (sb-ext:dynamic-space-size)) 3/8)
    (note 1 "Dynamic space usage is ~a/~a ~ ~f, pausing run!~%"
          (sb-vm::DYNAMIC-USAGE) (sb-ext:dynamic-space-size)
          (/ (sb-vm::DYNAMIC-USAGE) (sb-ext:dynamic-space-size)))
    (setf *running* nil))
  (checkpoint))

(setf *checkpoint-func* #'memory-checkpoint)
