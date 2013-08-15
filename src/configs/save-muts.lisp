;;; save-muts.lisp --- save mutations over the course of a run

;; Copyright (C) 2013  Eric Schulte

;; Mutations will be saved into a single large list which will be
;; written out to "muts.store" at the end of the run.

;;; Code:
(in-package :optimize)


;;; Save applied mutations
(unless (boundp '*mutation-hooks*)
  (defvar *mutation-hooks* nil)

  (defmethod apply-mutation :around ((asm asm) op)
             (call-next-method)
             (mapc {funcall _ asm op} *mutation-hooks*)))

(defvar *mutations* nil)

(defun mut-saver (asm op)
  (declare (ignorable asm))
  (push op *mutations*))

(push (lambda (asm op) (declare (ignorable asm)) (push op *mutations*))
      *mutation-hooks*)

(push (lambda () (store *mutations* (make-pathname :directory *res-dir*
                                              :name "muts"
                                              :type "store")))
      *final-funcs*)

#|


;;; Methods to save the picked indices
(defvar picked nil)

(defmethod pick-bad ((asm simple))
  (let ((id (pick asm [{+ 0.01} {aget :annotation}])))
    (push id picked) id))

(defmethod pick-bad ((asm ann-range))
  (let ((id (proportional-pick (annotations asm) #'identity)))
    (push id picked) id))

|#
