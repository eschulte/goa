;;; apply-annotate.lisp --- mark genome elements with perf annotations

;; Copyright (C) 2013  Eric Schulte

;;; Commentary:

;; Use perf annotate to label the assembly instructions with the HW
;; counters to which they contribute.  This file may be passed to the
;; --config option of the optimize script.

;;; Code:
(in-package :optimize)

;; Weight mutation location selection using the annotations, and
;; maintain annotation over mutations
(defmethod pick-bad ((asm simple)) (pick asm [{+ 0.01} {aget :annotation}]))

(defmethod mutate :around ((asm asm))
  (call-next-method)
  (let ((edit (car (edits asm))))
    (with-slots (genome) asm
      (flet ((blend (i)
               (setf (cdr (assoc :annotation (nth i genome)))
                     (mean (remove nil
                             (list (when (> i 0)
                                     (aget :annotation (nth (1- i) genome)))
                                   (aget :annotation (nth (1+ i) genome))))))))
        (case (car edit)
          (:insert (blend (second edit)))
          (:swap (blend (second edit)) (blend (third edit)))))))
  asm)

;; apply the perf annotations to the genome
(unless (aget :annotation (car (genome *orig*)))
  (mapcar (lambda (ann element)
            (cons (cons :annotation ann) element))
          (smooth (mapcar (lambda (ans) (or ans 0)) (genome-anns *orig*)))
          (genome *orig*))
  (store *orig* (format nil "~a/orig.store" (pathname-name *res-dir*))))
