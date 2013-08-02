;;; apply-annotate.lisp --- mark genome elements with perf annotations

;; Copyright (C) 2013  Eric Schulte

;;; Commentary:

;; Use annotations to bias mutation operations towards annotated
;; portions of the genome.

;;; Code:
(in-package :optimize)

;; Weight mutation location selection using the annotations, and
;; maintain annotation over mutations
(defmethod pick-bad ((asm simple)) (pick asm [{+ 0.01} {aget :annotation}]))

(defmethod apply-mutation :around ((asm asm) op)
  (call-next-method)
  (with-slots (genome) asm
    (flet ((blend (i)
             (setf (cdr (assoc :annotation (nth i genome)))
                   (mean (remove nil
                           (list (when (> i 0)
                                   (aget :annotation (nth (1- i) genome)))
                                 (aget :annotation (nth (1+ i) genome))))))))
      (case (car op)
        (:insert (blend (second op)))
        (:swap (blend (second op)) (blend (third op))))))
  asm)
