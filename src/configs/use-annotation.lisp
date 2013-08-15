;;; apply-annotate.lisp --- mark genome elements with perf annotations

;; Copyright (C) 2013  Eric Schulte

;;; Commentary:

;; Use annotations to bias mutation operations towards annotated
;; portions of the genome.

;;; Code:
(in-package :optimize)

;; Weight mutation location selection using the annotations, and
;; maintain annotation over mutations
(defmethod pick-bad ((asm simple))    (pick asm [{+ 0.01} {aget :annotation}]))
(defmethod pick-bad ((asm ann-range))
  (proportional-pick (annotations asm) #'identity))

(defvar *mutation-hooks* nil)

(defgeneric smooth-annotation (asm op)
  (:documentation "Smooth the annotations around a new mutation."))

(defmethod smooth-annotation ((asm asm) op)
  (with-slots (genome) asm
    (flet ((blend (i)
             (setf (cdr (assoc :annotation (nth i genome)))
                   (mean (remove nil
                           (list (when (> i 0)
                                   (aget :annotation (nth (1- i) genome)))
                                 (aget :annotation (nth i genome))))))))
      (case (car op)
        (:insert (blend (second op)))
        (:swap (blend (second op)) (blend (third op)))))))

(defmethod smooth-annotation ((asm ann-range) op)
  (let ((op (first op))
        (s1 (second op)))
    (with-slots (anns) asm
      (setf anns
            (case op
              (:cut    (software-evolution::range-cut    anns s1))
              ;; the value of the annotation should be that originally there
              (:insert (software-evolution::range-insert
                        anns s1 (software-evolution::range-nth s1 anns)))
              ;; swap lines but keep annotations, i.e., do nothing
              (:swap anns))))))

(defmethod apply-mutation :around ((asm asm) op)
  (call-next-method)
  (smooth-annotation asm op)
  (mapc {funcall _ asm op} *mutation-hooks*)
  asm)
