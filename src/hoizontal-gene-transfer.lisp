;;; horizontal-gene-transfer.lisp --- some crossover alternates

;; Copyright (C) 2013  Eric Schulte

;;; Commentary:

;; Use perf annotate to label the assembly instructions with the HW
;; counters to which they contribute.

;;; Code:
(in-package :optimize)


;;; Gene transfer
;; Insert a subset of b into a copy of a.
;;
;;      b      (copy a)
;;      |         |
;;      +--->---\ |
;;      |        X+
;;      +--->---/ |
;;      |         |
;;
(defgeneric gene-transfer (a b)
  (:documentation "Like `crossover' but w/o fixed points across both parents."))

(defmethod gene-transfer ((a asm) (b asm))
  (let ((new (copy a))
        (piece (sort (loop :for i :below 2 :collect (random (size b))) #'<))
        (insertion (random (size a))))
    (setf (genome new)
          (copy-tree (append (subseq (genome a) 0 insertion)
                             (subseq (genome b)
                                     (first piece)
                                     (second piece))
                             (subseq (genome a) insertion))))
    (values new (cons insertion piece))))


;;; Synapsis-crossover
;; Variable length crossover targeting similar instructions between genomes
(defgeneric synapsis-crossover (a b)
  (:documentation
   "Variable length crossover with between-genome pairing through synapsis."))

(defun string-similar (string1 string2)
  "Return the similarity between STRING1 and STRING2 as a number from 0 to 1."
  (reduce (lambda-bind (acc (a b)) (if (equal a b) (1+ acc) acc))
          (mapcar #'list (coerce string1 'list) (coerce string2 'list))
          :initial-value 0))

(defun line-similar (line1 line2)
  "Return the similarity between LINE1 and LINE2 as a number from 0 to 1."
  (let ((splits (mapcar [{remove ""} {split "[ \\t]"}] (list line1 line2))))
    ((lambda (a b) (if (zerop b) 0 (/ a b)))
     (reduce #'+ (apply {mapcar #'string-similar} splits))
     (reduce #'+ (apply {mapcar #'min} (mapcar {mapcar #'length} splits))))))

(defmethod synapsis-crossover ((a asm) (b asm))
  (let ((new (copy a))
        (piece (sort (loop :for i :below 2 :collect (random (size b))) #'<))
        ;; random direction of scan
        (step (if (zerop (random 2)) -1 1))
        ;; random start of scan
        (index (random (size a))))
    (labels ((line-at (ind asm) (aget :line (nth ind (genome asm))))
             (scan-towards (target)
               (loop :until (or (not (and (>= index 0)
                                          (< index (length (genome a)))))
                                (< (random 1.0)
                                   (line-similar target (line-at index a))))
                  :do (incf index step))
               index))
      (let* ((in-start (scan-towards (line-at (first piece) b)))
             (in-end (progn (setf step 1)
                            (scan-towards (line-at (second piece) b)))))
        (setf (genome new)
              (copy-tree
               (append (subseq (genome a) 0 in-start)
                       (subseq (genome b) (first piece) (second piece))
                       (subseq (genome a) in-end))))
        (values new (list piece (list in-start in-end)))))))
