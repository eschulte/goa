;;; annotate.lisp --- annotate assembly LOC with perf

;; Copyright (C) 2013  Eric Schulte

;;; Commentary:

;; The following code assumes that the compiler flag has been saved
;; into the `flag' variable.
;;
;; Run with the optimize script with something like the following
;;
;;     optimize bzip2.s bzip2 -e "(defvar flag 'O0)" -c by-flag.lisp

;;; Code:
(in-package :optimize)

(defun asm-disassemble (bin func)
  (let ((raw (shell "gdb --batch --eval-command=\"disassemble ~a\" ~a"
                    func bin))
        (rx "[ \t]*0x([a-zA-Z0-9]+)[ \t]*<\\+[0-9]+>:.*"))
    (remove nil
      (mapcar (lambda (line)
                (multiple-value-bind (all matches) (scan-to-strings rx line)
                  (when all
                    (read-from-string (format nil "#x~a" (aref matches 0))))))
              (split-sequence #\Newline raw)))))

(defun perf-annotations (bin)
  (let ((raw (if (null *size*)
                 (shell "~a ~a ~a -a"       *script* *benchmark* bin)
                 (shell "~a ~a ~a -s ~a -a" *script* *benchmark* bin *size*)))
        (rx "([0-9\.]+) +:[ \\t]+([a-fA-F0-9]+):"))
    (remove nil
      (mapcar (lambda (line)
                (multiple-value-bind (all matches) (scan-to-strings rx line)
                  (when all
                    (cons (read-from-string (format nil "#x~a"
                                                    (aref matches 1)))
                          (parse-number (aref matches 0))))))
              (split-sequence #\Newline raw)))))

(defun genome-addrs (asm &key bin &aux func-addrs)
  (let ((my-bin (or bin (phenome asm))))
    (unwind-protect
         (mapcar
          (lambda (l)
            (multiple-value-bind (all matches)
                (scan-to-strings "^([^\\.][a-zA-Z0-9_]*):" (aget :line l))
              (if all
                  (prog1 nil
                    (setf func-addrs (asm-disassemble my-bin (aref matches 0))))
                  (when func-addrs (pop func-addrs)))))
          (genome asm))
      (when (not bin) (delete-file my-bin)))))

(defun genome-anns (asm &key bin)
  (let ((my-bin (or bin (phenome asm))))
    (unwind-protect
         (mapcar {aget _ (perf-annotations my-bin)}
                 (genome-addrs asm :bin my-bin))
      (when (not bin) (delete-file my-bin)))))

(defvar *kernel* '((-3 . 0.006) (-2 . 0.061) (-1 . 0.242)
                   (0 . 0.383)
                   (1 . 0.242)  (2 . 0.061) (3 . 0.006)))

(defun smooth (list &optional (kernel *kernel*))
  "Gaussian smoothing of LIST by KERNEL."
  (let ((result (make-array (length list) :initial-element 0)))
    (loop :for el :in list :as i :from 0 :do
       (loop :for (off . mult) :in kernel :do
          (let ((ind (+ i off)))
            (when (and (>= ind 0) (< ind (length list)))
              (incf (aref result ind) (* mult el))))))
    (coerce result 'list)))

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
(setf (genome *orig*)
      (mapcar (lambda (ann element)
                (cons (cons :annotation ann) element))
              (smooth (mapcar (lambda (ans) (or ans 0)) (genome-anns *orig*)))
              (genome *orig*)))
