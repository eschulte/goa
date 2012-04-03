;;; optimize.lisp --- optimize metrics in a population of software variants

;; Copyright (C) 2012  Eric Schulte

;;; Commentary:

;; Starting with an initial software object, generate a population of
;; variant implementations and then evolve to optimize some metric
;; such as fastest execution, least communication, lowest energy
;; consumption etc...

;;; Code:
(require :software-evolution)
(in-package :software-evolution)

(defvar *pop*   nil "Population of variants.")
(defvar *dir*   nil "Optional sub-directory in which to store results.")
(defvar *psize* 100 "Population size.")
(defvar *tsize* 2   "Tournament size.")
(defvar *script* "./host-test.sh"
  "Script used to evaluate variants.
Note: This does not follow the normal test script format but rather it;
1. takes the path to a .s asm file
2. copies that file to a VM
3. runs the resulting program in Graphite in the VM
4. returns the full set of Graphite debug information")

(defclass pll-asm (asm)
  ((time-wo-init :accessor time-wo-init :initform nil)
   (time-w-init  :accessor time-w-init  :initform nil)
   (trans-time   :accessor trans-time   :initform nil))
  (:documentation
   "Extending the ASM class with a number of parallel run statistics."))

(defun pll-from-asm (asm)
  (make-instance 'pll-asm :genome  (copy (genome asm))))

(defvar *orig* (pll-from-asm (asm-from-file "../data/fft.s"))
  "Original seed program.")

(defmethod evaluate ((var pll-asm))
  "Run parallel program VAR collecting and saving fitness and all metrics."
  (let ((tmp (temp-file-name "s")))
    (asm-to-file var tmp)
    (multiple-value-bind (output err-output exit)
        (shell "~a ~a" *script* tmp)
      (declare (ignorable err-output))
      (setf (fitness var) (if (= exit 0) 1 0))
      (mapcar (lambda (line)
                (when (> (length line) 0)
                  (let* ((pair (split-sequence #\Space line))
                         (key  (read-from-string (car pair)))
                         (val  (read-from-string (cadr pair))))
                    (when (and key (slot-exists-p var key))
                      (setf (slot-value var key) val)))))
              (split-sequence #\Newline output)))
    (fitness var)))

(defun stats (var)
  "Return an alist of the vital stats of VAR."
  (mapcar (lambda (stat) `(,stat . ,(slot-value stat var)))
          '(:time-wo-init :history)))

(defun take-biased-step (pop &key (test #'<) (key #'time-wo-init))
  "Take a whole-population biased step through neutral space."
  (flet ((pick ()
           (first (sort (repeatedly *tsize* (random-elt pop)) test :key key))))
    (loop :for var = (copy (pick)) :until (= *psize* 0)
       :do (mutate var) (evaluate var)
       :if (= (fitness var) 1) :collect (progn (decf *psize*) var))))

(defun do-biased-walk (seed &key (steps 100) (test #'<) (key #'time-wo-init))
  "Evolve a population in the neutral space biased by metric and KEY."
  (setf *pop* (list seed))
  (dotimes (n steps)
    (store (mapcar #'stats *pop*)
           (let ((file (format nil "biased-pop-~S.store" n)))
             (if *dir* (merge-pathnames file dir) file)))
    (setf *pop* (take-biased-step *pop* :test test :key key))))

#+run
(let ((*psize* 2)) (do-biased-walk *orig* :steps 2))
