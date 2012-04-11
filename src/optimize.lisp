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

(advise-thread-pool-size 46)

(defvar *pop*   nil "Population of variants.")
(defvar *dir*   nil "Optional sub-directory in which to store results.")
(defvar *file-format* "biased-pop-~S.store" "File name format.")
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

(defun pll-to-s (var)
  "Write VAR to a temporary .s file."
  (let ((tmp (temp-file-name "s"))) (asm-to-file var tmp) tmp))

(defmethod evaluate ((var pll-asm))
  "Run parallel program VAR collecting and saving fitness and all metrics."
  (multiple-value-bind (out err exit) (shell "~a ~a" *script* (pll-to-s var))
    (declare (ignorable err))
    (setf (fitness var) (if (= exit 0) 1 0))
    (mapcar (lambda (line)
              (when (> (length line) 0)
                (let* ((pair (split-sequence #\Space line))
                       (key  (read-from-string (car pair)))
                       (val  (read-from-string (cadr pair))))
                  (when (and key (slot-exists-p var key))
                    (setf (slot-value var key) val)))))
            (split-sequence #\Newline out)))
  var)

(defun stats (var)
  "Return an alist of the vital stats of VAR."
  (mapcar (lambda (stat) `(,stat . ,(slot-value var stat)))
          '(time-wo-init history)))

(defun file-for-run (n)
  (let ((file (format nil *file-format* n)))
    (if *dir* (merge-pathnames file *dir*) file)))

(defun take-biased-step (pop &key (test #'<) (key #'time-wo-init) &aux result)
  "Take a whole-population biased step through neutral space."
  (flet ((new-var ()
           (let ((t-pop (repeatedly *tsize* (random-elt pop))))
             (evaluate (mutate (copy (first (sort t-pop test :key key))))))))
    (loop :until (= (length result) *psize*) :do
       (let ((pool (prepeatedly (- *psize* (length result)) (new-var))))
         (dolist (var pool) (when (= (fitness var) 1) (push var result)))))
    result))

(defun do-biased-walk (seed &key (steps 100) (test #'<) (key #'time-wo-init))
  "Evolve a population in the neutral space biased by metric and KEY."
  (setf *pop* (list seed))
  (dotimes (n steps)
    (store (mapcar #'stats *pop*) (file-for-run n))
    (setf *pop* (take-biased-step *pop* :test test :key key))))

#+run
(do-biased-walk *orig*)
