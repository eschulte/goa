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

#+lab-machine
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
  ((neutral-p      :accessor neutral-p      :initform nil)
   ;; execution stats
   (start          :accessor start          :initform nil)
   (init-finish    :accessor init-finish    :initform nil)
   (finish         :accessor finish         :initform nil)
   (trans-fraction :accessor trans-fraction :initform nil)
   (time-wo-init   :accessor time-wo-init   :initform nil)
   (time-w-init    :accessor time-w-init    :initform nil)
   (raw-output     :accessor raw-output     :initform nil)
   (trans-time     :accessor trans-time     :initform nil)
   ;; network stats
   (total-packets-sent :accessor total-packets-sent :initform nil)
   (total-flits-sent :accessor total-flits-sent :initform nil)
   (total-bytes-sent :accessor total-bytes-sent :initform nil)
   (total-packets-broadcasted :accessor total-packets-broadcasted :initform nil)
   (total-flits-broadcasted :accessor total-flits-broadcasted :initform nil)
   (total-bytes-broadcasted :accessor total-bytes-broadcasted :initform nil)
   (total-packets-received :accessor total-packets-received :initform nil)
   (total-flits-received :accessor total-flits-received :initform nil)
   (total-bytes-received :accessor total-bytes-received :initform nil)
   (average-packet-latency-in-clock-cycles
    :accessor average-packet-latency-in-clock-cycles :initform nil)
   (average-packet-latency-in-ns
    :accessor average-packet-latency-in-ns :initform nil)
   (average-contention-delay-in-clock-cycles
    :accessor average-contention-delay-in-clock-cycles :initform nil)
   (average-contention-delay-in-ns
    :accessor average-contention-delay-in-ns :initform nil)
   (switch-allocator-traversals
    :accessor switch-allocator-traversals :initform nil)
   (crossbar-traversals :accessor crossbar-traversals :initform nil)
   (link-traversals :accessor link-traversals :initform nil))
  (:documentation
   "Extending the ASM class with a number of parallel run statistics."))

(defun pll-from-asm (asm)
  (make-instance 'pll-asm :genome  (copy (genome asm))))

(defvar *orig* (pll-from-asm (asm-from-file "../data/fft.s"))
  "Original seed program.")

(defun pll-to-s (var)
  "Write VAR to a temporary .s file."
  (let ((tmp (temp-file-name "s"))) (asm-to-file var tmp) tmp))

(defun output-to-stats (output)
  (delete nil
   (mapcar
    (lambda (line)
      (when (> (length line) 0)
        (let* ((pair (split-sequence #\Space line :remove-empty-subseqs t))
               (key  (read-from-string (car pair)))
               (val  (mapcar #'read-from-string (cdr pair))))
          (cons key val))))
    (split-sequence #\Newline output :remove-empty-subseqs t))))

(defun apply-output (var output)
  (mapcar (lambda (pair)
            (let ((key (car pair)) (val (cdr pair)))
              (when (and key (slot-exists-p var key))
                (setf (slot-value var key)
                      (if (= (length val) 1) (first val) val)))))
          (output-to-stats output)))

(defmethod evaluate ((var pll-asm))
  "Run parallel program VAR collecting and saving neutrality and all metrics."
  (let ((s-file (pll-to-s var)))
    (handler-case
        (with-timeout (360)
          (multiple-value-bind (output err exit) (shell "~a ~a" *script* s-file)
            (declare (ignorable err))
            (delete-file s-file)
            (note 2 "$ ~a ~a; $? => ~d" *script* s-file exit)
            (setf (raw-output var) output)
            (apply-output var (raw-output var))
            (setf (neutral-p var) (= exit 0))
            var))
      (timeout-error (c)
        (declare (ignore c))
        var))))

(defun file-for-run (n)
  (let ((file (format nil *file-format* n)))
    (if *dir* (merge-pathnames file *dir*) file)))

(defun safe< (a b)
  "A version of < which gives the right values in the case of non-numbers."
  (if (numberp a) (if (numberp b) (< a b) T) nil))

(defun biased-step (pop &key (test #'safe<) (key #'time-wo-init) &aux result)
  "Take a whole-population biased step through neutral space."
  (flet ((new-var ()
           (let ((t-pop (repeatedly *tsize* (random-elt pop))))
             (evaluate (mutate (copy (first (sort t-pop test :key key))))))))
    (loop :until (>= (length result) *psize*) :do
       (let* ((to-run (min (thread-pool-size)
                           (floor (* (- *psize* (length result)) 3))))
              (pool (progn
                      (note 1 "~&generating ~a" to-run)
                      (prepeatedly to-run (progn (note 2 "starting")
                                                 (new-var))))))
         (note 1 "~&keeping the fit")
         (dolist (var pool) (when (neutral-p var) (push var result)))
         (note 1 "~&(length results) ;; => ~a" (length result))))
    (subseq result 0 *psize*)))

(defun biased-walk (seed &key (steps 100) (test #'safe<) (key #'time-wo-init))
  "Evolve a population in the neutral space biased by metric and KEY."
  (setf *pop* (list seed))
  (dotimes (n steps)
    (note 1 "saving population ~d" n)
    (store *pop* (file-for-run n))
    (note 1 "generating population ~d" (1+ n))
    (setf *pop* (biased-step *pop* :test test :key key))))

#+run
(biased-walk *orig*)
