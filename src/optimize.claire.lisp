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
(defvar *psize* 100 "Population size.") ;; CLG: --popsize 100
(defvar *tsize* 2   "Tournament size.") ;; CLG: --tournament-size 2
(defvar *script* "./host-test.sh" ;; CLG: --test-script ./host-test.sh
  "Script used to evaluate variants.
Note: This does not follow the normal test script format but rather it;
1. takes the path to a .s asm file
2. copies that file to a VM
3. runs the resulting program in Graphite in the VM
4. returns the full set of Graphite debug information")
;; CLG: you will also need --test-command __TEST_SCRIPT__ __SOURCE_NAME__
;; __FITNESS_FILE__ 
;; and have host-test.sh put the graphite debug info into __FITNESS_FILE__
;; you're welcome to add different command keywords beyond __EXE_NAME__ etc to
;; rep.ml so long as they're consistent and not redundant.
;; you reference guest-test.sh in your commentary for optimize.lisp, but I don't
;; see it in the actual code so I'm not sure where it fits in/if repair needs to
;; know about it.

(defclass pll-asm (asm)
  ((time-wo-init :accessor time-wo-init :initform nil)
   (time-w-init :accessor time-w-init :initform nil)
   (trans-time :accessor trans-time :initform nil)
   (total-packets-sent :accessor total-packets-sent :initform nil)
   (total-flits-sent :accessor total-flits-sent :initform nil)
   (total-bytes-sent :accessor total-bytes-sent :initform nil)
   (total-packets-broadcasted :accessor total-packets-broadcasted :initform nil)
   (total-flits-broadcasted :accessor total-flits-broadcasted :initform nil)
   (total-bytes-broadcasted :accessor total-bytes-broadcasted :initform nil)
   (total-packets-received :accessor total-packets-received :initform nil)
   (total-flits-received :accessor total-flits-received :initform nil)
   (total-bytes-received :accessor total-bytes-received :initform nil)
   (average-packet-latency-in-clock-cycles :accessor average-packet-latency-in-clock-cycles :initform nil)
   (average-packet-latency-in-ns :accessor average-packet-latency-in-ns :initform nil)
   (average-contention-delay-in-clock-cycles :accessor average-contention-delay-in-clock-cycles :initform nil)
   (average-contention-delay-in-ns :accessor average-contention-delay-in-ns :initform nil)
   (switch-allocator-traversals :accessor switch-allocator-traversals :initform nil)
   (crossbar-traversals :accessor crossbar-traversals :initform nil)
   (link-traversals :accessor link-traversals :initform nil))
  (:documentation
   "Extending the ASM class with a number of parallel run statistics."))

(defun pll-from-asm (asm)
  (make-instance 'pll-asm :genome  (copy (genome asm))))

(defvar *orig* (pll-from-asm (asm-from-file "../data/fft.s"))
  "Original seed program.") ;; --fault-scheme uniform --fix-scheme uniform

(defun pll-to-s (var)
  "Write VAR to a temporary .s file."
  (let ((tmp (temp-file-name "s"))) (asm-to-file var tmp) tmp))

;; CLG: OK this is slightly different from the code on your website.
;; Referencing your comments, however, it looks like you call the ./host-test.sh
;; command or something and store the result in the rep, which is then processed
;; by the individuals to get the fitness.
;;
;; Given the appropriate commands (above), you can replicate this by overriding
;; the method internal_test_case_postprocess status fitness_file to process the
;; fitness_file to which the output is saved to do whatever you want to the
;; fitness.  internal_test_case_postprocess returns an array of floating point
;; values corresponding to the different possible dimensions along which fitness
;; is evaluated.  all the output-to-stats and related code in the code on your
;; website then go in internal_test_case_postprocess in your representation
;; subclass.
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
                    (unless (numberp val)
                      (format t "~&non-numeric value ~a:~a" key val)
                      (setf (fitness var) 0))
                    (setf (slot-value var key) val)))))
            (split-sequence #\Newline out)))
  var)

(defmethod evaluate-network ((var pll-asm))
  (multiple-value-bind (out err exit) (shell "~a ~a" *script* (pll-to-s var))
    (declare (ignorable err))
    (setf (fitness var) (if (= exit 0) 1 0))
    (mapcar (lambda (line)
              (when (> (length line) 0)
                (let* ((pair (split-sequence #\Space line
                                             :remove-empty-subseqs t))
                       (key  (read-from-string (car pair)))
                       (vals (mapcar #'read-from-string (cdr pair))))
                  (when (and key (slot-exists-p var key))
                    (setf (slot-value var key) vals)))))
            (split-sequence #\Newline out))))

(defun stats (var)
  "Return an alist of the vital stats of VAR."
  (mapcar (lambda (stat) `(,stat . ,(slot-value var stat)))
          '(time-wo-init history)))

(defun file-for-run (n)
  (let ((file (format nil *file-format* n)))
    (if *dir* (merge-pathnames file *dir*) file)))

(defun safe< (a b)
  "A version of < which gives the right values in the case of non-numbers."
  (if (numberp a)
      (if (numberp b) (< a b) T)
      nil))

;; Your comments on your website suggest that there is "one test", and I'm not
;; sure how that jives with your multiple-fitness-keys thing.
;; Regardless, tournament selection is implemented on populations so that's
;; already possible.
;; The test and key are the aspects that can now be replicated given the change
;; I just made in whatever "biased search" you implement. 
(defun take-biased-step (pop &key (test #'safe<) (key #'time-wo-init) &aux result)
  "Take a whole-population biased step through neutral space."
  (flet ((new-var ()
           (let ((t-pop (repeatedly *tsize* (random-elt pop))))
             (evaluate (mutate (copy (first (sort t-pop test :key key))))))))
    (loop :until (>= (length result) *psize*) :do
       (let ((pool (prepeatedly (floor (* (- *psize* (length result)) 1.3333))
                     (new-var))))
         (dolist (var pool) (when (= (fitness var) 1) (push var result)))))
    (subseq result 0 *psize*)))

(defun do-biased-walk (seed &key (steps 100) (test #'safe<) (key #'time-wo-init))
  "Evolve a population in the neutral space biased by metric and KEY."
  (setf *pop* (list seed))
  (dotimes (n steps)
    (store (mapcar #'stats *pop*) (file-for-run n))
    (setf *pop* (take-biased-step *pop* :test test :key key))))

#+run
(do-biased-walk *orig*)
