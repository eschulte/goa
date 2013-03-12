;;; neutral.lisp --- generate neutral versions of blackscholes
(require :software-evolution)
(in-package :software-evolution)
(mapc (lambda (pkg) (require pkg) (use-package pkg))
      (list :cl-ppcre :curry-compose-reader-macros))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))

(defvar infinity
  #+sbcl
  SB-EXT:DOUBLE-FLOAT-POSITIVE-INFINITY
  #-(or sbcl)
  (error "must specify a positive infinity value"))

(defvar *max-ind-evals* 4
  "Maximum number of times to evaluate an individual in graphite.
This would be 1 but some graphite metrics are non-deterministic, so we
take a mean.")

(defvar *work-dir* "sh-runner/work/")

(defvar *num-tests* 5 "Number of tests in `*test*'.")

(defvar *script* "../../bin/host-test"
  "Script used to evaluate variants.
Note: This does not follow the normal test script format but rather it;
1. takes the path to a .s asm file
2. copies that file to a VM
3. runs the resulting program in Graphite in the VM
4. returns the full set of Graphite debug information")

(defclass graphite-asm (asm)
  ((stats  :initarg :stats  :accessor stats  :initform nil)
   (broken :initarg :broken :accessor broken :initform nil)))

(defmethod copy ((graphite-asm graphite-asm)
                 &key (edits (copy-tree (edits graphite-asm)))
                   (fitness (fitness graphite-asm))
                   (stats (stats graphite-asm)))
  (make-instance (type-of graphite-asm)
    :edits edits
    :fitness fitness
    :addr-map (copy-tree (addr-map graphite-asm))
    :genome (copy-tree (genome graphite-asm))
    :linker (linker graphite-asm)
    :flags (flags graphite-asm)
    :stats stats))

(defvar *orig* (from-file (make-instance 'graphite-asm)
                          "data/blackscholes.m4.s"))

(defun parse-stdout (stdout)
  "Parse the Graphite output of host-test."
  (remove
      nil
      (mapcar
       (lambda (line)
         (let ((fields (split-sequence #\Space (regex-replace-all "" line "")
                                       :remove-empty-subseqs t)))
           (unless (null fields)
             (cons (make-keyword (string-upcase (car fields)))
                   (mapcar (lambda (c) (or (ignore-errors (parse-number c)) c))
                           (cdr fields))))))
       (remove-if (lambda (line) (or (scan "hooks" line)
                                (scan "warning" line)
                                (scan "spawn_master" line)))
                  (split-sequence #\Newline stdout :remove-empty-subseqs t)))))

(defun group-stats (stats &aux group results)
  (dolist (row stats (reverse (cons (reverse group) results)))
    (case (car row)
      ((:tile-summary :core-model-summary :network-summary :cache-summary)
       (setf group nil))
      ((:cache-l1-i :cache-l2 :cache-l1-d :dram-performance-model-summary)
       (when group (push (reverse group) results))
       (setf group row))
      (:network-model
       (when group
         (push (reverse group) results)
         (setf group nil))
       (push row group))
      (t
       (if group
           (push row group)
           (push row results))))))

(defun energy-delay-product (stats)
  (flet ((energy (group)
           (+ (reduce #'+ (cdr (assoc :static-power group)))
              (reduce #'+ (cdr (assoc :dynamic-energy group))))))
    (*
     ;; Runtime
     (reduce #'+ (aget :completion-time stats))
     ;; Energy
     (reduce #'+ (mapcar #'energy
                         (cons
                          (aget '(:network-model 2) stats :test #'tree-equal)
                          (mapcar {aget _ stats}
                                  '(:cache-l1-i :cache-l1-d :cache-l2
                                    :dram-performance-model-summary))))))))

(defun graphite-run (variant)
  (with-temp-file-of (asm "s") (genome-string variant)
    (incf *fitness-evals*)
    (multiple-value-bind (stdout stderr errno)
        (shell "~a blackscholes asm ~a" *script* asm)
      (declare (ignorable stderr))
      (values (zerop errno)
              (when (zerop errno) (group-stats (parse-stdout stdout)))))))

(defun test (variant)
  (when (and (not (broken variant))
             (< (length (stats variant)) *max-ind-evals*))
    (multiple-value-bind (neutral stats) (graphite-run variant)
        (if neutral
            (push stats (stats variant))
            (setf (broken variant) t))))
  (if (broken variant)
      infinity
      (mean (mapcar #'energy-delay-product (stats variant)))))

(defun quick-test (variant)
  (let ((*max-ind-evals* 1)) (test variant)))

(defvar *mutate-chance* 0.2
  "Chance that each new individual will be mutated.")

(setf ;; Evolutionary parameters
 *max-population-size* (expt 2 8)
 *tournament-size*     2
 *fitness-predicate*   #'<
 *mutate-chance*       0.2
 *cross-chance*        0.2)

(defun tourny (&optional (predicate *fitness-predicate*) (key #'test))
  "Select an individual from *POPULATION* with a tournament of size NUMBER."
  (assert *population* (*population*) "Empty population.")
  (extremum (repeatedly *tournament-size* (random-elt *population*))
            predicate :key key))

(defun mutant ()
  "Generate a new mutant from a *POPULATION*."
  (let ((copy (copy (tourny) :stats nil)))
    (if (< (random 1.0) *mutate-chance*)
        (mutate copy)
        copy)))

(defun crossed ()
  "Generate a new individual from *POPULATION* using crossover."
  (crossover (copy (tourny) :stats nil) (copy (tourny) :stats nil)))

(defun new-individual ()
  "Generate a new individual from *POPULATION*."
  (if (< (random 1.0) *cross-chance*) (crossed) (mutant)))

(defun evolver ()
  (loop :while *running* :do
     (push (new-individual) *population*)
     (loop :while (> (length *population*) *max-population-size*) :do
        ;; (tourny :predicate #'> :key #'quick-test)
        (let ((loser (nth (random (length *population*)) *population*)))
          (setf *population* (remove loser *population* :count 1))))))

#+run
(progn
  (setf *population* (repeatedly *max-population-size* (copy *orig*)))
  (loop :for i :upto 24 :do
     (sb-thread:make-thread #'evolver :name (format nil "opt-~d" i))))

(defun opt-threads ()
  (remove-if-not (lambda (thread)
                   (string= "opt" (subseq (sb-thread:thread-name thread) 0 3)))
                 (sb-thread:list-all-threads)))

;;; Notes:
;; 2013-03-11 17:00 ----- started run
;; 2013-03-12 10:15 ----- doubled population size

;;; Observations:
;;
;; at 2013-03-12 10:15
;;
;; (mapcar [#'length #'stats] *population*)
;; (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
;;  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
;;  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
;;  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0
;;  0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
;;  0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 0 4 0 1 0 0 0 1 1 0 0 2 1 2 2 2 0 0 0 0
;;  2 2 4 0 1 4 0 0 4 3 4 3 1 1 2 2 4 4 4 4 4 5)
;; (mapcar [#'length #'edits] *population*)
;; (3 4 4 2 3 4 1 3 2 3 2 1 1 3 7 3 1 2 6 1 2 3 1 3 2 1 3 2 1 7 1 3 4 3 3 3 2 4 2
;;  2 1 5 1 3 2 1 3 2 6 1 1 3 1 1 2 1 3 4 2 3 6 3 5 2 1 3 0 1 4 1 3 1 1 3 3 3 2 2
;;  4 4 2 1 1 2 2 5 1 3 2 2 6 6 3 2 2 3 2 3 5 4 1 6 2 7 2 6 2 1 2 2 1 4 2 2 2 4 6
;;  1 3 4 1 2 2 3 1 1 1 3 3 1 3 4 4 3 2 5 2 3 3 1 1 4 3 1 1 5 2 3 4 3 2 4 1 3 2 2
;;  2 2 2 1 4 3 4 2 3 4 4 4 2 3 1 4 4 4 3 3 3 1 3 1 4 3 1 3 1 2 3 1 3 2 4 2 1 3 5
;;  6 2 2 1 1 8 3 1 3 2 2 1 3 3 1 3 3 4 4 4 3 2 4 3 2 8 2 1 5 4 2 1 2 2 2 6 2 2 1
;;  4 1 6 3 4 3 5 1 3 2 3 4 4 3 6 2 2 3 3 3 1 2)
;;
;; at 2013-03-12 10:19
;;
;; - some of the virtual machines have large /tmp directories with lockfiles
;; - cleaned them up with the new bin/cond-clean script
;;
;; at 2013-03-12 10:48
;; 
;; (mapcar [#'energy-delay-product #'car #'stats]
;;         (remove-if [#'zerop #'length #'stats] *population*))
;; (1.2153501e9 1.2153254e9 1.2149624e9 1.2153268e9 1.2149618e9 1.2150474e9
;;  1.2151401e9 1.2148028e9 1.2153265e9 1.2153262e9 1.2153272e9 1.215321e9
;;  1.215327e9 1.215048e9 1.2150523e9 1.2153265e9 1.2150486e9 1.2150486e9
;;  1.2149617e9 1.2153274e9 1.2149624e9 1.2150776e9 1.2148165e9)
;; (apply #'min (mapcar [#'energy-delay-product #'car #'stats]
;;                      (remove-if [#'zerop #'length #'stats] *population*)))
;; 1.2148028e9
;; (energy-delay-product (car (stats *orig*)))
;; 1.2153262e9
;; 
;; Need more exploitation
;;
;; at 2013-03-12 10:54
;;
;; - setting *tournament-size* to 4
;; - pushed a couple of *orig*'s into *population*
