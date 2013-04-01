(mapcar #'require '(:software-evolution :memoize :cl-store))
(defpackage :bs-opt
  (:use :common-lisp :alexandria :metabang-bind :curry-compose-reader-macros
        :software-evolution :software-evolution-utility
        :split-sequence :memoize :cl-store :elf :cl-ppcre)
  (:shadow :type :magic-number))
(in-package :bs-opt)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))

(defclass asm-perf (asm)
  ((stats :initarg :stats :accessor stats :initform nil)))

(defvar infinity
  #+sbcl
  SB-EXT:DOUBLE-FLOAT-POSITIVE-INFINITY
  #-(or sbcl)
  (error "must specify a positive infinity value"))

;; do one of these depending on the perf version on your machine
;; (push :old-perf *features*)
;; (push :new-perf *features*)

(defvar *test-fmt*
  #+new-perf "../../bin/bs-test ~a -n 1 -t 12000 -r -p"
  #+old-perf "./bin/bs-test ~a -n 1 -t 12000 -r -p -b"
  "Script used to evaluate variants.
Take the path to a blackscholes executable, and returns the difference
between it's output and the oracle output.")

(defvar *orig* (from-file (make-instance 'asm-perf :linker "g++")
                          "data/blackscholes/asms/bs-g++-O0.s"))

(defvar *output-size* 83096.7
  "Sum of all output from a correct run of the original.")

(defvar *max-err* (/ *output-size* (expt 10 3))
  "Maximum error allowed, 3 orders of magnitude below total output.")

#+new-perf
(defun parse-stdout (stdout)
  (mapcar (lambda-bind ((val key))
            (cons (make-keyword (string-upcase key))
                  (or (ignore-errors (parse-number val))
                      infinity)))
          (mapcar {split-sequence #\,}
                  (cdr (split-sequence #\Newline
                                       (regex-replace-all ":HG" stdout "")
                                       :remove-empty-subseqs t)))))

#+old-perf
(defun parse-stdout (stdout)
  (remove nil
    (mapcar (lambda (line)
              (register-groups-bind (val key) (" +([0-9.-e]+) +([^ ]+)" line)
                (cons (make-keyword (string-upcase key))
                      (or (ignore-errors (parse-number val)) infinity))))
            (remove-if (lambda (line) (scan "Performance counter" line))
                       (split-sequence #\Newline stdout
                                       :remove-empty-subseqs t)))))

(defun test (asm)
  (with-temp-file (bin)
    (phenome asm :bin bin)
    (multiple-value-bind (stdout stderr errno) (shell *test-fmt* bin)
      (declare (ignorable stderr))
      (cons `(:exit . ,errno)
            (or (ignore-errors (parse-stdout stdout))
                `((:error . ,infinity)))))))

(defun neutralp (asm)
  (when-let ((err (cdr (assoc :error (stats asm)))))
    (and (zerop (cdr (assoc :exit (stats asm))))
         (numberp err)
         (< err *max-err*))))

(defun multi-obj (asm)
  (unless (stats asm) (setf (stats asm) (test asm)))
  (or (ignore-errors
        (when (and (neutralp asm)
                   (aget :instructions (stats asm))
                   (aget :error (stats asm)))
          (let ((err (aget :error (stats asm))))
            (+ (* (aget :instructions (stats asm))
                  (+ 1 (/ err *max-err*)))
               (length (genome asm))))))
      infinity))


#+run-neut
(progn
(defvar *neutral* nil)
(loop :for step :upto 10 :do
   (let ((prev (copy-tree *neutral*)))
     (setf *neutral* nil)
     (loop :until (>= (length *neutral*) 100) :as i :from 0 :do
        (let ((new (copy (random-elt prev))))
          (mutate new)
          (setf (fitness new) (test new))
          (format t "~S/~S edits:~S error:~S~%"
                  i (length *neutral*) (edits new) (aget 'error (fitness new)))
          (when (neutralp new) (push new *neutral*))))
     (store *neutral* (format nil "results/bs-neut/~d.store" step))))
)


#+analysis
(progn
(setf *neutral* (loop :for step :from 1 :to 10 :collect
                   (restore (format nil "results/bs-neut/~d.store" step))))

;; write out to a txt file
(with-open-file (out "results/bs-neut.data" :direction :output)
  (loop :for step :from 1 :upto 10 :collect
     (mapcar [{format out "~a ~{~a~^ ~}~%" step} {mapcar #'cdr} #'fitness]
             (restore (format nil "results/bs-neut/~d.store" step)))))

(mapcar [{aget 'stalled-cycles-frontend} #'fitness] (car *neutral*))

(defun range (sample) (abs (- (apply #'max sample) (apply #'min sample))))

(defvar *metrics*
  (loop :for metric :in (mapcar #'car (fitness *orig*)) :collect
     (let ((vals (remove nil
                   (mapcan {mapcar [{aget metric} #'fitness]} *neutral*))))
       (cons metric
             (mapcar #'float (list (aget metric (fitness *orig*))
                                   (mean vals)
                                   (standard-deviation vals)
                                   (range vals)))))))

(defvar *dev-by-step*
  (loop :for step :below 10 :collect
     (let ((fits (mapcar #'fitness (nth step *neutral*))))
       (list (+ 1 step)
             (reduce #'+ (mapcar (lambda (metric)
                                   (let ((vals (mapcar {aget metric} fits)))
                                     (if (zerop (reduce #'+ vals))
                                         0
                                         (let ((std  (standard-deviation vals))
                                               (mean (mean vals)))
                                           (/ std mean)))))
                                 (mapcar #'car (fitness *orig*)))))))
  "Total Deviation by step away from the original.")
)


;;; Artificial Selection
;; see blackscholes-w-graphite.lisp for eviction and other pop tricks
#+run
(progn
(defvar *base* "results/bs-evo" "Where to store incremental results.")

(setf *work-dir* "sh-runner/work/")

(setf
 (fitness *orig*) (multi-obj *orig*)
 *max-population-size* (expt 2 7)
 *tournament-size* 4
 *fitness-predicate* #'<
 *population* (loop :for n :upto *max-population-size* :collect (copy *orig*)))

(loop :for i :from 1 :to 7 :do
   (sb-thread:make-thread
    (lambda ()
      (evolve
       #'multi-obj
       :filter (lambda (var) (< (fitness var) (* 10 (fitness *orig*))))
       :period (expt 2 12)
       :period-func
       (lambda ()
         (sb-ext:gc :force t)
         (store *population* 
                (format nil "~a/~d-pop.store" *base* *fitness-evals*)))))
    :name (format nil "opt-~d" i)))
)
