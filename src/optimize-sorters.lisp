;;; neutral.lisp --- test sorters for neutrality across representations
(in-package :software-evolution)
(use-package :cl-ppcre)
(use-package :curry-compose-reader-macros)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))

(defvar infinity
  #+sbcl
  SB-EXT:DOUBLE-FLOAT-POSITIVE-INFINITY
  #-(or sbcl)
  (error "must specify a positive infinity value"))

(defvar *func-test* "../../bin/test-file.sh")

(defvar *opt-test* "../../bin/host-test")

(defvar *fitness-predicate* #'<)

(defvar *prog* "sort")

(defvar *orig* (from-file (make-instance 'cil) "data/bubble.c"))

(defvar *work-dir* "sh-runner/work/")

(setf *max-population-size* (expt 2 8))

(setf *tournament-size* 2)

(defun parse-stdout (stdout)
  (mapcar
   (lambda (line)
     (let ((fields (split-sequence #\Space line :remove-empty-subseqs t)))
       (cons (make-keyword (string-upcase (car fields)))
             (mapcar (lambda (c) (or (ignore-errors (parse-number c)) c))
                     (cdr fields)))))
   (split-sequence #\Newline stdout :remove-empty-subseqs t)))

(defun multi-obj-fitness (output)
  "Calculate the total combined fitness of VARIANT based on `evaluate' output."
  (if output
      (reduce #'+ (cdr (assoc :INSTRUCTIONS output)))
      infinity))

(defmethod evaluate ((variant cil))
  (or
   (ignore-errors
     ;; first ensure that VARIANT is functional
     ((lambda (tests-passed)
        (assert (= 10 tests-passed) (tests-passed variant)
                "Variant only passed ~d of 10 tests: ~S" tests-passed variant))
      (parse-integer (shell "~a ~a" *func-test* (phenome variant))))
     ;; if so, then check the runtime of VARIANT
     (with-temp-file (file)
       (string-to-file (genome variant) file)
       (multiple-value-bind (stdout stderr exit)
           (shell "~a ~a ~a ~a" *opt-test* *prog* "CIL" file)
         (declare (ignorable stderr))
         (when (zerop exit) (multi-obj-fitness (parse-stdout stdout))))))
   infinity))

(defun test (variant)
  (incf *fitness-evals*)
  (evaluate variant))

(memoize #'test :key [#'genome #'first])

;; Run -- this will just run forever
#+run
(progn
  (setf (fitness *orig*) (test *orig*))
  (setf *population* (repeatedly *max-population-size* (copy *orig*)))
  (loop :for i :upto 46 :do
     (sb-thread:make-thread
      (lambda ()
        (evolve #'test
                :period 1024
                :period-func (lambda ()
                               (store
                                *population*
                                (format nil "pops/~d.store" *fitness-evals*)))))
      :name (format nil "opt-~d" i))))

(defun opt-threads ()
  (remove-if-not (lambda (thread)
                   (string= "opt" (subseq (sb-thread:thread-name thread) 0 3)))
                 (sb-thread:list-all-threads)))
