(load "src/optimize.lisp")
(in-package :optimize)

(defvar *help* "Usage: run BENCHMARK EXECUTABLE -p|~a [OPTIONS...]
 calculate the energy of a run

Options:
 -m,--model NAME ------- model name~%")

(defun main (args)
  (flet ((arg-pop () (pop args)))
    (let ((bin-path (arg-pop)))

      ;; print help information
      (when (and args
                 (ignore-errors (or (string= (subseq (car args) 0 2) "-h")
                                    (string= (subseq (car args) 0 3) "--h"))))
        (format t *help* bin-path)
        (sb-ext:exit :code 1)))

    ;; get model
    (getopts
     ("-m" "--model"     (setf *model* (intern (string-upcase (arg-pop))))))
    (unless *model*
      (setf *model* (case (arch)
                      (:intel 'intel-sandybridge-energy-model)
                      (:amd   'amd-opteron-energy-model))))
    (when (symbolp *model*) (setf *model* (eval *model*)))

    ;; parse inputs
    (let ((cs (mapcar {mapcar #'read-from-string}
                      (mapcar (lambda (l) (split-sequence "," l :test #'string=))
                              (loop :for line = (read-line *standard-input* nil)
                                 :while line :collect line)))))

      ;; convert to an alist with variance
      (setf cs (mapcar (lambda-bind ((val counter . stdev))
                         (let ((stdev (when stdev
                                        (let* ((name (symbol-name (car stdev)))
                                               (list (coerce name 'list))
                                               (num  (read-from-string
                                                      (coerce (butlast list)
                                                              'string))))
                                          (list (* num num))))))
                           (cons (make-keyword (symbol-name counter))
                                 (cons val stdev))))
                       cs))

      (setf cs (mapcar (lambda-bind ((counter val . variance))
                         (declare (ignorable variance))
                         (cons counter val)) cs))

      ;; calculate energy (optionally with standard deviations)
      ;;
      ;; need to combine variances
      ;; http://en.wikipedia.org/wiki/Variance#Properties
      (let ((e (reduce (lambda-bind (acc (cf . cntrs))
                         (+ acc (* cf (reduce #'+ (mapcar {aget _ cs} cntrs)))))
                       *model* :initial-value 0)))
        (format t "~S~%" e)))))
