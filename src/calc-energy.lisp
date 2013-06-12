(load "src/optimize.lisp")
(in-package :optimize)

(defvar *help* "Usage: run BENCHMARK EXECUTABLE -p|~a [OPTIONS...]
 calculate the energy of a run

Options:
 -m,--model NAME ------- model name
 -d,--debug ------------ show extra output~%")

(defvar *debug* nil)

(defun main (args)
  (in-package :optimize)
  (flet ((arg-pop () (pop args)))
    (let ((bin-path (arg-pop)))

      ;; print help information
      (when (and args
                 (ignore-errors (or (string= (subseq (car args) 0 2) "-h")
                                    (string= (subseq (car args) 0 3) "--h"))))
        (format t *help* bin-path)
        (error-out)))
    
    (getopts
     ("-m" "--model" (setf *model* (intern (string-upcase (arg-pop)))))
     ("-d" "--debug" (setf *debug* t)))

    (setf *model* (eval (or *model*
                            (case (arch)
                              (:intel 'intel-sandybridge-energy-model)
                              (:amd   'amd-opteron-energy-model)))))

    (when *debug*
      (format t "; *model*~%")
      (mapc (lambda-bind ((count . counters))
              (format t "~a ~a~%"
                      count
                      (mapcar ['string-downcase 'symbol-name] counters)))
            *model*))

    ;; parse inputs
    (let ((cs (mapcar
               (lambda-bind ((count counter))
                 (cons (make-keyword (string-upcase (symbol-name counter)))
                       count))
               (mapcar {mapcar #'read-from-string}
                       (mapcar
                        (lambda (l) (split-sequence "," l :test #'string=))
                        (loop :for line = (read-line *standard-input* nil)
                           :while line :collect line))))))

      (when *debug*
        (format t "~&~%; counters~%")
        (mapc (lambda-bind ((counter . count))
                (format t "~a ~a~%"
                        count
                        (string-downcase (symbol-name counter))))
              cs))

      ;; Apply the model to the counters
      (format t "~S~%"
              (reduce (lambda-bind (acc (cf . cntrs))
                        (let ((val (reduce #'+ (mapcar {aget _ cs} cntrs))))
                          (when *debug* (format t "~s ~s ~s~%" acc cf cntrs))
                          (+ acc (* cf val))))
                      *model* :initial-value 0)))))
