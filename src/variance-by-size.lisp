(in-package :optimize)

(defun counters (base bench size &aux vals)
  (mapc (lambda-bind ((counter count))
          (if (assoc counter vals)
              (push count (cdr (assoc counter vals)))
              (push (list counter count) vals)))
        (mapcar (lambda (list)
                  (cons (intern (string-upcase (car list)))
                        (mapcar #'read-from-string (cdr list))))
                (mapcar {split-sequence #\Space}
                        (split-sequence #\Newline
                          (file-to-string
                           (format nil "~a/~a-~a.txt" base bench size))
                          :remove-empty-subseqs t))))
  vals)

;; http://en.wikipedia.org/wiki/Variance#Weighted_sum_of_variables
(defun sum-of-var (coefficients samples)
  (loop :for i :in (mapcar #'car coefficients) :summing
     (loop :for j :in (mapcar #'car coefficients) :summing
        (* (aget i coefficients) (aget j coefficients)
           (covariance (aget i samples) (aget i samples))))))

;; This won't be fully general, rather it will assume that `*model*'
;; is of the seconds × power variety, where power is the sum of the
;; intercept with a number of /cycles counts w/coefficients.
(defun power-model-variance (samples) ;; TODO: not dividing values by cycles!
  "Return the mean and variance of power-model estimates of a list of samples.
Using the model in `*model*'"
  (let* (;; collect the counters from the model
         (hw-cs (mapcar [#'car #'cdaddr] (cddr (third *model*))))
         ;; pull the coefficients from the model
         (w-vals (mapcar (lambda (c)
                           (if (listp c)
                               (cons 'fops (mapcar (first c)
                                                   (aget (second c) samples)
                                                   (aget (third c) samples)))
                               (cons c (aget c samples))))
                         hw-cs))
         ;; collect the counter values into an alist keyed by counter
         (cfs (mapcar #'cons
                      (mapcar #'car w-vals)
                      (mapcar #'second (cddr (third *model*)))))
         ;; transpose of above gives a list of runs
         (runs-w-headers (apply #'mapcar #'list w-vals))
         ;; calculate the model power of each run
         (powers (mapcar (lambda (vals)
                           (+ (second (third *model*)) ;; power constant
                              (reduce #'+ (mapcar (lambda (c v) (* (aget c cfs) v))
                                                  (car runs-w-headers)
                                                  vals))))
                         (cdr runs-w-headers)))
         ;; variance of the power portion of the model
         (power-var (sum-of-var cfs w-vals))
         (seconds (aget 'seconds samples)) (seconds-var (variance seconds)))
    (values
     (mean (mapcar #'* seconds powers))
     ;; produce of independent variables, seconds and powers
     ;; http://en.wikipedia.org/wiki/Variance#Product_of_independent_variables
     (+ (* (expt (mean powers) 2) seconds-var)
        (* (expt (mean seconds) 2) power-var)
        (* seconds-var power-var)))))

(defun variance-by-size (&optional (args *arguments*))
  (in-package :optimize)
  (flet ((arg-pop () (pop args)))
    (let ((sizes (list "test" "tiny" "small" "medium" "large"))
          (base "results/counters-by-input-size")
          (*model* (case (arch)
                     (:intel 'intel-sandybridge-power-model)
                     (:amd   'amd-opteron-power-model)))
          bench
          (help "Usage: variance-by-input benchmark [OPTIONS...]
 print the variance as a function of size

Options:
 -h,--help --------- print this help message and exit
 -b,--base --------- change the base directory
 -m,--model NAME --- set model to NAME~%"))
      (when (or (not args)
                (string= (subseq (car args) 0 2) "-h")
                (string= (subseq (car args) 0 3) "--h"))
        (format t help) (quit))

      (setf bench (arg-pop))
      (getopts
       ("-b" "--base"  (setf base (arg-pop)))
       ("-m" "--model" (setf *model* (intern (string-upcase (arg-pop))))))
      (setf *model* (eval *model*))

      (format t "size   mean         variance~%")
      (loop :for size :in sizes :do
         (multiple-value-bind (mean variance)
             (power-model-variance (counters base bench size))
           (format t "~6a ~12a ~12a~%" size mean variance))))))
