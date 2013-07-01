(in-package :optimize)

(defun parse-hw-counter-file (path &aux run results size)
  (with-open-file (in path)
    (loop :for line = (read-line in nil nil) :while line
       :do (let ((parts (split-sequence #\, line :remove-empty-subseqs t)))
             (when parts
               (if  (= (length parts) 1)
                    (setf size (intern (string-upcase (car parts))))
                    (let ((key (intern (string-upcase (second parts))))
                          (val (parse-number (car parts))))
                      (if (eq key 'exit)
                          (progn (when run (push run results))
                                 (setf run (list (cons key val)
                                                 (cons 'size size))))
                          (push (cons key val) run))))))))
  (push run results)
  results)

;; http://en.wikipedia.org/wiki/Variance#Weighted_sum_of_variables
(defun sum-of-var (coefficients samples)
  (declare (optimize speed))
  (loop :for i :below (length coefficients) :summing
     (loop :for j :below (length coefficients) :summing
        (* (nth i coefficients) (nth j coefficients)
           (covariance (nth i samples) (nth i samples))))))

(defun power-stats (runs size)
  "Return the mean and variance of the `*model*' for RUNS of size SIZE."
  (block nil
    (let* ((hw-cs (mapcar [#'second #'third] (cddr (third *model*))))
           (cfs   (mapcar  #'second          (cddr (third *model*))))
           (valid (remove-if-not (lambda (record)
                                   (every (lambda (c)
                                            (if (listp c)
                                                (every {aget _ record} (cdr c))
                                                (aget c record)))
                                          (cons 'cycles hw-cs)))
                                 (remove-if-not [{eq size} {aget 'size}] runs)))
           ;; cleaned up inputs to the model
           (clean (mapcar
                   (lambda (run)
                     (let ((cycles (aget 'cycles run)))
                       (mapcar (lambda (c)
                                 (/ (if (listp c)
                                        (reduce #'+ (mapcar {aget _ run}
                                                            (cdr c)))
                                        (aget c run))
                                    cycles))
                               hw-cs)))
                   (or valid (return (list nil nil 0)))))
           (pwr-var (sum-of-var
                     cfs (apply #'mapcar #'list clean)))
           (pwr-mean (mean (mapcar (lambda (vars)
                                     (+ (second (third *model*)) ;; constant
                                        (reduce #'+ (mapcar #'* cfs vars))))
                                   clean))))
      (list pwr-mean pwr-var (length valid)))))

(defun energy-stats (runs size)
  (let* ((power-stats (power-stats runs size))
         (pwr-mean (first power-stats))
         (pwr-var (second power-stats))
         (hw-cs (mapcar [#'second #'third] (cddr (third *model*))))
         (valid (remove-if-not (lambda (record)
                                 (every (lambda (c)
                                          (if (listp c)
                                              (every {aget _ record} (cdr c))
                                              (aget c record)))
                                        (cons 'cycles hw-cs)))
                               (remove-if-not [{eq size} {aget 'size}] runs)))
         (seconds (mapcar {aget 'seconds} valid))
         (sec-mean (mean seconds))
         (sec-var (variance seconds)))
    (list
     ;; Note: should do mean of products and not products of mean, but
     ;;       this is easier and we mainly want order of magnitude
     (* sec-mean pwr-mean)
     ;; http://en.wikipedia.org/wiki/Variance#Product_of_independent_variables
     (+ (* (expt sec-mean 2) pwr-var)
        (* (expt pwr-mean 2) sec-var)
        (* pwr-var sec-var))
     (length valid))))

(defun total-stats (runs size)
  ;; stat groups of the correct size
  (flet ((keyword (it) (make-keyword (string-upcase (symbol-name it)))))
    (let ((model-results
           (mapcar
            (lambda (group)
              (apply-model *model*
                           (mapcar (lambda-bind ((counter . count))
                                     (cons (keyword counter) count))
                                   (remove-if [{eql 'size} #'car] group))))
            (remove-if-not [{eq size} {aget 'size}] runs))))
      (list (mean model-results)
            (variance model-results)
            (length model-results)))))

(defun model-variance (&optional (args *arguments*))
  (in-package :optimize)
  (flet ((arg-pop () (pop args))
         (to-sym (str) (intern (string-upcase str))))
    (let ((sizes '(test tiny small medium large))
          (*model* (eval (case (arch)
                           (:intel 'intel-sandybridge-power-model)
                           (:amd   'amd-opteron-power-model))))
          (help "Usage: variance-by-input file [OPTIONS...]
 print the variance as a function of size

Options:
 -h,--help --------- print this help message and exit
 -s,--size --------- only for size
 -m,--model NAME --- set model to NAME
 -e,--energy ------- variance of energy, not power 
 -c,--counter C ---- also print values of counter C
 -t,--total -------- don't calculate model variance from
                     variance of inputs, instead run model
                     on each input set and return variance
                     of model outputs~%")
          runs counter energy total)
      (when (or (not args)
                (string= (subseq (car args) 0 2) "-h")
                (string= (subseq (car args) 0 3) "--h"))
        (format t help) (quit))

      (setf runs (parse-hw-counter-file (arg-pop)))
      (getopts
       ("-s" "--size" (setf sizes (list (to-sym (arg-pop)))))
       ("-m" "--model" (setf *model* (eval (to-sym (arg-pop)))))
       ("-c" "--counter" (setf counter (to-sym (arg-pop))))
       ("-e" "--energy" (setf energy t))
       ("-t" "--total" (setf total t)))

      (format t "size          mean    variance      percent number   ~a~%"
              (string-downcase (or counter "")))
      (mapc (lambda (size stats counter)
              (let ((mean (first stats))
                    (variance (second stats))
                    (number (third stats)))
                (if (or (null mean) (zerop mean))
                    (format t "~6a ~11@a ~11@a  ~11@a ~4@a~%"
                            size "NA" "NA" "NA" "NA")
                    (apply #'format t
                           (format
                            nil
                            "~~6,a ~~11F ~~11F ~~11F% ~~4d  ~a~~%"
                            (if counter " ~11F" ""))
                           size mean variance
                           (* 100 (/ variance mean))
                           number
                           (list counter)))))
            (mapcar [#'string-downcase #'symbol-name] sizes)
            (mapcar
             (lambda (size)
               (handler-case
                   (funcall (cond (total #'total-stats)
                                  (energy #'energy-stats)
                                  (t #'power-stats))
                            runs size)
                 (error () (list nil nil 0))))
             sizes)
            (mapcar
             (lambda (size)
               (if counter
                   (handler-case
                       (mean (remove nil
                               (mapcar {aget counter}
                                       (remove-if-not [{eq size} {aget 'size}]
                                                      runs))))
                     (error () "NA"))
                   nil))
             sizes)))))
