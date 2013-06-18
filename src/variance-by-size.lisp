(in-package :optimize)

(defun parse-counter-by-size-file (path &aux run results size)
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
  (loop :for i :below (length coefficients) :summing
     (loop :for j :below (length coefficients) :summing
        (* (nth i coefficients) (nth j coefficients)
           (covariance (nth i samples) (nth i samples))))))

;; TODO: add option to output energy as well
(defun power-stats-for-size (runs size)
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
                       (cons (aget 'seconds run)
                             (mapcar (lambda (c)
                                       (/ (if (listp c)
                                              (reduce #'+ (mapcar {aget _ run}
                                                                  (cdr c)))
                                              (aget c run))
                                          cycles))
                                     hw-cs))))
                   (or valid (return (list nil nil 0)))))
           (pwr-var (sum-of-var
                     cfs (apply #'mapcar #'list (mapcar #'cdr clean))))
           (pwr-mean (mean (mapcar (lambda (vars)
                                     (+ (second (third *model*)) ;; constant
                                        (reduce #'+ (mapcar #'* cfs vars))))
                                   clean))))
      (list pwr-mean pwr-var (length valid)))))

(defun variance-by-size (&optional (args *arguments*))
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
 -c,--counter C ---- also print values of counter C~%")
          runs counter)
      (when (or (not args)
                (string= (subseq (car args) 0 2) "-h")
                (string= (subseq (car args) 0 3) "--h"))
        (format t help) (quit))

      (setf runs (parse-counter-by-size-file (arg-pop)))
      (getopts
       ("-s" "--size" (setf sizes (list (to-sym (arg-pop)))))
       ("-m" "--model" (setf *model* (eval (to-sym (arg-pop)))))
       ("-c" "--counter" (setf counter (to-sym (arg-pop)))))

      ;; TODO: add var/mean percent
      (format t "size   mean       variance   percent    number ~a~%"
              (string-downcase (or counter "")))
      (mapc (lambda (size stats counter)
              (let ((mean (first stats))
                    (variance (second stats))
                    (number (third stats)))
                (apply #'format t
                       (format nil "~~6a ~~10a ~~10a ~~9f% ~~6a ~a~~%"
                               (if counter " ~f" ""))
                       size mean variance (* 100 (/ variance mean)) number
                       (list counter))))
            (mapcar [#'string-downcase #'symbol-name] sizes)
            (mapcar {power-stats-for-size runs} sizes)
            (mapcar
             (lambda (size)
               (if counter
                   (mean (remove nil
                           (mapcar {aget counter}
                                   (remove-if-not [{eq size} {aget 'size}]
                                                  runs))))
                   nil))
             sizes)))))
