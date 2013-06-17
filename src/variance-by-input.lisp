(in-package :optimize)

(defun counters (base bench size &aux vals)
  (mapc (lambda-bind ((counter count))
          (if (assoc counter vals)
              (push count (cdr (assoc counter vals)))
              (push (list counter count) vals)))
        (mapcar (lambda (list)
                  (cons (make-keyword (string-upcase (car list)))
                        (mapcar #'read-from-string (cdr list))))
                (mapcar {split-sequence #\Space}
                        (split-sequence #\Newline
                          (file-to-string
                           (format nil "~a/~a-~a.txt" base bench size))
                          :remove-empty-subseqs t))))
  vals)

;; http://en.wikipedia.org/wiki/Variance#Weighted_sum_of_variables
(defun sum-of-var (samples)
  "Return the weighted variance of MODEL given SAMPLES."
  (let ((mod (mapcar (lambda-bind ((a b)) (cons b a)) *model*)))
    (loop :for i :in (mapcar #'car mod) :summing
       (loop :for j :in (mapcar #'car mod) :summing
          (* (aget i mod) (aget j mod)
             (covariance (aget i samples) (aget i samples)))))))

(defun variance-by-input (&optional (args *arguments*))
  (flet ((arg-pop () (pop args)))
    (let ((sizes (list "test" "tiny" "small" "medium" "large"))
          (base "results/counters-by-input-size")
          bench
          (help "Usage: variance-by-input benchmark [OPTIONS...]
 print the variance as a function of size

Options:
 -h,--help --------- print this help message and exit
 -b,--base --------- change the base directory
 -m,--model NAME --- set model to NAME
~%"))
      (when (or (not args)
                (string= (subseq (car args) 0 2) "-h")
                (string= (subseq (car args) 0 3) "--h"))
        (format t help) (quit))

      (setf bench (arg-pop))
      (getopts
       ("-b" "--base"  (setf base (arg-pop)))
       ("-m" "--model" (setf *model* (intern (string-upcase (arg-pop))))))

      (format t "~{~{~a~^ ~}~^~%~}~%"
              (mapcar #'list
                      sizes
                      (mapcar [{sum-of-var *model*} {counters base bench}]
                              sizes)))))
  (quit))
