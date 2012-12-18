;;; neutral.lisp --- test sorters for neutrality across representations
(load "~/.sbclrc" :if-does-not-exist nil)
(require 'software-evolution)
(in-package :software-evolution)

(advise-thread-pool-size 40)

(defvar *test* "host-test")
(defvar *prog* "blackscholes")
(defvar *orig* (from-file (make-instance 'cil) "blackscholes.c"))
(setf *population* (repeatedly 100 (copy *orig*)))

(defun parse-stdout (stdout)
  (mapcar
   (lambda (line)
     (let ((fields (split-sequence #\Space line :remove-empty-subseqs t)))
       (cons (make-keyword (string-upcase (car fields)))
             (mapcar (lambda (c) (or (read-from-string c) c))
                     (cdr fields)))))
       (split-sequence #\Newline stdout :remove-empty-subseqs t)))

(defun evaluate (variant)
  (with-temp-file-of (file "c") (genome variant)
    (multiple-value-bind (stdout err-output exit)
        (shell "~a ~a ~a" *test* *prog* file)
      (declare (ignorable output err-output))
      (when (zerop exit) (parse-stdout stdout)))))

(defun multi-obj-fitness (variant)
  "Calculate the total combined fitness of VARIANT based on `evaluate' output."
  (/ 1 (apply #'max (assoc-ref (evaluate variant) :completion-time))))

;; Sanity Check
(format t "original programs has fitness ~a~%" (multi-obj-fitness *orig*))

;; Optimize
(store (evolve #'multi-obj-fitness)
       (make-pathname
        :directory (pathname-directory pathname)
        :name (pathname-name pathname)
        :type "store"))
