;;; neutral.lisp --- test sorters for neutrality across representations
(load "~/.sbclrc" :if-does-not-exist nil)
(require 'software-evolution)
(in-package :software-evolution)

#+complex
(advise-thread-pool-size 46)

(defvar *test* "host-test")
(defvar *fitness-predicate* #'<)
(defvar *prog* "blackscholes")
(defvar *orig* (from-file (make-instance 'asm) "blackscholes.s"))
(defvar *output* nil)
(defvar *work-dir* "../../sh-runner/work/")
(setf *max-population-size* 46)

(defun parse-stdout (stdout)
  (mapcar
   (lambda (line)
     (let ((fields (split-sequence #\Space line :remove-empty-subseqs t)))
       (cons (make-keyword (string-upcase (car fields)))
             (mapcar (lambda (c) (or (read-from-string c) c))
                     (cdr fields)))))
       (split-sequence #\Newline stdout :remove-empty-subseqs t)))

(defun evaluate (variant)
  (with-temp-file-of (file "s") (genome-string variant)
    (multiple-value-bind (stdout err-output exit)
        (shell "~a ~a ~a ~a" *test* *prog* "ASM" file)
      (declare (ignorable output err-output))
      (when (zerop exit) (parse-stdout stdout)))))

(defun multi-obj-fitness (output)
  "Calculate the total combined fitness of VARIANT based on `evaluate' output."
  ;; TODO: switch `:completion-time' with cycles.
  (if output
      (reduce #'+ (cdr (assoc :INSTRUCTIONS output)))
      double-float-positive-infinity))

(defun test (variant) (multi-obj-fitness (evaluate variant)))
(memoize #'test)

;; Sanity Check
#+sanity
(let ((orig-fit (multi-obj-fitness (evaluate *orig*))))
  (assert (and (numberp orig-fit) (> orig-fit 0))
          ((multi-obj-fitness *output*))
          "Fitness of the original is ~S but should be a number >0"
          orig-fit))

;; Run -- this will just run forever
#+run
(progn
  (dotimes (n 46)
    (sb-thread:make-thread (lambda () (evolve #'test)) :name "optimizer"))

  (sb-thread:make-thread
   (lambda () (loop :while *running* :for i :upfrom 0 :do
                 (store *population* (format nil "pop-~5,'0d.store" i))
                 (sleep 300)))))
