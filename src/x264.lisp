;;; neutral.lisp --- generate neutral versions of x264
(require :software-evolution)
(in-package :software-evolution)
(mapc (lambda (pkg) (require pkg) (use-package pkg))
      (list :cl-ppcre :curry-compose-reader-macros))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))

(defvar *work-dir* "sh-runner/work")

(defvar *test* "../../bin/x264-test")

(defvar *num-tests* 2 "Number of tests in `*test*'.")

(defvar *asm-lib* "../../data/libx264-asm.a")

(defvar *flags*
  `("-D_GNUCC" ,*asm-lib*
    "-L/usr/lib32" "-L/usr/lib" "-L/usr/lib32" "-L/usr/lib"
    "-lm" "-lpthread" "-s"))

(defvar *orig* (from-file (make-instance 'cil :compiler "cilly" :flags *flags*)
                          "data/x264_comb.c"))

(defvar *steps* 10 "Number of neutral steps to take.")
(defvar *size* 500 "Size of each neutral step.")

(defvar *neutral-walk* (list (list (edits *orig*)))
  "Variable to hold the results of the walk.")

(defun test (variant)
  (with-temp-file (file)
    (phenome variant :bin file)
    (multiple-value-bind (stdout stderr errno) (shell "~a ~a" *test* file)
      (declare (ignorable stderr))
      (if (zerop errno)
          (parse-integer stdout)
          0))))
(memoize #'test :key [#'edits #'car])
;; (un-memoize 'test)

(defun neutralp (variant)
  (setf (fitness variant) (test variant))
  (= *num-tests* (fitness variant)))

(defun neutral-walker ()
  (loop :while *running* :do
     (let ((new (copy *orig*)))
       (setf (edits new) (copy-tree (random-elt (second *neutral-walk*))))
       (setf new (mutate new))
       ;; check if the variant is neutral
       (when (neutralp new)
         (push (copy-tree (edits new)) (first *neutral-walk*))
         ;; check if we should move on to the next step
         (when (>= (length (first *neutral-walk*)) *size*)
           (push nil *neutral-walk*)
           ;; check if we are done
           (when (> (length *neutral-walk*) *steps*)
             (setf *running* nil)))))))

#+run
(progn
  ;; setup
  (setf *running* t)
  (push nil *neutral-walk*)
  ;; take the neutral walk
  (loop :for i :below 46 :do (sb-thread:make-thread #'neutral-walker))
  (loop :until (not *running*) :do (sleep 10))
  ;; save the results
  (store *neutral-walk* "neutral-walk.store"))

#+save
(loop :for step :in (reverse *neutral-walk*) :as s-count :from 0 :do
   (loop :for ind :in step :as n-count :from 0 :do
      (string-to-file (genome-string (copy *orig* :edits ind))
                      (format nil "x264-walk/~d-~3,'0d.s" s-count n-count))))
