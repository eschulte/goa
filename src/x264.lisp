;;; neutral.lisp --- generate neutral versions of x264
(require :software-evolution)
(in-package :software-evolution)
(mapc (lambda (pkg) (require pkg) (use-package pkg))
      (list :cl-ppcre :curry-compose-reader-macros))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))

(defvar *work-dir* nil)

(defvar *test* "bin/x264-test")

(defvar *num-tests* 2 "Number of tests in `*test*'.")

(defvar *asm-lib* "data/libx264-asm.a")

(defvar *flags*
  `("-D_GNUCC" ,*asm-lib*
    "-L/usr/lib32" "-L/usr/lib" "-L/usr/lib32" "-L/usr/lib"
    "-lm" "-lpthread" "-s"))

(defvar *orig* (from-file (make-instance 'cil
                            :compiler "cilly"
                            :flags (mapconcat #'identity *flags* " "))
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

#+run
(progn
  ;; take the neutral walk
  (loop :for i :from 1 :to *steps* :do
     (push nil *neutral-walk*)
     (loop :until (= (length (first *neutral-walk*)) *size*) :do
        (let ((new (copy *orig*)))
          (setf (edits new) (copy-tree (random-elt (second *neutral-walk*))))
          (setf new (mutate new))
          (when (neutralp new)
            (push (copy-tree (edits new)) (first *neutral-walk*))))))
  ;; save the results
  (store *neutral-walk* "neutral-walk.store"))
