;;; neutral.lisp --- generate neutral versions of blackscholes
(require :software-evolution)
(in-package :software-evolution)
(mapc (lambda (pkg) (require pkg) (use-package pkg))
      (list :cl-ppcre :curry-compose-reader-macros))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))

(defvar *work-dir* "sh-runner/work/")

(defvar *test* "../../bin/blackscholes-test")

(defvar *num-tests* 5 "Number of tests in `*test*'.")

(defvar *flags*
  '("-m32"  "-O3" "-g" "-funroll-loops" "-fprefetch-loop-arrays" "-fpermissive"
    "-fno-exceptions" "-static-libgcc" "-Wl,--hash-style=both,--as-needed"
    "-DPARSEC_VERSION=3.0-beta-20120904" "-pthread" "-DENABLE_THREADS" "-DNCO=4"
    "-L/usr/lib64" "-L/usr/lib"))

(defvar *orig* (from-file (make-instance 'asm
                            :linker "g++"
                            :flags (mapconcat #'identity *flags* " "))
                          "data/blackscholes.m4.s"))

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

#+save
(loop :for step :in (reverse *neutral-walk*) :as s-count :from 0 :do
   (loop :for ind :in step :as n-count :from 0 :do
      (string-to-file (genome-string (copy *orig* :edits ind))
                      (format nil "bs-walk/~d-~3,'0d.s" s-count n-count))))
