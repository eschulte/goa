;;; neutral.lisp --- generate neutral versions of blackscholes
(require :software-evolution)
(in-package :software-evolution)
(mapc (lambda (pkg) (require pkg) (use-package pkg))
      (list :cl-ppcre :curry-compose-reader-macros))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))

(defvar infinity
  #+sbcl
  SB-EXT:DOUBLE-FLOAT-POSITIVE-INFINITY
  #-(or sbcl)
  (error "must specify a positive infinity value"))

(defvar *work-dir* "sh-runner/work/")

(defvar *test* "../../bin/blackscholes-test")

(defvar *num-tests* 5 "Number of tests in `*test*'.")

(defvar *flags*
  '("-L/usr/lib64" "-L/usr/lib" "-static" "-u" "CarbonStartSim"
    "-u" "CarbonStopSim" "-pthread" "-lstdc++" "-lm"))

(defvar *orig* (from-file (make-instance 'asm :linker "g++" :flags *flags*)
                          "data/blackscholes.m4.s"))

(defvar *steps* 10 "Number of neutral steps to take.")
(defvar *size* 500 "Size of each neutral step.")

(defvar *neutral-walk* (list (list (edits *orig*)))
  "Variable to hold the results of the walk.")

(defvar *stdout*)

(defun parse-stdout (stdout)
  "Parse the Graphite output of host-test."
  (remove
      nil
      (mapcar
       (lambda (line)
         (let ((fields (split-sequence #\Space (regex-replace-all "" line "")
                                       :remove-empty-subseqs t)))
           (unless (null fields)
             (cons (make-keyword (string-upcase (car fields)))
                   (mapcar (lambda (c) (or (ignore-errors (parse-number c)) c))
                           (cdr fields))))))
       (remove-if (lambda (line) (or (scan "hooks" line)
                                (scan "warning" line)
                                (scan "spawn_master" line)))
                  (split-sequence #\Newline stdout :remove-empty-subseqs t)))))

(defun group-stats (stats &aux group results)
  (dolist (row stats (reverse (cons (reverse group) results)))
    (case (car row)
      ((:tile-summary :core-model-summary :network-summary :cache-summary)
       (setf group nil))
      ((:cache-l1-i :cache-l2 :cache-l1-d :dram-performance-model-summary)
       (when group (push (reverse group) results))
       (setf group row))
      (:network-model
       (when group
         (push (reverse group) results)
         (setf group nil))
       (push row group))
      (t
       (if group
           (push row group)
           (push row results))))))

(defun energy-delay-product (stats)
  (flet ((energy (group)
           (+ (reduce #'+ (cdr (assoc :static-power group)))
              (reduce #'+ (cdr (assoc :dynamic-energy group))))))
    (* 
     ;; Runtime
     (reduce #'+ (aget :completion-time (group-stats (parse-stdout *stdout*))))
     ;; Energy
     (reduce #'+ (mapcar #'energy
                         (cons
                          (aget '(:network-model 2) stats :test #'tree-equal)
                          (mapcar {aget _ stats}
                                  '(:cache-l1-i :cache-l1-d :cache-l2
                                    :dram-performance-model-summary))))))))

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

(defun graphite-test (variant)
  (multiple-value-bind (stdout stderr errno) (shell "cat /tmp/output")
    (declare (ignorable stderr))
    (if (zerop errno)
        (energy-delay-product (group-stats (parse-stdout stdout)))
        infinity)))

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
                      (format nil "bs-walk/~d-~3,'0d.s" s-count n-count))))
