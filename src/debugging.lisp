;;; debug.lisp --- functions for interactive debugging

;; Copyright (C) 2013  Eric Schulte

;;; Code:
(in-package :optimize)
(require :hu.dwim.debug)

(defun setup ()
  (setf *orig* (from-file (make-instance 'asm-perf)
                          "benchmarks/bzip2/bzip2.s"))
  (setf *benchmark* "bzip2")
  (run *orig*))


;;; Memory analysis
(defvar *bad* nil)
(defvar *perf* nil)

(let ((counter 0) (asm-counter 0))
  (sb-vm::map-allocated-objects
   (lambda (obj type size)
     (declare (ignorable type size))
     (typecase obj
       (asm-perf (incf asm-counter)
                 (when (= asm-counter 200) (setf *perf* obj)))
       ((SIMPLE-VECTOR 256))
       (cons (incf counter) (when (= counter 3000000) (setf *bad* obj)))
       (t)))
   :dynamic)
  asm-counter)

(defun top-memory-instances (space &key (top-n 15))
  "Return a list of the TOP-N memory consuming instances in SPACE."
  (mapcar (lambda (line)
            (let ((re "^ *([^ ]\+): ([0-9,]\+) bytes, ([0-9,]\+) objects.$"))
              (multiple-value-bind (whole matches) (scan-to-strings re line)
                (declare (ignorable whole))
                (let ((m (coerce matches 'list)))
                  (cons (car m)
                        (mapcar (lambda (l)
                                  (parse-integer (regex-replace-all "," l "")))
                                (cdr m)))))))
          (cdr (butlast
                (split-sequence #\Newline
                  (with-output-to-string (out)
                    (let ((*standard-output* out))
                      (sb-vm:instance-usage space :top-n top-n)))
                  :remove-empty-subseqs t) 2))))

(defun preview-output-streams (&optional (stream *standard-output*))
  "Print the prefix of every allocated output stream."
  (let ((count 0) (biggest 0))
    (sb-vm::map-allocated-objects
     (lambda (obj this-type size)
       (declare (ignorable this-type size))
       (when (typep obj 'SB-IMPL::STRING-OUTPUT-STREAM)
         (incf count)
         (when (> (length (sb-impl::string-output-stream-buffer obj)) biggest)
           (setf biggest (length (sb-impl::string-output-stream-buffer obj))))
         (format stream "[~d] ~S~%"
                 (length (sb-impl::string-output-stream-buffer obj))
                 (subseq (sb-impl::string-output-stream-buffer obj)
                         0 (min 40 (length (sb-impl::string-output-stream-buffer obj)))))))
     :dynamic)
    (cons count biggest)))
