;;; debug.lisp --- functions for interactive debugging

;; Copyright (C) 2013  Eric Schulte

;;; Code:
(in-package :optimize)

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

(defun join (alist-a alist-b &key (test #'eql) missing)
  "Join ALIST-A and ALIST-B into an alist keyed by KEY"
  (let ((coll (make-hash-table :test test)))
    (mapc (lambda (a) (push (cons :a (cdr a)) (gethash (car a) coll)))
          alist-a)
    (mapc (lambda (b) (push (cons :b (cdr b)) (gethash (car b) coll)))
          alist-b)
    (loop :for key :being :the :hash-keys :of coll :using (hash-value val)
       :collect
       `(,key
         ,@(or (cdr (assoc :a val)) missing)
         ,@(or (cdr (assoc :b val)) missing)))))

(defun top-memory-instances (space &key (top-n 15))
  "Return a list of the TOP-N memory consuming instances in SPACE."
  (sb-ext:gc :full t :force t)
  (mapcar (lambda (line)
            (let ((re "^ *([^ ]\+): ([0-9,]\+) bytes, ([0-9,]\+) objects.$"))
              (multiple-value-bind (whole matches) (scan-to-strings re line)
                (declare (ignorable whole))
                (let ((m (coerce matches 'list)))
                  (cons (read-from-string (car m))
                        (mapcar (lambda (l)
                                  (parse-integer (regex-replace-all "," l "")))
                                (cdr m)))))))
          (cdr (butlast
                (split-sequence #\Newline
                  (with-output-to-string (out)
                    (let ((*standard-output* out))
                      (sb-vm:instance-usage space :top-n top-n)))
                  :remove-empty-subseqs t) 2))))

(defun preview-printable-type (&optional (stream *standard-output*))
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
                         0 (min 40 (length (sb-impl::string-output-stream-buffer
                                            obj)))))))
     :dynamic)
    (cons count biggest)))

(defun browse-conses (space &key next size)
  "Print selected allocated conses.
Starting with the START cons, or the at least SIZE big."
  (let ((count 0) (biggest 0) current-size)
    (sb-vm::map-allocated-objects
     (lambda (obj this-type this-size)
       (declare (ignorable this-type this-size))
       (when (typep obj 'cons)
         (incf count)
         (when (> this-size biggest) (setf biggest this-size))
         ;; interactive section
         (when (or (and next (= next count))
                   (and size (>= this-size size)))
           (format t "[~s,~s,~s] ~S~%"
                   next size this-size obj)
           (if next
               (progn (format t "set next to: ")
                      (setf next (ignore-errors (read))))
               (progn (format t "set size to: ")
                      (setf size (ignore-errors (read))))))))
     space)
    (cons count biggest)))

(defun print-memory (&optional (stream *standard-output*))
  (let ((*standard-output* stream))
    (sb-ext:gc :full t :force t)
    (format *standard-output* "~&#|~%")
    (sb-vm::memory-usage :print-spaces t :print-summary nil)
    (sb-vm:instance-usage :dynamic :top-n 200)
    (format *standard-output* "~&|#~%")))