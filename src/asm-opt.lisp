;; just like `asm-perf', but more compact in-memory representation
(defclass asm-opt (asm-perf) ())

(defvar *code-strings* nil)

(setf *orig* (make-instance 'asm-opt))
(setf (genome *orig*)
      (loop :for i :below (length *code-strings*)
         :collect (list (cons :line i))))

(declaim (inline lines))
(defmethod lines ((simple asm-opt))
  (mapcar [{aref *code-strings*} {aget :line}] (genome simple)))

(declaim (inline genome-string))
(defmethod genome-string ((simple asm-opt))
  (format nil "~{~a~%~}" (lines simple)))

(defmethod phenome ((asm asm-opt) &key bin)
  (with-temp-file-of (src "s") (genome-string asm)
    (let ((bin (or bin (temp-file-name))))
      (multiple-value-bind (stdout stderr exit)
          (shell "~a -o ~a ~a ~{~a~^ ~}"
                 (or (linker asm) *asm-linker*) bin src (flags asm))
        (declare (ignorable stdout ))
        (values (if (zerop exit) bin stderr) exit)))))
