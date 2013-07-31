;;; annotate.lisp --- annotate assembly LOC with perf

;; Copyright (C) 2013  Eric Schulte

;;; Commentary:

;; Use perf annotate to label the assembly instructions with the HW
;; counters to which they contribute.

;;; Code:
(in-package :optimize)

;;; Genome Annotations
(defun asm-disassemble (bin func)
  (ignore-errors ;; TODO: debug parse-integer errors thrown within
    (let ((raw (shell "gdb --batch --eval-command=\"disassemble ~a\" ~a"
                      func bin))
          (rx "[ \t]*0x([a-zA-Z0-9]+)[ \t]*<\\+[0-9]+>:.*"))
      (remove nil
        (mapcar (lambda (line)
                  (multiple-value-bind (all matches) (scan-to-strings rx line)
                    (when all
                      (read-from-string (format nil "#x~a" (aref matches 0))))))
                (split-sequence #\Newline raw))))))

(defun perf-annotations (script)
  (remove nil
    (mapcar (lambda (line)
              (multiple-value-bind (all matches)
                  (scan-to-strings "([0-9\.]+) +:[ \\t]+([a-fA-F0-9]+):" line)
                (when all
                  (cons (read-from-string (format nil "#x~a"
                                                  (aref matches 1)))
                        (parse-number (aref matches 0))))))
            (split-sequence #\Newline (shell script)))))

(defun genome-addrs (asm &key bin &aux func-addrs)
  (let ((my-bin (or bin (phenome asm))))
    (unwind-protect
         (mapcar
          (lambda (l)
            (multiple-value-bind (all matches)
                (scan-to-strings "^([^\\.][a-zA-Z0-9_]*):" (aget :line l))
              (if all
                  (prog1 nil
                    (setf func-addrs (asm-disassemble my-bin (aref matches 0))))
                  (when func-addrs (pop func-addrs)))))
          (genome asm))
      (when (not bin) (delete-file my-bin)))))

(defun genome-anns (asm &key bin)
  (let* ((my-bin (or bin (phenome asm)))
         (script (format nil *script* my-bin)))
    (unwind-protect
         (mapcar {aget _ (perf-annotations script)}
                 (genome-addrs asm :bin my-bin))
      (when (not bin) (delete-file my-bin)))))

(defun smooth (list)
  (declare (cl-user::optimize speed))
  (mapcar (lambda (b3 b2 b1 o a1 a2 a3)
            (+ (* 0.006 (+ b3 a3))
               (* 0.061 (+ b2 a2))
               (* 0.242 (+ b1 a1))
               (* 0.383 o)))
          (append         (cdddr list) '(0 0 0))
          (append         (cddr  list) '(0 0))
          (append         (cdr   list) '(0))
          (append          list)
          (append '(0)     list)
          (append '(0 0)   list)
          (append '(0 0 0) list)))

(defun apply-annotations (asm &key smooth)
  "Apply annotations to the genome of ASM."
  (setf (genome asm)
        (mapcar (lambda (ann element)
                  (cons (cons :annotation ann) element))
                ((lambda (raw) (if smooth (smooth raw) raw))
                 (mapcar (lambda (ans) (or ans 0)) (genome-anns asm)))
                (genome asm))))


;;; print assembly LOC (aka ids) of annotations
(defun annotate (&optional (args *arguments*))
  (in-package :optimize)
  (flet ((arg-pop () (pop args)))
    (let ((help "Usage: annotate ANN-SCRIPT ASM-FILE [OPTIONS...]
 print the LOC of an ASM object annotated with perf

ANN-SCRIPT:
  Command line used to evaluate executables.  If the test
  script contains the substring \"~~a\" it will be replaced
  with the name of the executable, otherwise the executable
  will be appended to the end of the test script.  The script
  should return \"perf annotate\" output.

ASM-FILE:
  Is a text file of assembler code.

Options:
 -h,--help ------------- print this help message and exit
 -f,--flags FLAGS ------ flags to use when linking
 -l,--linker LINKER ---- linker to use
 -s,--smooth ----------- smooth the annotations
 -o,--out FILE --------- store annotated individual in FILE
 -e,--extended NUM ----- run extended test NUM
 -v,--verbose ---------- verbose debugging output~%")
          smooth out)
      (when (or (not args)
                (string= (subseq (car args) 0 2) "-h")
                (string= (subseq (car args) 0 3) "--h"))
        (format t help) (quit))

      (setf
       *script* (arg-pop)
       *path* (arg-pop)
       *orig* (from-file (make-instance 'asm-perf) *path*))

      (getopts
       ("-f" "--flags"  (setf (flags *orig*) (list (arg-pop))))
       ("-l" "--linker" (setf (linker *orig*) (arg-pop)))
       ("-s" "--smooth" (setf smooth t))
       ("-o" "--out"    (setf out (arg-pop)))
       ("-e" "--extended" (throw-error "Extended option not supported."))
       ("-v" "--verbose" (setf *shell-debug* t)))

      (apply-annotations *orig* :smooth smooth)

      (if out
          (progn
            (store *orig* out)
            (format t "~&Stored annotated individual in ~a~%" out))
          (format t "~&~{~{~a~^ ~}~^~%~}~%"
                  (indexed (mapcar {aget :annotation} (genome *orig*))))))))
