;;; annotate.lisp --- annotate assembly LOC with perf

;; Copyright (C) 2013  Eric Schulte

;;; Commentary:

;; Use perf annotate to label the assembly instructions with the HW
;; counters to which they contribute.

;;; Code:
(in-package :optimize)

;;; Genome Annotations
(defun asm-disassemble (bin func)
  (let ((raw (shell "gdb --batch --eval-command=\"disassemble ~a\" ~a"
                    func bin))
        (rx "[ \t]*0x([a-zA-Z0-9]+)[ \t]*<\\+[0-9]+>:.*"))
    (remove nil
      (mapcar (lambda (line)
                (multiple-value-bind (all matches) (scan-to-strings rx line)
                  (when all
                    (read-from-string (format nil "#x~a" (aref matches 0))))))
              (split-sequence #\Newline raw)))))

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

(defun genome-anns (asm &key bin script)
  (let* ((my-bin (unless script (or bin (phenome asm))))
         (script
          (or script
              (if *size*
                  (format nil "~a ~a ~a -s ~a -a"
                          *script* *benchmark* my-bin *size*)
                  (format nil "~a ~a ~a -a"
                          *script* *benchmark* my-bin)))))
    (unwind-protect
         (mapcar {aget _ (perf-annotations script)}
                 (genome-addrs asm :bin my-bin))
      (when (not (or bin script)) (delete-file my-bin)))))

(defvar *kernel* '((-3 . 0.006) (-2 . 0.061) (-1 . 0.242)
                   (0 . 0.383)
                   (1 . 0.242)  (2 . 0.061) (3 . 0.006)))

(defun smooth (list &optional (kernel *kernel*))
  "Gaussian smoothing of LIST by KERNEL."
  (let ((result (make-array (length list) :initial-element 0)))
    (loop :for el :in list :as i :from 0 :do
       (loop :for (off . mult) :in kernel :do
          (let ((ind (+ i off)))
            (when (and (>= ind 0) (< ind (length list)))
              (incf (aref result ind) (* mult el))))))
    (coerce result 'list)))


;;; print assembly LOC (aka ids) of annotations
(defun annotate (&optional (args *arguments*))
  (in-package :optimize)
  (flet ((arg-pop () (pop args)))
    (let ((help "Usage: annotate benchmark benchmark.s [OPTIONS...]
 print the LOC of an ASM object annotated with perf

Options:
 -h,--help ------------- print this help message and exit
 -f,--flags FLAGS ------ flags to use when linking
 -l,--linker LINKER ---- linker to use
 -e,--extended NUM ----- run extended test NUM
 -v,--verbose ---------- verbose debugging output~%"))
      (when (or (not args)
                (string= (subseq (car args) 0 2) "-h")
                (string= (subseq (car args) 0 3) "--h"))
        (format t help) (quit))

      (let (script)
        (setf
         *benchmark* (arg-pop)
         *path* (arg-pop)
         *orig* (from-file (make-instance 'asm-perf) *path*))

        (getopts
         ("-f" "--flags"  (setf (flags *orig*) (list (arg-pop))))
         ("-l" "--linker" (setf (linker *orig*) (arg-pop)))
         ("-e" "--extended"
               (setf script (format nil "./bin/extended-tests.py ~a ~a ~d -a"
                                    *benchmark* (phenome *orig*) (arg-pop))))
         ("-v" "--verbose" (setf *shell-debug* t)))

        (loop :for ann :in (genome-anns *orig* :script script) :as i :upfrom 0
           :do (when ann (format t "~a ~a~%" i ann)))))))
