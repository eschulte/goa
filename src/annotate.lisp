;;; annotate.lisp --- annotate assembly LOC with perf

;; Copyright (C) 2013  Eric Schulte

;;; Commentary:

;; The following code assumes that the compiler flag has been saved
;; into the `flag' variable.
;; 
;; Run with the optimize script with something like the following
;; 
;;     optimize bzip2.s bzip2 -e "(defvar flag 'O0)" -c by-flag.lisp

;;; Code:
(in-package :optimize)

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

(defun perf-annotations (bin)
  (let ((raw (if (null *size*)
                 (shell "~a ~a ~a -a"       *script* *benchmark* bin)
                 (shell "~a ~a ~a -s ~a -a" *script* *benchmark* bin *size*)))
        (rx "([0-9\.]+) +:[ \\t]+([a-fA-F0-9]+):"))
    (remove nil
      (mapcar (lambda (line)
                (multiple-value-bind (all matches) (scan-to-strings rx line)
                  (when all
                    (cons (read-from-string (format nil "#x~a"
                                                    (aref matches 1)))
                          (parse-number (aref matches 0))))))
              (split-sequence #\Newline raw)))))

(defun annotate-w-addr (asm &key bin &aux func-addrs)
  (unwind-protect
       (let ((my-bin (or bin (phenome asm))))
         (mapcar
          (lambda (l)
            (multiple-value-bind (all matches)
                (scan-to-strings "^([^\\.][a-zA-Z0-9_]*):" (aget :line l))
              (if all
                  (setf func-addrs (asm-disassemble my-bin (aref matches 0)))
                  (when func-addrs
                    (push (cons :address (pop func-addrs)) l))))
            l)
          (genome asm)))
    (when bin (delete-file bin))))

(defun annotate-w-perf (asm &key bin)
  (unwind-protect
       (let ((my-bin (or bin (phenome asm))))
         (let ((perf-ann (perf-annotations my-bin)))
           (format t "perf-ann has ~d~%" (length perf-ann))
           (mapcar
            (lambda (l)
              (when-let ((addr (aget :address l)))
                (when-let ((anns (aget addr perf-ann)))
                  (push (cons :annotation anns) l)))
              l)
            (annotate-w-addr asm :bin my-bin))))
    (when bin (delete-file bin))))

;; apply the perf annotations to the genome
(setf (genome *orig*) (annotate-w-perf *orig*))
