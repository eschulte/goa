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

(defmacro annotate-w-addr (asm &key bin)
  (let* ((bin-var (or bin (gensym)))
         (func-addrs (gensym))
         (body
          `(let (,func-addrs)
             (mapcar
              (lambda (l)
                (multiple-value-bind (all matches)
                    (scan-to-strings "^([^\\.][a-zA-Z0-9_]*):" (aget :line l))
                  (if all
                      (setf ,func-addrs (asm-disassemble ,bin-var (aref matches 0)))
                      (when ,func-addrs
                        (push (cons :address (pop ,func-addrs)) l))))
                l)
              (genome ,asm)))))
    (if bin
        body
        `(with-temp-file (,bin-var)
           (phenome ,asm :bin ,bin-var)
           ,body))))

(defun annotate-w-perf (asm)
  (with-temp-file (bin)
    (phenome asm :bin bin)
    (let ((perf-ann (perf-annotations bin)))
      (mapcar
       (lambda (l)
         (when-let ((addr (aget :addr l)))
           (when-let ((anns (aget addr perf-ann)))
             (push (cons :annotation anns) l)))
         l)
       (annotate-w-addr asm :bin bin)))))
