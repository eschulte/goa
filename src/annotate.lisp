;;; annotate.lisp --- annotate assembly LOC with perf

;; Copyright (C) 2013  Eric Schulte

;;; Commentary:

;; Use perf annotate to label the assembly instructions with the HW
;; counters to which they contribute.

;;; Code:
(in-package :optimize)

(defun annotate (args)
  (in-package :optimize)
  (let ((help "Usage: ~a ANN-SCRIPT ASM-FILE [OPTIONS...]
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
 -l,--linker LINKER ---- linker to use
 -L,--lflags FLAGS ----- flags to use when linking
 -s,--smooth ----------- smooth the annotations
 -o,--out FILE --------- store annotated individual in FILE
 -r,--range ------------ save as an `ann-range' object
 -e,--extended NUM ----- run extended test NUM
 -v,--verbose ---------- verbose debugging output~%")
          smooth out range)
      (when (or (not args)
                (string= (subseq (car args) 0 2) "-h")
                (string= (subseq (car args) 0 3) "--h"))
        (format t help) (quit))

      (setf
       *script* (arg-pop)
       *path* (arg-pop)
       *orig* (from-file (make-instance 'asm-perf) *path*))

      (getopts
       ("-l" "--linker" (setf (linker *orig*) (arg-pop)))
       ("-L" "--lflags" (setf (flags *orig*)
                              (split-sequence #\Space (pop args)
                                              :remove-empty-subseqs t)))
       ("-s" "--smooth" (setf smooth t))
       ("-o" "--out"    (setf out (arg-pop)))
       ("-r" "--range"  (setf range t))
       ("-e" "--extended" (throw-error "Extended option not supported."))
       ("-v" "--verbose" (setf *shell-debug* t)))

      (apply-annotations *orig* :smooth smooth)

      (if out
          (progn
            (store (if range (to-ann-range *orig*) *orig*) out)
            (format t "~&Stored annotated individual in ~a~%" out))
          (format t "~&~{~{~a~^ ~}~^~%~}~%"
                  (indexed (annotations *orig*)))))))
