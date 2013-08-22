;;; neutral.lisp --- collect neutral variants

;; Copyright (C) 2013  Eric Schulte

;;; Commentary:

;; Collect multiple neutral variants of an assembly file.  This has
;; not yet been implemented.

;;; Code:
(in-package :optimize)

(defun neutral (args)
  (in-package :optimize)
  (let ((help "Usage: ~a SCRIPT ASM-FILE [OPTIONS...]
 Collect neutral variants of ASM-FILE removed from the
 original by multiple numbers of edits (steps).  For each
 neutral variant also store the output of SCRIPT.

SCRIPT:
  Command line used to evaluate executables.  If the test
  script contains the substring \"~~a\" it will be replaced
  with the name of the executable, otherwise the executable
  will be appended to the end of the test script.

ASM-FILE:
  A text file of assembler code or (if using the \".store\"
  extension) a serialized assembly software object.

Options:
 -h,--help ------------- print this help message and exit
 -l,--linker LINKER ---- linker to use
 -L,--lflags FLAGS ----- flags to use when linking
 -r,--res-dir DIR ------ store collected variants in DIR
 -n,--num NUM ---------- collect NUM variants at each step
                         default: 256
 -s,--steps ------------ total steps from the original
                         default: 16
 -d,--diff ------------- only store diffs of variants
 -v,--verbose ---------- verbose debugging output~%")
        (self (pop args))
        (res-dir "neutral-variants") diff (num 256) (steps 16))
    (when (or (not args)
              (string= (subseq (car args) 0 2) "-h")
              (string= (subseq (car args) 0 3) "--h"))
      (format t help self) (quit))

    (setf
     *script* (pop args)
     *path* (pop args)
     *orig* (from-file (make-instance 'asm-perf) *path*))

    (getopts
     ("-l" "--linker"  (setf (linker *orig*) (pop args)))
     ("-L" "--lflags"  (setf (flags *orig*)
                             (split-sequence #\Space (pop args)
                                             :remove-empty-subseqs t)))
     ("-r" "--res-dir" (setf res-dir (pop args)))
     ("-n" "--num"     (setf num (parse-number (pop args))))
     ("-s" "--steps"   (setf steps (parse-number (pop args))))
     ("-d" "--diff"    (setf diff t))
     ("-v" "--verbose" (setf *shell-debug* t)))

    (throw-error "TODO: implement")))
