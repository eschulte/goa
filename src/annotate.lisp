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
  should return \"perf annotate\" output unless the --direct
  option is given.

ASM-FILE:
  Is a text file of assembler code.

Options:
 -h,--help ------------- print this help message and exit
 -l,--linker LINKER ---- linker to use
 -L,--lflags FLAGS ----- flags to use when linking
 -s,--smooth ----------- smooth the annotations
 -f,--flat ------------- binary annotation indicating execution
 -w,--widen RADIUS ----- widen binary annotations by RADIUS
 -d,--direct ----------- script outputs direct addresses
                         (otherwise \"perf annotate\" assumed)
 -D,--Direct ----------- script outputs direct ASM LOC numbers
 -o,--out FILE --------- store annotated individual in FILE
 -r,--range ------------ save as an `ann-range' object
 -e,--extended NUM ----- run extended test NUM
 -v,--verbose ---------- verbose debugging output~%")
        (self (pop args))
        smooth flat widen out range direct very-direct)
    (when (or (not args)
              (string= (subseq (car args) 0 2) "-h")
              (string= (subseq (car args) 0 3) "--h"))
      (format t help self) (quit))

    (setf
     *script* (pop args)
     *path* (pop args)
     *orig* (from-file (make-instance 'asm-perf) *path*))

    (getopts
     ("-l" "--linker" (setf (linker *orig*) (pop args)))
     ("-L" "--lflags" (setf (flags *orig*)
                            (split-sequence #\Space (pop args)
                                            :remove-empty-subseqs t)))
     ("-s" "--smooth" (setf smooth t))
     ("-f" "--flat"   (setf flat t))
     ("-w" "--widen"  (setf widen (parse-number (pop args))))
     ("-d" "--direct" (setf direct t))
     ("-D" "--Direct" (setf very-direct t))
     ("-o" "--out"    (setf out (pop args)))
     ("-r" "--range"  (setf range t))
     ("-e" "--extended" (throw-error "Extended option not supported."))
     ("-v" "--verbose" (setf *shell-debug* t)))

    (with-temp-file (bin)
      (phenome *orig* :bin bin)
      (let* ((script (format nil *script* bin))
             (anns (cond
                     ;; each line of the input file corresponds to an ASM LOC
                     (very-direct (mapcar #'parse-number
                                          (split-sequence #\Newline
                                            (shell script)
                                            :remove-empty-subseqs t)))
                     ;; count number of times each address occurs
                     (direct (counts (mapcar #'parse-number
                                             (split-sequence #\Newline
                                               (shell script)
                                               :remove-empty-subseqs t))))
                     ;; default to perf annotations
                     (t (perf-annotations script)))))
        (apply-annotations *orig* anns
                           :smooth smooth :widen widen :flat flat :bin bin
                           :loc very-direct)))

    (if out
        (progn
          (store (if range (to-ann-range *orig*) *orig*) out)
          (format t "~&Stored annotated individual in ~a~%" out))
        (format t "~&~{~{~a~^ ~}~^~%~}~%"
                (indexed (annotations *orig*))))))
