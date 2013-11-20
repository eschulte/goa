;;; delta.lisp --- delta-debugging between two software objects

;; Copyright (C) 2013  Eric Schulte

;;; Commentary:

;; Use Delta Debugging to minimize the genetic differences between two
;; software objects while maintaining their phenotypic differences.

;;; Code:
(in-package :optimize)

(defun retest (asm)
  (setf (stats asm) nil)
  (test asm))

(defun significance (seq1 seq2)
  (let ((std1 (standard-deviation seq1))
        (std2 (standard-deviation seq2)))
    (if (or (zerop std1) (zerop std2))
        (if (tree-equal seq1 seq2) 1.0 0)
        (t-test-two-sample-on-sequences seq1 seq2))))

(defun delta (args)
  (in-package :optimize)
  (let ((help "Usage: ~a TEST-SCRIPT ORIG.store NEW.store [OPTIONS...]
 Minimize the genetic differences between ORIGINAL and NEW as
 much as possible such that TEST-SCRIPT continues to show a
 phenotypic difference between the two.

TEST-SCRIPT:
  Command line used to evaluate executables.  If the test
  script contains the substring \"~~a\" it will be replaced
  with the name of the executable, otherwise the executable
  will be appended to the end of the test script.

ORIG.store:
  A cl-store file holding the original version of the program.

NEW.store:
  A cl-store file holding the new version of the program.

Options:
 -h,--help ------------- print this help message and exit
 -o,--out FILE --------- write results to FILE
 -p,--patch ------------ return an assembly patch
 -f,--fit-func FLAGS --- fitness function
                         default: output of TEST-SCRIPT
 -a,--alpha ------------ maximum significance (default ~d)
 -r,--reps NUM --------- run objects NUM times (default ~d)
 -v,--verbose ---------- verbose debugging output~%")
        (self (pop args))
        (alpha 0.1) (reps 5) orig new patch out)
    (when (or (not args)
              (< (length args) 3)
              (string= (subseq (car args) 0 2) "-h")
              (string= (subseq (car args) 0 3) "--h"))
      (format t help self alpha reps) (quit))
    (setf *script* (pop args)
          orig     (restore (pop args))
          new      (restore (pop args)))
    (getopts
     ("-o" "--out"      (setf out (pop args)))
     ("-p" "--patch"    (setf patch t))
     ("-f" "--fit-func" (setf *fitness-function* (read-from-string (pop args))))
     ("-a" "--alpha"    (setf alpha (parse-number (pop args))))
     ("-v" "--verbose"  (let ((level (parse-number (pop args))))
                          (setf *note-level* level)
                          (when (>= level 4) (setf *shell-debug* t)))))
    (unless out (setf out (if patch *standard-output* "minimized.store")))

    ;; sanity check
    (when (= (retest orig) (worst))
      (throw-error "Original program has no fitness!~%~S" (stats orig)))

    (let* ((base (lines orig))
           (diff (generate-seq-diff 'unified-diff base (lines new))))
      (flet ((rep-test (new)
               (let ((fits (loop :for i :below reps :collect (retest new))))
                 (note 3 "fitnesses: ~S~%" fits)
                 fits))
             (from-windows (windows)
               (let ((new (copy orig)))
                 (setf (lines new) (car (reduce
                                         (lambda-bind ((seq offset) window)
                                           (multiple-value-call #'list
                                             (apply-seq-window seq window
                                                               :offset offset)))
                                         windows :initial-value (list base 0))))
                 new)))
        ;; minimize the difference
        (let ((standard (progn (retest new) (rep-test new))))
          (setf (diff-windows diff)
                (minimize (diff-windows diff)
                          (lambda (windows)
                            (let* ((reps (rep-test (from-windows windows)))
                                   (sig (significance standard reps)))
                              (note 2 "sig of ~3d diffs is ~S"
                                    (length windows) sig)
                              ;; return success if sig is lower than alpha
                              (< alpha sig))))))
        ;; print the diff or the individual
        (if patch
            (if (streamp out)
                (render-diff diff out)
                (with-open-file (out out :direction :output)
                  (render-diff diff out)))
            (progn (note 2 "saving minimized individual to ~s" out)
                   (store (from-windows (diff-windows diff)) out)))))))
