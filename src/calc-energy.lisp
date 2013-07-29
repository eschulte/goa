(in-package :optimize)


;;; Models
(defvar intel-sandybridge-energy-model
  '(+
    (* 5.007e-15 cycles)
    (* 1.774e-16 instructions)
    (* 2.787e-16 (+ r532010 r538010))
    (* 2.374e-14 cache-references)
    (* 1.464e-14 cache-misses))
  "HW counters and coefficients for the Intel Sandybridge energy model.")

(defvar amd-opteron-energy-model
  '(+
    (*  4.411e-14 cycles)
    (*  2.235e-15 instructions)
    (* -8.531e-16 r533f00)
    (* -1.256e-14 cache-references)
    (* 3.679e-13  cache-misses))
  "HW counters and coefficients in the AMD Opteron energy model.")

(defvar intel-sandybridge-power-model
  '(* seconds (+ 31.530
               (*   20.490 (/ instructions cycles))
               (*    9.838 (/ (+ r532010 r538010) cycles))
               (*   -4.102 (/ cache-references cycles))
               (* 2962.678 (/ cache-misses cycles))))
  "HW counters and coefficients for the Intel Sandybridge power model.")

(defvar amd-opteron-power-model
  '(* seconds (+ 394.74
               (*   -83.68 (/ instructions cycles))
               (*    60.23 (/ r533f00 cycles))
               (*   -16.38 (/ cache-references cycles))
               (* -4209.09 (/ cache-misses cycles))))
  "HW counters and coefficients in the AMD Opteron power model.")

(defvar amd-opteron-power-model-plus
  '(* seconds (+ 3.049e+02
               (* 7.205e-06 (/ instructions cycles))
               (* 9.395e-07 (/ r533f00 cycles))
               (* 7.031e-06 (/ cache-references cycles))
               (* 1.370e-05 (/ cache-misses cycles))))
  "HW counters and coefficients in the AMD Opteron power model.
This includes evolved individuals in the training set.")


;;; Executable script driver
(defun calc-energy (&optional (args *arguments*))
  (flet ((arg-pop () (pop args)))
    (let ((help "Usage: run BENCHMARK EXECUTABLE -p|calc-energy [OPTIONS...]
 calculate the energy of a run

Options:
 -m,--model NAME ------- model name
 -d,--debug ------------ show extra output~%")
          debug)
      (in-package :optimize)
      (setf *print-pretty* t)

      ;; print help information
      (when (and (stringp (car args))
                 (ignore-errors (or (string= (subseq (car args) 0 2) "-h")
                                    (string= (subseq (car args) 0 3) "--h"))))
        (format t help)
        (quit))
      
      (getopts
       ("-m" "--model" (setf *model* (intern (string-upcase (arg-pop)))))
       ("-d" "--debug" (setf debug t)))

      (setf *model* (eval (or *model*
                              (case (arch)
                                (:intel 'intel-sandybridge-power-model)
                                (:amd   'amd-opteron-power-model)))))
      (when debug (format t "~&; model~%~S~%~%" *model*))

      ;; parse inputs
      (let ((cs (mapcar
                 (lambda-bind ((count counter))
                   (cons (make-keyword (string-upcase (symbol-name counter)))
                         count))
                 (mapcar {mapcar #'read-from-string}
                         (mapcar
                          (lambda (l) (split-sequence "," l :test #'string=))
                          (loop :for line = (read-line *standard-input* nil)
                             :while line :collect line))))))
        (when debug (format t "~&; counters~%~S~%~%" cs))

        ;; Apply the model to the counters
        (handler-case
            (multiple-value-bind (value expr) (apply-model *model* cs)
              (when debug (format t "~&; expression~%~S~%~%" expr))
              (format t "~S~%" value))
          (UNBOUND-VARIABLE (e) (format t "~S~%" e)))))))
