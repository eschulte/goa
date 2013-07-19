;;; swaptions-spread.lisp --- generate a bunch of neutral variants of swaptions

;; Copyright (C) 2013  Eric Schulte

;;; Code:
(in-package :optimize)

(setf
 ;; original program
 *orig* (from-file (make-instance 'asm-perf)
                   "benchmarks/swaptions/swaptions.s")
 (linker *orig*) "g++"
 (flags *orig*) (list "-lpthread")
 ;; execution
 *benchmark* "swaptions")

(let ((counter 0)
      (out-file "swaptions-better-spread.lisp"))
  (loop :until (>= counter 1000) :do
     (let* ((new (copy *orig*))
            (fitness (test new)))
       (mutate new)
       (when (< fitness infinity)
         (incf counter)
         (with-open-file (out out-file :direction :output :if-exists :append
                              :if-does-not-exist :create)
           (format out "~&~s~%"
                   `((:stats ,(stats new))
                     (:fitness ,fitness)
                     (:edits ,(edits new)))))))))
