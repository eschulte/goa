;;; optimize.scm --- optimize metrics in a population of software variants

;; Copyright (C) 2012  Eric Schulte

;;; Commentary:

;; Starting with an initial software object, generate a population of
;; variant implementations and then evolve to optimize some metric
;; such as fastest execution, least communication, lowest energy
;; consumption etc...

;;; Code:
(use-modules
 (srfi srfi-1) (srfi srfi-11) (srfi srfi-88)
 (ice-9 match) (ice-9 format)
 (sevo utility) (sevo sevo))

(define max-fit #f)
(define num-threads #f)
(define program "blackscholes")
(define source (from-file "../data/blackscholes.c"))
(define original '((edit-history source)))

(define evaluate
  (memoize
   (lambda (variant)
     (apply values
      (with-temp-file-of (path "/tmp/clang-mutate-" ".c" (genome variant))
        (call-with-values
            (lambda () (command-to-string "../bin/host-test" program path))
          (lambda (stdout err)
            (list
             (map (lambda (line)
                    (let ((split (delete "" (string-split line #\space))))
                      (cons (string->keyword (car split))
                            (map (lambda (cell) (or (string->number cell) cell))
                                 (cdr split)))))
                  (delete "" (string-split stdout #\newline)))
             err))))))))

(define (multi-obj-fitness variant)
  "Calculate the total combined fitness of PHENOME based on `evaluate' output."
  (let-values (((metrics err) (evaluate variant)))
    (if (not (zero? err)) 0
        (/ 1 (apply max (assoc-ref metrics #:completion-time))))))

(evolve (repeatedly 10 (rand-mutate 0.8 original)) multi-obj-fitness max-fit
        #:num-threads num-threads)
