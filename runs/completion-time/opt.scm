;;; optimize.scm --- optimize metrics in a population of software variants

;; Copyright (C) 2012  Eric Schulte

;;; Commentary:

;; Starting with an initial software object, generate a population of
;; variant implementations and then evolve to optimize some metric
;; such as fastest execution, least communication, lowest energy
;; consumption etc...

;;; Code:
(use-modules
 (srfi srfi-1) (srfi srfi-11) (srfi srfi-69) (srfi srfi-88)
 (ice-9 match) (ice-9 format)
 (sevo utility) (sevo sevo))

(define-syntax unwind-msg-protect
  (syntax-rules ()
    "Do BODY-FORM, protecting with UNWIND-FORMS."
    ((unwind-protect msg body-form unwind-forms ...)
     (catch #t
       (lambda () (prog1 body-form unwind-forms ...))
       (lambda args
         (format #t ";; [Error ~s] ~S" msg args)
         unwind-forms ... (apply throw args))))))

(define cache-file "run.cache.gz")
(define num-threads 48)
(define program "blackscholes")
(define source (from-file "blackscholes.c"))
(define original '((edit-history source) (cflags "2>/dev/null")))

(define evaluate
  (memoize 'evaluate
    (lambda (variant)
      (let ((empty? (lambda (el) (member el (list "" "\n" "\t" "\r")))))
        (apply values
          (with-temp-file-of (path "/tmp/clang-mutate-" ".c" (genome variant))
            (call-with-values
                (lambda () (command-to-string "host-test" program path))
              (lambda (stdout err)
                (list
                 (map (lambda (line)
                        (let ((split (remove empty?
                                             (string-split line #\space))))
                          (cons (string->keyword (car split))
                                (map (lambda (cell) (or (string->number cell) cell))
                                     (cdr split)))))
                      (remove empty? (string-split stdout #\newline)))
                 err)))))))))

(define (multi-obj-fitness variant)
  "Calculate the total combined fitness of PHENOME based on `evaluate' output."
  (let-values (((metrics err) (evaluate variant)))
    (let ((vals (assoc-ref metrics #:completion-time)))
      (if (not (zero? err)) 0
          (if (all number? vals)
              (/ 1 (apply max vals))
              (begin (format #t "bad for completion-time:~a~%~a"
                             vals variant)
                     0))))))

(when (file-exists? cache-file)
  (read-memoized cache-file))

(evolve (repeatedly 20 (rand-mutate 0.8 original)) multi-obj-fitness
        #:max-gen 2
        #:num-threads num-threads)

(write-memoized cache-file)
