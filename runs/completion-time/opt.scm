;;; optimize.scm --- optimize metrics in a population of software variants

;; Copyright (C) 2012  Eric Schulte

;;; Commentary:

;; Starting with an initial software object, generate a population of
;; variant implementations and then evolve to optimize some metric
;; such as fastest execution, least communication, lowest energy
;; consumption etc...

;;; TODO:

;; - evolution: abort if everyone in the population has 0 fitness
;; - slow down the mutation rate
;; - possibly increase the tournament rate
;; - make sure we catch *every* error

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
                 (if (string? stdout)
                     (map (lambda (line)
                            (let ((split (remove empty?
                                                 (string-split line #\space))))
                              (cons (string->keyword (car split))
                                    (map (lambda (c) (or (string->number c) c))
                                         (cdr split)))))
                          (remove empty? (string-split stdout #\newline)))
                     #f)
                 err)))))))))

(define (multi-obj-fitness variant)
  "Calculate the total combined fitness of VARIANT based on `evaluate' output."
  (catch #t
    (lambda ()
      (let-values (((stdout err) (evaluate variant)))
        (catch #t
          (lambda ()
            (let ((fail-with
                   (lambda (fmt . args)
                     (apply format #t
                            (string-concatenate (cons "(mof ~S) " fmt))
                            (cons stdout err) args) 0)))
              (cond
               ((not (number? err))
                (fail-with "non-numeric err: ~a~%" err))
               ((not (zero? err)) 0)
               ((not (list? stdout))
                (fail-with "mangled STDOUT: ~a~%" stdout))
               ((all number? (assoc-ref stdout #:completion-time))
                (/ 1 (apply max (assoc-ref stdout #:completion-time))))
               (else
                (fail-with "bad metrics: ~a~%"
                           (assoc stdout #:completion-time))))))
          (lambda (key . args)
            (fail-with "~S: ~S~%" key args)))))
    (lambda args
      (format #t "failure in evaluate: ~S" args) 0)))

(when (file-exists? cache-file)
  (read-memoized cache-file))

(evolve (repeatedly 100 (rand-mutate 0.8 original)) multi-obj-fitness
        #:mut-p 0.2
        #:cross-p 0.2
        #:tournament-size 6
        #:max-gen 250
        #:num-threads #f)

(write-memoized cache-file)
