;;; dist-conf.lisp --- configuration file for by-flag sub-pop optimization

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
(load "src/io.lisp")

(defvar ports
  '((O0 . 4000)
    (O1 . 4001)
    (O2 . 4002)
    (O3 . 4003)
    (Os . 4004))
  "Map compiler flags to ports for socket communication.")

;; Mark the genome in `*orig*' with the compiler flag used.
(setf (genome *orig*)
      (mapcar (lambda (l) (cons (list :flag flag) l)) (genome *orig*)))

;; Set the results directory.
(setq *res-dir* (append *res-dir* (list (symbol-name flag))))

;; Periodically share individuals with a neighbor.
(defun sharing-checkpoint ()
  ;; write out composition information
  (let ((cmp-file (make-pathname :directory *res-dir* :name "cmp" :type "lisp"))
        (cmps (mapcar [#'counts {mapcar [#'second {assoc :flag}]} #'genome]
                      *population*)))
    (with-open-file (out cmp-file
                         :direction :output
                         :if-exists :append
                         :if-does-not-exist :create)
      (format out "~S~%" (cons *fitness-evals* cmps))))
  ;; Share an individual with a neighbor 1/4 checkpoints.
  (let ((share-roll (random 4)))
    (when (zerop share-roll)
      (let ((share-to (random-elt (remove-if [{equal flag} #'car] ports))))
        (share (tournament) :port (cdr share-to))))))

(setf *checkpoint-funcs*
      (append *checkpoint-funcs* (list #'sharing-checkpoint)))

;; Lets try to get some more intra-individual flag mixing
(defun heros-welcome (ind)
  (mapc [#'incorporate {crossover ind}]
        (loop :for i :below 12 :collect (tournament))))

;; Begin listening for shared individuals on flag-specified port.
(sb-thread:make-thread (lambda () (accept :port (aget flag ports)
                                     :add-fn #'heros-welcome)))
