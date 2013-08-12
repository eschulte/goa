;;; swaptions-ann.lisp --- Optimize the swaptions program using annotations

;; Copyright (C) 2013  Eric Schulte

;;; Code:
(load "src/optimize.lisp")
(in-package :optimize)

;; 1. create an annotated swaptions software object
;;    $ annotate "run swaptions ~a -a" benchmarks/swaptions/swaptions.s \
;;               -L -lm -s -o benchmarks/swaptions/swaptions.store
(setf *orig* (restore "benchmarks/swaptions/swaptions.store"))

;; 2. customize the around method to `apply-mutation' to collection
;;    mutation information.
(defvar picked nil)

(defmethod pick-bad ((asm simple))
    (let ((id (pick asm [{+ 0.01} {aget :annotation}])))
      (push id picked) id))

(defvar mutations nil)

(defmethod apply-mutation :around ((asm asm) op)
  (call-next-method)
  (push op mutations)
  (with-slots (genome) asm
    (flet ((blend (i)
             (setf (cdr (assoc :annotation (nth i genome)))
                   (mean (remove nil
                           (list (when (> i 0)
                                   (aget :annotation (nth (1- i) genome)))
                                 (aget :annotation (nth (1+ i) genome))))))))
      (case (car op)
        (:insert (blend (second op)))
        (:swap (blend (second op)) (blend (third op))))))
  asm)

;; (5) Setup configuration variables.
(setf
 *evals* (expt 2 18)                    ; max runtime in evals
 *threads* 12                           ; number of threads
 *script* "run swaptions ~a -p"         ; test script
 (fitness *orig*) (test *orig*)         ; sanity
 *max-population-size* (expt 2 8)
 ;; fitness function stuff
 *fitness-predicate* #'<
 *fitness-function* (progn (load "src/configs/energy-models.lisp")
                           amd-opteron-power-model)
 ;; fill the population with copies of the original
 *population* (loop :for n :below *max-population-size* :collect (copy *orig*)))

;; (6) Run.
(let (threads
      #+ccl
      (*default-special-bindings*
       (list (cons '*terminal-io*
                   (make-two-way-stream
                    (make-string-input-stream "y")
                    (two-way-stream-output-stream
                     *terminal-io*))))))
  ;; kick off optimization threads
  (loop :for n :below *threads* :do
     (push (make-thread (lambda () (evolve #'test :max-evals *evals*))) threads))
  ;; wait for all threads to return
  (mapc #'join-thread threads))
