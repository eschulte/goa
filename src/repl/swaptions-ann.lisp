;;; swaptions-ann.lisp --- Optimize the swaptions program using annotations

;; Copyright (C) 2013  Eric Schulte

;;; Code:
(in-package :optimize)
(load "src/configs/use-annotation.lisp")
(load "src/configs/energy-models.lisp")
(load "src/repl/helpers.lisp")

;; (1) create an annotated swaptions software object
;;    $ annotate "run swaptions ~a -a" benchmarks/swaptions/swaptions.s \
;;        -l g++ -L -lpthread -s -o benchmarks/swaptions/swaptions.store

;; (setf *orig* (restore "benchmarks/swaptions/swaptions.store"))
(setf *orig* (to-ann-range (restore "benchmarks/swaptions/swaptions.store")))

;; (2) customize the around method to `apply-mutation' and the
;;     `pick-bad' function to collect mutation information.
(defvar picked nil)

(defmethod pick-bad ((asm simple))
  (let ((id (pick asm [{+ 0.01} {aget :annotation}])))
    (push id picked) id))

(defmethod pick-bad ((asm ann-range))
  (let ((id (proportional-pick (annotations asm) #'identity)))
    (push id picked) id))

(defvar mutations nil)
(push (lambda (it) (push it mutations)) *mutation-hooks*)

;; (3) Setup
(setf
 *work-dir* "sh-runner/work"            ; use external script execution
 *evals* (expt 2 18)                    ; max runtime in evals
 *threads* 12                           ; number of threads
 *script* "run swaptions ~a -p"         ; test script
 *max-population-size* (expt 2 8)       ; max pop size
 *fitness-predicate* #'<                ; fitness function stuff
 *fitness-function* (case (arch)
                      (:intel intel-sandybridge-power-model)
                      (:amd amd-opteron-power-model))
 (fitness *orig*) (test *orig*))        ; original fitness

;; (4) Sanity check
(assert (not (equal infinity (fitness *orig*))) (*fitness-function* *orig*)
        "Original program has bad fitness.~%fitness function:~%~Sstats:~%~S"
        *fitness-function*
        (stats *orig*))

;; (5) Populate
(setf
 #+sbcl (sb-ext:bytes-consed-between-gcs) (expt 2 24)
 *population* (loop :for n :below *max-population-size* :collect (copy *orig*)))

;; (6) Run
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
     (push (make-thread (lambda () (evolve #'test :max-evals *evals*))
                        :name (format nil "opt-~d" n))
           threads))
  ;; wait for all threads to return
  (mapc #'join-thread threads))
