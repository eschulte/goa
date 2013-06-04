(in-package :optimize)

(setf
 *orig* (from-file (make-instance 'asm-perf) "benchmarks/h264ref/h264ref.s")
 *benchmark* "h264ref"
 (flags *orig*) '("-lm" "-O3")
 *model* (case (arch)
           (:intel 'intel-sandybridge-energy-model)
           (:amd   'amd-opteron-energy-model))
 *model* (eval *model*)
 (fitness *orig*) (test *orig*)
 *max-population-size* (expt 2 4)
 *population* (loop :for n :below *max-population-size*
                 :collect (copy *orig*)))

(sb-thread:make-thread (evolve #'test :evals 256))

(defun status ()
  (room)
  (flet ((size (it) (length (to-bytes it))))
    (cons
     (size *consolidated-edits*)
     (mapcar #'size *population*))))

;; starting
;;
;; OPTIMIZE> (status)
;; Dynamic space usage is:   1,023,765,344 bytes.
;; Read-only space usage is:      5,952 bytes.
;; Static space usage is:         4,064 bytes.
;; Control stack usage is:        9,840 bytes.
;; Binding stack usage is:        1,072 bytes.
;; Control and binding stack usage is for the current thread only.
;; Garbage collection is currently enabled.
;; 
;; Breakdown for dynamic space:
;;   752,205,872 bytes for 47,012,867 cons objects.
;;   213,338,464 bytes for   311,952 simple-character-string objects.
;;   58,221,008 bytes for   502,983 other objects.
;;   1,023,765,344 bytes for 47,827,802 dynamic objects (space total.)
;; (5 8331310 8331405 8331064 8331247 8331273 8331314 8331284 8331276 8331271
;;  8331246 8331256 8331246 8331246 8331260 8331247 8331208)
;;
;; ran 257 fitness evals
;; 
;; OPTIMIZE> (status)
;; Dynamic space usage is:   1,483,890,288 bytes.
;; Read-only space usage is:      5,952 bytes.
;; Static space usage is:         4,064 bytes.
;; Control stack usage is:        9,840 bytes.
;; Binding stack usage is:        1,072 bytes.
;; Control and binding stack usage is for the current thread only.
;; Garbage collection is currently enabled.
;; 
;; Breakdown for dynamic space:
;;   1,195,029,584 bytes for 74,689,349 cons objects.
;;   204,508,784 bytes for   311,727 simple-character-string objects.
;;   84,481,664 bytes for 1,279,239 other objects.
;;   1,484,020,032 bytes for 76,280,315 dynamic objects (space total.)
;; (5 8331345 8331236 8331330 8331407 8331448 8331444 8331200 8331305 8331403
;;  8331272 8331416 8331356 8331310 8331417 8331288 8331314)
;; OPTIMIZE> *fitness-evals*
;; 257
;; OPTIMIZE> (sb-ext:gc :full t)
;; NIL
;; OPTIMIZE> (status)
;; Dynamic space usage is:   544,252,144 bytes.
;; Read-only space usage is:      5,952 bytes.
;; Static space usage is:         4,064 bytes.
;; Control stack usage is:        9,840 bytes.
;; Binding stack usage is:        1,072 bytes.
;; Control and binding stack usage is for the current thread only.
;; Garbage collection is currently enabled.
;;
;; Breakdown for dynamic space:
;;   455,504,176 bytes for 28,469,011 cons objects.
;;   31,815,968 bytes for   310,773 simple-character-string objects.
;;   56,932,000 bytes for   485,973 other objects.
;;   544,252,144 bytes for 29,265,757 dynamic objects (space total.)
;; (5 8331345 8331236 8331330 8331407 8331448 8331444 8331200 8331305 8331403
;;  8331272 8331416 8331356 8331310 8331417 8331288 8331314)
