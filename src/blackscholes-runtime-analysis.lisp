(require :software-evolution)
(in-package :software-evolution)

;; at 2013-03-13 09:12

;;; Spread of # edits in the population
(mapcar [#'length #'edits] *population*)
;; =>
;; (9 5 9 7 1 5 9 8 3 8 8 8 9 8 5 8 3 8 7 3 6 6 8 4 1 6 7 7 8 5 7 5 8
;;  9 7 6 3 5 3 8 8 6 7 3 4 7 9 7 10 3 7 7 6 5 4 6 8 8 5 9 9 7 9 8 9 8
;;  7 8 5 7 4 8 7 8 5 8 10 6 7 5 3 9 4 9 8 7 3 8 9 7 8 1 7 7 9 7 7 3 8
;;  5 8 3 4 10 7 6 7 8 10 6 4 3 3 3 9 6 4 7 8 8 9 3 3 6 3 8 3 6 3 6 4
;;  7 7 3 7 7 3 3 9 4 8 4 6 8 4 6 4 9 3 6 9 3 5 7 4 5 6 8 8 7 7 7 8 8
;;  5 5 5 7 6 6 5 6 1 7 6 5 8 8 9 8 4 7 9 7 3 7 4 7 5 9 3 4 5 7 3 7 8
;;  3 4 5 9 6 8 7 8 4 6 8 3 7 9 6 3 3 4 5 5 3 7 6 8 8 5 6 7 9 8 4 5 4
;;  7 5 3 3 5 5 7 3 7 4 9 8 1 7 4 8 6 3 7 4 5 7 9 3 3 7 5 5 6 3 6 9 4
;;  9 5 7 4 3 2 8 6 7 5 8 8 8 8 4 9 3 6 5 8 4 3 8 7 8 1 5 3 4 8 9 3 6
;;  8 3 8 7 7 2 5 5 4 3 5 5 7 1 6 7 5 5 7 5 8 2 5 3 3 9 6 9 6 2 6 6 7
;;  6 7 7 2 3 6 8 7 5 8 1 4 7 10 6 8 3 7 6 3 9 3 4 4 7 5 3 8 4 3 3 8 6
;;  5 8 3 2 4 5 8 7 8 9 3 5 5 4 8 7 8 11 7 7 4 5 6 5 8 5 8 3 5 8 5 8 3
;;  7 9 8 5 8 5 6 8 5 7 8 7 8 9 3 8 6 7 9 8 4 3 7 3 8 7 1 8 4 8 6 8 3
;;  5 8 7 4 5 3 8 6 3 7 3 7 7 3 4 4 6 6 5 8 7 1 6 8 4 5 4 7 7 5 3 6 2
;;  8 8 7 7 5 6 4 9 6 6 7 7 3 4 2 2 8 6 5 5 7 6 2 3 5 8 6 9 5 7 1 8 7
;;  5 8 5 3 6 7 8 3 5 7 4 2 7 4 6 8 7 6)

;;; Spread of # evaluations in the population
(mapcar [#'length #'stats] *population*)
;; =>
;; (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 0 0
;;  0 0 0 0 0 0 0 0 0 2 0 3 3 0 0 0 3 1 3 1 1 0 4 2 0 0 0 0 0 0 0 2 0
;;  0 3 3 2 0 1 3 0 0 1 1 0 0 1 2 0 1 4 0 3 3 0 4 3 0 0 0 0 2 3 0 0 0
;;  3 0 4 0 0 2 1 1 1 0 1 1 4 4 3 4 1 0 3 2 0 4 3 4 1 3 1 3 0 3 2 2 0
;;  0 4 4 4 2 0 0 0 0 1 4 4 0 0 4 2 0 1 0 1 1 4 1 0 2 4 2 3 4 1 4 0 0
;;  2 4 4 3 3 3 3 4 0 0 4 1 4 1 4 5 0 2 4 0 4 4 4 3 1 4 4 4 4 0 0 2 2
;;  4 2 3 0 1 0 1 4 4 4 3 4 4 2 1 4 5 3 2 5 4 0 4 4 0 4 3 1 2 4 0 4 4
;;  1 4 4 4 4 1 4 0 4 4 1 1 1 0 0 1 4 4 4 4 0 2 1 4 0 0 1 0 3 4 0 2 4
;;  1 0 0 0 0 4 4 2 4 1 1 4 1 0 1 4 4 4 1 4 4 3 3 0 4 2 4 2 3 4 4 4 4
;;  4 0 4 3 4 0 0 1 1 0 0 3 0 4 0 1 0 2 3 4 3 0 4 4 0 0 1 4 4 4 4 0 0
;;  4 4 3 4 2 4 0 1 4 0 5 2 5 4 4 4 2 5 0 4 4 4 4 1 4 4 4 4 4 4 3 2 4
;;  2 0 4 3 3 4 0 4 3 0 4 4 4 4 0 0 0 0 2 1 0 0 0 0 0 4 4 1 1 0 4 4 0
;;  4 3 2 4 3 4 4 4 4 0 4 1 4 4 4 0 1 4 5 0 3 4 4 3 4 0 4 4 5 4 4 0 4
;;  1 4 4 1 0 4 0 4 4 4 4 1 4 0 0 0 4 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 2
;;  0 0 0 0 0 0 0 0 0 0 0 2 1 3 0 1 0 0 0 2 0 0 0 0 0 0 0 1 0 4 0 1 0
;;  0 0 0 0 5 1 1 0 0 1 1 1 0 0 0 0 0)

;;; Lowest EDP individual in the population
(extremum (mapcar [#'mean {remove-if #'zerop} {mapcar #'energy-delay-product} #'stats]
                  (remove-if [#'zerop #'length #'stats] *population*))
          #'<)
;; => 1.2144388e9

;;; EDP of the original program (averaged over 4 graphite measurements)
(mean (mapcar #'energy-delay-product (stats *orig*)))
;; => 1.2150966e9

;;; Lowest EDP individual in the population with 4 graphite measurements
(extremum (mapcar [#'mean {remove-if #'zerop} {mapcar #'energy-delay-product} #'stats]
                  (remove-if-not [{> _ 3} #'length #'stats] *population*))
          #'<)
;; => 1.2145029e9

(defvar *lowest*
  (extremum (remove-if-not [{> _ 3} #'length #'stats] *population*)
            #'<
            :key [#'mean {mapcar #'energy-delay-product} #'stats]))

(mapcar #'energy-delay-product (stats *lowest*))
(1.2153271e9 0 1.2150774e9 1.2149622e9)

(mapcar #'energy-delay-product (stats *orig*))
(1.2153262e9 1.2150486e9 1.2149624e9 1.2150488e9)

(edits *lowest*)
;; ((:CUT 687) (:INSERT 4060 2736) (:CUT 5251) (:SWAP 1857 1551) (:CUT 4490))

(prog1 :done
  (store *lowest* (format nil "bs-results/lowest-at-~d.store" *fitness-evals*)))
