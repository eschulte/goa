;;; runtime-debug.lisp --- analysis of the runtime-2 run

;; Copyright (C) 2012  Eric Schulte

;;; Code:
(require :software-evolution)
(in-package :software-evolution)
(load "./optimize.lisp")

(defvar *slow* nil "Slow individual.")
(defvar *fast* nil "Fast individual.")

(defvar *pop* (restore "../results/runtime-2/biased-pop-1.store")
  "First pop.")

(setf *fast* (car (sort *pop* #'< :key #'time-wo-init)))
(setf *slow* (lastcar (sort *pop* #'< :key #'time-wo-init)))

(time-wo-init *fast*) ;; 115079782
(time-wo-init *slow*) ;; 125461608

(history *fast*) ;; ((:CUT . 1775))

(history *slow*) ;; ((:SWAP 1872 617))

(equal-it (genome *fast*)
          (genome *slow*))

(defun mean-speed (orig &aux speeds)
  (dotimes (n 10 (mean speeds))
    (format t "run ~a~%" n)
    (let ((child (copy *fast*)))
      (evaluate child) (push (time-wo-init child) speeds))))

(defvar *speeds* (mapcar #'mean-speed (list *fast* *slow*)))

;; it looks like the speed of an offspring may rarely have anything to
;; do with the speed of the parent.
(mapcar #'float *speeds*) ;; => (7.724425e7 7.705046e7)

;; for what fields are there any variance across the population

(map 'list #'cons
     (mapcar #'float (mapcar #'mean (mapcar #'total-flits-received *pop*)))
     (mapcar #'history *pop*))
