(require :software-evolution)
(in-package :software-evolution)
(mapc (lambda (pkg) (require pkg) (use-package pkg))
      (list :cl-ppcre :curry-compose-reader-macros))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))

(defun parse-stdout (stdout)
  "Parse the Graphite output of host-test."
  (remove
      nil
      (mapcar
       (lambda (line)
         (let ((fields (split-sequence #\Space (regex-replace-all "" line "")
                                       :remove-empty-subseqs t)))
           (unless (null fields)
             (cons (make-keyword (string-upcase (car fields)))
                   (mapcar (lambda (c) (or (ignore-errors (parse-number c)) c))
                           (cdr fields))))))
       (remove-if (lambda (line) (or (scan "hooks" line)
                                (scan "warning" line)
                                (scan "spawn_master" line)))
                  (split-sequence #\Newline stdout :remove-empty-subseqs t)))))

(defun group-stats (stats &aux group results)
  (dolist (row stats (reverse (cons (reverse group) results)))
    (case (car row)
      ((:tile-summary :core-model-summary :network-summary :cache-summary)
       (setf group nil))
      ((:cache-l1-i :cache-l2 :cache-l1-d :dram-performance-model-summary)
       (when group (push (reverse group) results))
       (setf group row))
      (:network-model
       (when group
         (push (reverse group) results)
         (setf group nil))
       (push row group))
      (t
       (if group
           (push row group)
           (push row results))))))

(defun energy-delay-product (stats)
  (flet ((energy (group)
           (+ (reduce #'+ (cdr (assoc :static-power group)))
              (reduce #'+ (cdr (assoc :dynamic-energy group))))))
    (*
     ;; Runtime
     (reduce #'+ (aget :completion-time stats))
     ;; Energy
     (reduce #'+ (mapcar #'energy
                         (cons
                          (aget '(:network-model 2) stats :test #'tree-equal)
                          (mapcar {aget _ stats}
                                  '(:cache-l1-i :cache-l1-d :cache-l2
                                    :dram-performance-model-summary))))))))

(defvar *origs*
  (loop :for i :below 100 :collect
     (energy-delay-product
      (group-stats (parse-stdout (file-to-string
                                  (format nil "0-000-~2,'0d.graphite" i)))))))

(defvar *vars*
  (loop :for i :below 100 :collect
     (energy-delay-product
      (group-stats (parse-stdout (file-to-string
                                  (format nil "4-073-~2,'0d.graphite" i)))))))

(mean *vars*)

;; *vars* 9.0727464e7 +- 391684.53
;; *origs* 9.074615e7 +- 435647.53
