;;; energy-models.lisp --- trained energy models

;; Copyright (C) 2013  Eric Schulte

;;; Commentary:

;; Load this file with the -C option to define two energy model
;; fitness functions.  These models were trained against a Intel i7
;; machine with 4 physical cores, hyper-threading and 8G of memory,
;; and against an AMD Opteron machine with 48 cores and 120G of
;; memory.

;;; Code:
(in-package :optimize)

;;; Models
(defvar intel-sandybridge-power-model
  '(* seconds (+ 31.530
               (*   20.490 (/ instructions cycles))
               (*    9.838 (/ (+ r532010 r538010) cycles))
               (*   -4.102 (/ cache-references cycles))
               (* 2962.678 (/ cache-misses cycles))))
  "HW counters and coefficients for the Intel Sandybridge power model.")

(defvar amd-opteron-power-model
  '(* seconds (+ 394.74
               (*   -83.68 (/ instructions cycles))
               (*    60.23 (/ r533f00 cycles))
               (*   -16.38 (/ cache-references cycles))
               (* -4209.09 (/ cache-misses cycles))))
  "HW counters and coefficients in the AMD Opteron power model.")
