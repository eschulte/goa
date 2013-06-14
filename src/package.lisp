(defpackage :optimize
  (:use :common-lisp
        :software-evolution
        :software-evolution-utility
        :alexandria
        :metabang-bind
        :curry-compose-reader-macros
        :cl-store
        :split-sequence
        :cl-ppcre
        :cl-launch
        :bordeaux-threads)
  (:shadow :type :magic-number)
  (:export
   :main
   :objread
   :calc-energy))
