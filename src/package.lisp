(defpackage :goa
  (:use :common-lisp
        :alexandria
        :metabang-bind
        :curry-compose-reader-macros
        :split-sequence
        :cl-store
        :cl-ppcre
        :bordeaux-threads
        :diff
        :delta-debug
        :software-evolution
        :software-evolution-utility)
  (:import-from :statistics :t-test-two-sample-on-sequences)
  (:shadow :type :magic-number)
  (:export
   :goa
   :objread
   :annotate
   :delta))
