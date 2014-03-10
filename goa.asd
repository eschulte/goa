(defsystem :goa
  :description "Genetic Optimization Algorithm to optimize extant software."
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on (alexandria
               metabang-bind
               curry-compose-reader-macros
               split-sequence
               cl-store
               cl-ppcre
               bordeaux-threads
               lhstats
               diff
               delta-debug
               software-evolution
               software-evolution-utility)
  :components
  ((:file "src/package")
   (:file "src/goa-core"       :depends-on ("src/package"))
   (:file "src/goa"            :depends-on ("src/package" "src/goa-core"))
   (:file "src/annotate"       :depends-on ("src/package" "src/goa-core"))
   (:file "src/objread"        :depends-on ("src/package" "src/goa-core"))
   (:file "src/delta"          :depends-on ("src/package" "src/goa-core"))
   (:file "src/horizontal-gene-transfer"
          :depends-on ("src/package" "src/goa-core"))))
