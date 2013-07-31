(defsystem :optimize
  :description "optimize extant software."
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on (alexandria
               metabang-bind
               curry-compose-reader-macros
               software-evolution
               software-evolution-utility
               cl-store
               split-sequence
               cl-ppcre
               cl-launch
               bordeaux-threads
               iolib)
  :components
  ((:file "src/package")
   (:file "src/optimize"       :depends-on ("src/package"))
   (:file "src/annotate"       :depends-on ("src/package" "src/optimize"))
   (:file "src/objread"        :depends-on ("src/package" "src/optimize"))
   (:file "src/calc-energy"    :depends-on ("src/package" "src/optimize"))
   (:file "src/io"             :depends-on ("src/package" "src/optimize"))
   (:file "src/horizontal-gene-transfer"
          :depends-on ("src/package" "src/optimize"))))
