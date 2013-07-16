(defsystem :optimize
  :description "optimize extant software."
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on (software-evolution
               cl-store
               split-sequence
               cl-ppcre
               cl-launch
               bordeaux-threads)
  :components
  ((:file "src/package")
   (:file "src/optimize"       :depends-on ("src/package"))
   (:file "src/run-optimize"   :depends-on ("src/package" "src/optimize"))
   (:file "src/calc-energy"    :depends-on ("src/package" "src/optimize"))
   (:file "src/objread"        :depends-on ("src/package" "src/optimize"))
   (:file "src/model-variance" :depends-on ("src/package" "src/optimize"))
   (:file "src/annotate"       :depends-on ("src/package" "src/optimize"))))
