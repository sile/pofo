(require :asdf)

(defsystem pofo
  :name "pofo"
  :author "Takeru Ohta"
  :version "0.0.2"
  :description "Port Forwarding"
  :serial t
  :depends-on (:sb-bsd-sockets :sb-concurrency)
  :components ((:file "package")
               (:file "util")
               (:file "socket")
               (:file "pofo")))
