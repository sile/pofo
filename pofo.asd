(require :asdf)

(defsystem pofo
  :name "pofo"
  :author "Takeru Ohta"
  :version "0.0.1"
  :description "Port Forwarding"
  :serial t
  :depends-on (:sb-bsd-sockets)
  :components ((:file "package")
               (:file "util")
               (:file "socket")
               (:file "pofo")))
