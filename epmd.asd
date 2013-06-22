(defpackage :epmd-system
  (:use :cl))

(in-package :epmd-system)

(asdf:defsystem :epmd
  :description "Erlang Port Mapper Daemon client"
  :author "Markus Flambard <mflambard@common-lisp.net>"
  :version "0.1.0"
  :license "MIT License"
  :depends-on (:com.gigamonkeys.binary-data :usocket)
  :components
  ((:module :src
            :components
            ((:file "packages")
             (:file "protocol"
                    :depends-on ("packages"))
             (:file "client"
                    :depends-on ("packages"
                                 "protocol"))
             (:file "server-node-registry"
                    :depends-on ("packages"))
             ))))
