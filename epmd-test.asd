(asdf:defsystem :epmd-test
  :description "Unit-tests for cl-epmd."
  :depends-on (:epmd :fiveam :flexi-streams)
  :components
  ((:module :test
            :components
            ((:file "package")
             (:file "protocol-tests"
                    :depends-on ("package"))
             (:file "server-tests"
                    :depends-on ("package"))
             ))))
