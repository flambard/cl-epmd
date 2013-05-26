(in-package :epmd-test)

(in-suite epmd-protocol)

(test alive2-request
  (is (typep
       (with-input-from-sequence
           (in (with-output-to-sequence (out)
                 (write-alive2-request out "test@example" 3456)))
         (read-request in))
       'alive2-request))
  )

(test alive2-response
  (is (typep
       (with-input-from-sequence
           (in (with-output-to-sequence (out)
                 (write-alive2-response out 0)))
         (read-alive2-response in))
       'alive2-response))
  )

(test port-please2-request
  (is (typep
       (with-input-from-sequence
           (in (with-output-to-sequence (out)
                 (write-port-please2-request out "test@example")))
         (read-request in))
       'port-please2-request))
  )

(test port2-response
  (is (typep
       (with-input-from-sequence
           (in (with-output-to-sequence (out)
                 (write-port2-null-response out)))
         (read-port2-response in))
       'port2-response))

  (is (typep
       (with-input-from-sequence
           (in (with-output-to-sequence (out)
                 (write-port2-node-info-response out "test@example" 3456)))
         (read-port2-response in))
       'port2-response))
  )

(test names-request
  (is (typep
       (with-input-from-sequence
           (in (with-output-to-sequence (out)
                 (write-names-request out)))
         (read-request in))
       'names-request))
  )

(test names-response
  ;; DOES NOT WORK YET
  ;; (is (typep
  ;;      (with-input-from-sequence
  ;;          (in (with-output-to-sequence (out)
  ;;                (write-names-response out 3456 "hello world")))
  ;;        (read-names-response in))
  ;;      'names-response))
  )

(test dump-request
  )

(test dump-response
  )

(test kill-request
  )

(test kill-response
  )

(test stop-request
  )

(test stop-response
  )

(test stop-notok-response
  )
