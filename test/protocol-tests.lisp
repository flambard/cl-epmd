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
  (is (typep
       (with-input-from-sequence
           (in (with-output-to-sequence (out)
                 (write-names-response out 3456 "hello world")))
         (read-names-response in))
       'names-response))
  )

(test dump-request
  (is (typep
       (with-input-from-sequence
           (in (with-output-to-sequence (out)
                 (write-dump-request out)))
         (read-request in))
       'dump-request))
  )

(test dump-response
  (is (typep
       (with-input-from-sequence
           (in (with-output-to-sequence (out)
                 (write-dump-response out 3456 "hello world")))
         (read-dump-response in))
       'dump-response))
  )

(test kill-request
  (is (typep
       (with-input-from-sequence
           (in (with-output-to-sequence (out)
                 (write-kill-request out)))
         (read-request in))
       'kill-request))
  )

(test kill-response
  (is (typep
       (with-input-from-sequence
           (in (with-output-to-sequence (out)
                 (write-kill-response out)))
         (read-kill-response in))
       'kill-response))
  )

(test stop-request
  (is (typep
       (with-input-from-sequence
           (in (with-output-to-sequence (out)
                 (write-stop-request out "test@example")))
         (read-request in))
       'stop-request))
  )

(test stop-response
  (is (typep
       (with-input-from-sequence
           (in (with-output-to-sequence (out)
                 (write-stop-ok-response out)))
         (read-stop-response in))
       'stop-ok-response))
  )

(test stop-notok-response
  (is (typep
       (with-input-from-sequence
           (in (with-output-to-sequence (out)
                 (write-stop-not-ok-response out)))
         (read-stop-response in))
       'stop-not-ok-response))
  )
