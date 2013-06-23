(in-package :epmd-test)

(in-suite epmd-protocol)

(test alive2-request
  (is (typep
       (with-input-from-sequence
           (in (with-output-to-sequence (out)
                 (write-message out (make-alive2-request "test@example" 3456))))
         (read-request in))
       'alive2-request))
  )

(test alive2-response
  (is (typep
       (with-input-from-sequence
           (in (with-output-to-sequence (out)
                 (write-message out (make-alive2-response 0))))
         (read-alive2-response in))
       'alive2-response))
  )

(test port-please2-request
  (is (typep
       (with-input-from-sequence
           (in (with-output-to-sequence (out)
                 (write-message out (make-port-please2-request
                                     "test@example"))))
         (read-request in))
       'port-please2-request))
  )

(test port2-response
  (is (typep
       (with-input-from-sequence
           (in (with-output-to-sequence (out)
                 (write-message out (make-port2-null-response))))
         (read-port2-response in))
       'port2-response))

  (is (typep
       (with-input-from-sequence
           (in (with-output-to-sequence (out)
                 (write-message out (make-port2-node-info-response
                                     "test@example" 3456))))
         (read-port2-response in))
       'port2-response))
  )

(test names-request
  (is (typep
       (with-input-from-sequence
           (in (with-output-to-sequence (out)
                 (write-message out (make-names-request))))
         (read-request in))
       'names-request))
  )

(test names-response
  (is (typep
       (with-input-from-sequence
           (in (with-output-to-sequence (out)
                 (write-message out (make-names-response 3456 "hello world"))))
         (read-names-response in))
       'names-response))
  )

(test dump-request
  (is (typep
       (with-input-from-sequence
           (in (with-output-to-sequence (out)
                 (write-message out (make-dump-request))))
         (read-request in))
       'dump-request))
  )

(test dump-response
  (is (typep
       (with-input-from-sequence
           (in (with-output-to-sequence (out)
                 (write-message out (make-dump-response 3456 "hello world"))))
         (read-dump-response in))
       'dump-response))
  )

(test kill-request
  (is (typep
       (with-input-from-sequence
           (in (with-output-to-sequence (out)
                 (write-message out (make-kill-request))))
         (read-request in))
       'kill-request))
  )

(test kill-response
  (is (typep
       (with-input-from-sequence
           (in (with-output-to-sequence (out)
                 (write-message out (make-kill-response))))
         (read-kill-response in))
       'kill-response))
  )

(test stop-request
  (is (typep
       (with-input-from-sequence
           (in (with-output-to-sequence (out)
                 (write-message out (make-stop-request "test@example"))))
         (read-request in))
       'stop-request))
  )

(test stop-response
  (is (typep
       (with-input-from-sequence
           (in (with-output-to-sequence (out)
                 (write-message out (make-stop-ok-response))))
         (read-stop-response in))
       'stop-ok-response))
  )

(test stop-notok-response
  (is (typep
       (with-input-from-sequence
           (in (with-output-to-sequence (out)
                 (write-message out (make-stop-not-ok-response))))
         (read-stop-response in))
       'stop-not-ok-response))
  )
