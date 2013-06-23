(in-package :epmd-test)

(in-suite epmd-server)

(test node-registry

  ;; Create a node registry
  (is (typep (epmd-server::make-node-registry) 'epmd-server::node-registry))

  ;; Trying to find an unregistered node
  (is (null (epmd-server::find-node (epmd-server::make-node-registry)
                                    "doesnt@exist")))

  ;; Registering a new node
  (is (let ((registry (epmd-server::make-node-registry))
            (node "Example node")
            (node-name "node@example.com"))
        (epmd-server::register-node registry node-name node)
        (string= node (epmd-server::find-node registry node-name))))

  ;; Replace an existing node with a new node
  (is (let ((registry (epmd-server::make-node-registry))
            (old-node "Old node")
            (new-node "New node")
            (node-name "node@example.com"))
        (epmd-server::register-node registry node-name old-node)
        (epmd-server::register-node registry node-name new-node)
        (string= new-node (epmd-server::find-node registry node-name))))

  ;; Unregister a node
  (is (let ((registry (epmd-server::make-node-registry))
            (node "Example node")
            (node-name "node@example.com"))
        (epmd-server::register-node registry node-name node)
        (epmd-server::unregister-node registry node-name)
        (null (epmd-server::find-node (epmd-server::make-node-registry)
                                      node-name))))
  )


(test alive2-request-response

  (let* ((server (make-instance 'epmd-server::epmd-server))
         (node-name "node@example.com")
         (node-port 1234)
         (request (make-alive2-request node-name node-port))
         (response (epmd-server::response server request)))
    (is (typep response 'alive2-response)))

  (let* ((server (make-instance 'epmd-server::epmd-server))
         (node-name "node@example.com")
         (node-port 1234)
         (request (make-alive2-request node-name node-port))
         (response (epmd-server::response server request)))
    (is (= 0 (result response))))

  (let* ((server (make-instance 'epmd-server::epmd-server))
         (node-name "node@example.com")
         (node-port 1234)
         (request (make-alive2-request node-name node-port))
         (response (epmd-server::response server request))
         (registry (epmd-server::registered-nodes server)))
    (is (and (typep response 'alive2-response)
             (typep (epmd-server::find-node registry node-name)
                    'epmd-server::node))))
  )


(test port-please2-request-response

  (let* ((server (make-instance 'epmd-server::epmd-server))
         (request (make-port-please2-request "node@example.com"))
         (response (epmd-server::response server request)))
    (is (typep response 'port2-null-response)))

  (let* ((server (make-instance 'epmd-server::epmd-server))
         (node-name "node@example.com")
         (node (make-instance 'epmd-server::node
                              :name node-name
                              :port 1234)))
    (epmd-server::register-node (epmd-server::registered-nodes server)
                                node-name
                                node)
    (let* ((request (make-port-please2-request node-name))
           (response (epmd-server::response server request)))
      (is (typep response 'port2-node-info-response))))

  )
