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
