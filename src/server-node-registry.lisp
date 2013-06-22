(in-package :epmd-server)

(defclass node-registry ()
  ((table :reader table :initform (make-hash-table))))

(defun make-node-registry ()
  (make-instance 'node-registry))

(defgeneric find-node (registry node-name)
  (:documentation "Return the node associated with node-name in registry."))

(defmethod find-node ((registry node-registry) (node-name string))
  (with-slots (table) registry
    (gethash node-name table)))

(defgeneric register-node (registry node-name node)
  (:documentation "Store node info in the registry."))

(defmethod register-node ((registry node-registry) (node-name string) node)
  (with-slots (table) registry
    (setf (gethash node-name table) node)))

(defmethod unregister-node ((registry node-registry) (node-name string))
  (with-slots (table) registry
    (remhash node-name table)))

