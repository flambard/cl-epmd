(in-package :epmd-server)

;;;
;;; EPMD-SERVER class
;;;

(defclass epmd-server ()
  ((listen-port
    :initarg :listen-port
    :reader listen-port
    :initform 4369)
   (listen-socket
    :initarg :listen-socket
    :reader listen-socket)
   (registered-nodes
    :initform (make-node-registry)
    :accessor registered-nodes))
  (:documentation "An EPMD server."))


;;;
;;; Generic function RESPONSE
;;;

(defgeneric response (server request)
  (:documentation "Returns the appropriate response for the incoming request."))


(defmethod response ((server epmd-server) (request alive2-request))
  (with-slots
        (name port node-type protocol lowest-version highest-version extra)
      request
    (let ((node (make-instance 'node
                               :name name
                               :port port
                               :node-type node-type
                               :protocol protocol
                               :lowest-version lowest-version
                               :highest-version highest-version
                               :extra extra)))
      (register-node (registered-nodes server) name node)))
  ;; TODO: Result = 1 if not OK
  ;; TODO: Find out what creation is for
  (make-alive2-response 0))

(defmethod response ((server epmd-server) (request port-please2-request))
  (let* ((node-name (node-name request))
         (node (find-node (registered-nodes server) node-name)))
    (if (null node)
        (make-port2-null-response)
        (with-slots (port type protocol lowest-version highest-version extra)
            node
          (make-port2-node-info-response node-name
                                         port
                                         :node-type type
                                         :protocol protocol
                                         :lowest-version lowest-version
                                         :highest-version highest-version
                                         :extra extra)) )))
