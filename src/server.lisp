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
;;; NODE class
;;;

(defclass node ()
  ((name            :initarg :name            :reader name)
   (port            :initarg :port            :reader port)
   (type            :initarg :node-type       :initform :erlang)
   (protocol        :initarg :protocol        :initform :tcpip4)
   (highest-version :initarg :highest-version :initform 5)
   (lowest-version  :initarg :lowest-version  :initform 5)
   (extra           :initarg :extra           :initform ""))
  (:documentation "A registered node."))


;;;
;;; Generic function CLOSE-CONNECTION
;;;

(defgeneric close-connection (connection)
  (:documentation "Close the client connection."))


;;;
;;; Generic function KILL-SERVER
;;;

(defgeneric kill-server (server)
  (:documentation "Kill the EPMD server."))


;;;
;;; Generic function RESPONSE
;;;

(defgeneric response (server request)
  (:documentation "Returns the appropriate response for the incoming request."))


(defmethod response ((server epmd-server) (request alive2-request))
  (with-slots
        (name port node-type protocol highest-version lowest-version extra)
      request
    (let ((node (make-instance 'node
                               :name name
                               :port port
                               :node-type node-type
                               :protocol protocol
                               :highest-version highest-version
                               :lowest-version lowest-version
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
        (with-slots (port type protocol highest-version lowest-version extra)
            node
          (make-port2-node-info-response node-name
                                         port
                                         :node-type type
                                         :protocol protocol
                                         :highest-version highest-version
                                         :lowest-version lowest-version
                                         :extra extra)) )))

(defmethod response ((server epmd-server) (request names-request))
  (let* ((node-strings (loop
                          for node in (get-all-nodes (registered-nodes server))
                          collect (format nil "name ~a at port ~a~&"
                                          (name node) (port node))))
         (node-info (format nil "~{~a~}" node-strings)))
    (make-names-response (listen-port server) node-info)))

(defmethod response ((server epmd-server) (request dump-request))
  ;; TODO: Return a dump(?) of all registered nodes
  (let ((dump ""))
    (make-dump-response (listen-port server) dump)))

(defmethod response ((server epmd-server) (request kill-request))
  (make-kill-response))

(defmethod response ((server epmd-server) (request stop-request))
  (make-stop-not-ok-response))


;;;
;;; Generic function POST-RESPONSE-ACTION
;;;

(defgeneric post-response-action (server response connection)
  (:documentation "Action to perform after sending a response."))


(defmethod post-response-action (server response connection)
  (declare (ignore server response))
  ;; Default action: Close the connection
  (close-connection connection))

(defmethod post-response-action (server (response alive2-response) connection)
  (declare (ignore server response connection))
  ;; Do NOT close the connection!
  )

(defmethod post-response-action (server (response kill-response) connection)
  (declare (ignore response connection))
  (kill-server server))
