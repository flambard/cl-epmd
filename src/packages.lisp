(defpackage #:epmd-protocol
  (:documentation "EPMD network protocol functions.")
  (:use #:cl
        #:com.gigamonkeys.binary-data
        #:com.gigamonkeys.binary-data.common-datatypes)
  (:export

   ;; Client functions
   #:read-alive2-response
   #:read-port2-response
   #:read-names-response
   #:read-dump-response
   #:read-kill-response
   #:read-stop-response
   #:write-alive2-request
   #:write-port-please2-request
   #:write-names-request
   #:write-dump-request
   #:write-kill-request
   #:write-stop-request

   ;; Server functions
   #:read-request
   #:write-alive2-response
   #:write-port2-node-info-response
   #:write-port2-null-response
   #:write-names-response
   #:write-dump-response
   #:write-kill-response
   #:write-stop-ok-response
   #:write-stop-not-ok-response

   ;; Classes
   #:epmd-request
   #:alive2-request
   #:alive2-response
   #:port-please2-request
   #:port2-response
   #:port2-node-info-response
   #:port2-null-response
   #:names-request
   #:names-response
   #:dump-request
   #:dump-response
   #:kill-request
   #:kill-response
   #:stop-request
   #:stop-response
   #:stop-ok-response
   #:stop-not-ok-response

   #:creation
   #:port
   #:node-info
   #:node-name
   #:node-host
   #:node-type
   #:node-protocol
   #:node-lowest-version
   #:node-highest-version
   #:node-extra-field

   ))

(defpackage #:epmd-client
  (:documentation "EPMD (Erlang Port Mapper Daemon) client.")
  (:use #:cl #:epmd-protocol #:usocket)
  (:export

   #:epmd-connection
   #:publish
   #:published-node-name
   #:published-node-port
   #:published-p
   #:unpublish

   #:lookup-node
   #:print-all-registered-nodes

   #:node-info
   #:node-name
   #:node-host
   #:node-port
   #:node-type
   #:node-protocol
   #:node-lowest-version
   #:node-highest-version
   #:node-extra-field

   #:already-registered
   #:unreachable-error
   #:host-unknown-error

   ))

(defpackage #:epmd-server
  (:documentation "EPMD server.")
  (:use #:cl #:epmd-protocol #:usocket)
  (:export

   #:start

   ))
