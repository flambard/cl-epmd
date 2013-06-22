;;;; The EPMD protocol

(in-package :epmd-protocol)


(defun protocol-keyword-to-integer (keyword)
  (ecase keyword
    (:tcpip4 0)))

(defun protocol-integer-to-keyword (integer)
  (ecase integer
    (0 :tcpip4)))


(defun node-type-keyword-to-integer (keyword)
  (ecase keyword
    (:hidden 72)
    (:erlang 77)))

(defun node-type-integer-to-keyword (integer)
  (ecase integer
    (72 :hidden)
    (77 :erlang)))


(defun response-class-tag (class)
  (ecase class
    (alive2-response 121)
    (port2-response 119)))

(defun request-class-tag (class)
  (ecase class
    (alive2-request       120)
    (port-please2-request 122)
    (names-request        110)
    (dump-request         100)
    (kill-request         107)
    (stop-request         115)))

(defun find-request-class (tag)
  (ecase tag
    (120 'alive2-request)
    (122 'port-please2-request)
    (110 'names-request)
    (100 'dump-request)
    (107 'kill-request)
    (115 'stop-request)))

(define-tagged-binary-class epmd-request ()
  ((size u2)
   (tag  u1))
  (:dispatch (find-request-class tag)))

(defun read-request (stream)
  (read-value 'epmd-request stream))


;;;
;;; ALIVE2_REQ
;;
;; 2 bytes: Total length of following message in bytes
;; 1 byte:  'x'               [ALIVE2_REQ message]
;; 2 bytes: Listening port
;; 1 byte:  72                [hidden node (not Erlang node)]
;; 1 byte:  0                 [protocol: tcp/ip v4]
;; 2 bytes: 5                 [lowest version supported]
;; 2 bytes: 5                 [highest version supported]
;; 2 bytes: Length of node name
;; N bytes: Node name
;; 2 bytes: Length of the Extra field
;; M bytes: Extra             [???]
;;

(define-binary-class alive2-request (epmd-request)
  ((port            u2)
   (node-type       u1)
   (protocol        u1)
   (lowest-version  u2)
   (highest-version u2)
   (name-length     u2)
   (name            (iso-8859-1-string :length name-length))
   (extra-length    u2)
   (extra           (iso-8859-1-string :length extra-length))))

(defun write-alive2-request (stream node-name node-port &key
                             (node-type :hidden)
                             (protocol :tcpip4)
                             (lowest-version 5)
                             (highest-version 5)
                             (extra ""))
  (let* ((node-name-length (length node-name))
         (extra-length (length extra))
         (message-length (+ 13 node-name-length extra-length))
         (node-type-integer (node-type-keyword-to-integer node-type))
         (protocol-integer (protocol-keyword-to-integer protocol)))
    (write-value 'alive2-request
                 stream
                 (make-instance 'alive2-request
                                :size message-length
                                :tag (request-class-tag 'alive2-request)
                                :port node-port
                                :node-type node-type-integer
                                :protocol protocol-integer
                                :lowest-version lowest-version
                                :highest-version highest-version
                                :name-length node-name-length
                                :name node-name
                                :extra-length extra-length
                                :extra extra))))


;;;
;;; ALIVE2_RESP
;;
;; 1 byte:  'y'               [ALIVE2_RESP message]
;; 1 byte:  Result            [0 means OK, >0 means ERROR]
;; 2 bytes: Creation          [?]
;;

(define-binary-class alive2-response ()
  ((tag      u1)
   (result   u1)
   (creation u2)))

(defun read-alive2-response (stream)
  (read-value 'alive2-response stream))

(defun write-alive2-response (stream result &optional (creation 0))
  (write-value 'alive2-response
               stream
               (make-instance 'alive2-response
                              :tag (response-class-tag 'alive2-response)
                              :result result
                              :creation creation)))


;;;
;;; PORT_PLEASE2_REQ
;;
;; 2 bytes: Total length of following message
;; 1 byte:  'z'            [PORT_PLEASE2_REQ message]
;; N bytes: Node name
;;

(define-binary-class port-please2-request (epmd-request)
  ((node-name (iso-8859-1-string :length (1- size)))))

(defun write-port-please2-request (stream node-name)
  (let ((message-length (1+ (length node-name))))
    (write-value 'port-please2-request
                 stream
                 (make-instance 'port-please2-request
                                :size message-length
                                :tag (request-class-tag 'port-please2-request)
                                :node-name node-name))))


;;;
;;; PORT2_RESP
;;
;; 1 byte:  'w'            [PORT2_RESP message]
;; 1 byte:  Result         [0 means OK, >0 means ERROR]
;;; Continued only if result = 0
;; 2 bytes: Port
;; 1 byte:  Node type      [77 means Erlang node, 72 means hidden node]
;; 1 byte:  Protocol       [0 means TCP/IP v4]
;; 2 bytes: Lowest version supported
;; 2 bytes: Highest version supported
;; 2 bytes: Node name length
;; N bytes: Node name
;; 2 bytes: Extra field length
;; M bytes: Extra field
;;

(define-tagged-binary-class port2-response ()
  ((tag    u1)
   (result u1))
  (:dispatch (if (= 0 result)
                 'port2-node-info-response
                 'port2-null-response)))

(define-binary-class port2-node-info-response (port2-response)
  ((port            u2)
   (node-type       u1)
   (protocol        u1)
   (lowest-version  u2)
   (highest-version u2)
   (name-length     u2)
   (name            (iso-8859-1-string :length name-length))
   (extra-length    u2)
   (extra           (iso-8859-1-string :length extra-length))))

(define-binary-class port2-null-response (port2-response)
  ())

(defun read-port2-response (stream)
  (read-value 'port2-response stream))

(defun write-port2-null-response (stream &optional (result 1))
  (write-value 'port2-null-response
               stream
               (make-instance 'port2-null-response
                              :tag (response-class-tag 'port2-response)
                              :result result)))

(defun write-port2-node-info-response (stream node-name node-port &key
                                       (node-type :erlang)
                                       (protocol :tcpip4)
                                       (lowest-version 5)
                                       (highest-version 5)
                                       (extra ""))
  (let ((node-name-length (length node-name))
        (extra-length (length extra))
        (node-type-integer (node-type-keyword-to-integer node-type))
        (protocol-integer (protocol-keyword-to-integer protocol)))
    (write-value 'port2-node-info-response
                 stream
                 (make-instance 'port2-node-info-response
                                :tag (response-class-tag 'port2-response)
                                :result 0
                                :port node-port
                                :node-type node-type-integer
                                :protocol protocol-integer
                                :lowest-version lowest-version
                                :highest-version highest-version
                                :name-length node-name-length
                                :name node-name
                                :extra-length extra-length
                                :extra extra))))


;;;
;;; NAMES_REQ
;;
;; 2 bytes: Total length of following message
;; 1 byte:  'n'            [NAMES_REQ message]
;;

(define-binary-class names-request (epmd-request)
  ())

(defun write-names-request (stream)
  (write-value 'names-request
               stream
               (make-instance 'names-request
                              :size 1
                              :tag (request-class-tag 'names-request))))


;;;
;;; NAMES_RESP
;;
;; 4 bytes: EPMDPortNo     Why do we get this?
;; N bytes: NodeInfo
;;

(define-binary-type iso-8859-1-string-until-eof ()
  (:reader (in)
    (coerce (loop for b = (read-byte in nil) while b collect (code-char b))
            'string))
  (:writer (out string)
    (loop for c across string do (write-byte (char-code c) out))
    (close out)))

(define-binary-class names-response ()
  ((epmd-port-number u4)
   (node-info        iso-8859-1-string-until-eof)))

(defun read-names-response (stream)
  (read-value 'names-response stream))

(defun write-names-response (stream port node-info)
  (write-value 'names-response
               stream
               (make-instance 'names-response
                              :epmd-port-number port
                              :node-info node-info)))

;;;
;;; DUMP_REQ
;;
;; 2 bytes: Total length of following message in bytes
;; 1 byte:  'd'            [DUMP_REQ message]
;;

(define-binary-class dump-request (epmd-request)
  ())

(defun write-dump-request (stream)
  (write-value 'dump-request
               stream
               (make-instance 'dump-request
                              :size 1
                              :tag (request-class-tag 'dump-request))))

;;;
;;; DUMP_RESP
;;
;; 4 bytes: EPMDPortNo
;; N bytes: NodeInfo
;;

(define-binary-class dump-response ()
  ((epmd-port-number u4)
   (node-info        iso-8859-1-string-until-eof)))

(defun read-dump-response (stream)
  (read-value 'dump-response stream))

(defun write-dump-response (stream port node-info)
  (write-value 'dump-response
               stream
               (make-instance 'dump-response
                              :epmd-port-number port
                              :node-info node-info)))

;;;
;;; KILL_REQ
;;
;; 2 bytes: Total length of following message in bytes
;; 1 byte:  'k'            [KILL_REQ message]
;;

(define-binary-class kill-request (epmd-request)
  ())

(defun write-kill-request (stream)
  (write-value 'kill-request
               stream
               (make-instance 'kill-request
                              :size 1
                              :tag (request-class-tag 'kill-request))))


;;;
;;; KILL_RESP
;;
;; 2 bytes: OKString
;;

(define-binary-class kill-response ()
  ((ok-string (iso-8859-1-string :length 2))))

(defun read-kill-response (stream)
  (read-value 'kill-response stream))

(defun write-kill-response (stream)
  (write-value 'kill-response
               stream
               (make-instance 'kill-response :ok-string "OK")))


;;;
;;; STOP_REQ
;;
;; 2 bytes: Total length of following message in bytes
;; 1 byte:  's'            [STOP_REQ message]
;; n bytes: NodeName
;;

(define-binary-class stop-request (epmd-request)
  ((node-name (iso-8859-1-string :length (1- size)))))

(defun write-stop-request (stream node-name)
  (write-value 'stop-request
               stream
               (make-instance 'stop-request
                              :size (length node-name)
                              :tag (request-class-tag 'stop-request)
                              :node-name node-name)))

;;;
;;; STOP_RESP / STOP_NOTOK_RESP
;;
;; 7 bytes: OKString / NOKString
;;

(define-tagged-binary-class stop-response ()
  ((ok-string (iso-8859-1-string :length 7)))
  (:dispatch (cond
               ((string= ok-string "STOPPED") 'stop-ok-response)
               ((string= ok-string "NOEXIST") 'stop-not-ok-response))))

(define-binary-class stop-ok-response (stop-response)
  ())

(define-binary-class stop-not-ok-response (stop-response)
  ())

(defun read-stop-response (stream)
  (read-value 'stop-response stream))

(defun write-stop-ok-response (stream)
  (write-value 'stop-ok-response
               stream
               (make-instance 'stop-ok-response :ok-string "STOPPED")))

(defun write-stop-not-ok-response (stream)
  (write-value 'stop-not-ok-response
               stream
               (make-instance 'stop-not-ok-response :ok-string "NOEXIST")))
