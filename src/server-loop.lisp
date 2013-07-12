(in-package :epmd-server)

(define-condition shutdown-server ()
  ()
  (:documentation "Signal to stop the server loop."))


;;;
;;; Server loop
;;;

(defun start (host)
  (server-loop (initialize-server host)))

(defun initialize-server (host)
  (let* ((listen-socket (usocket:socket-listen host
                                               +port+
                                               :element-type '(unsigned-byte 8)
                                               :reuse-address t)))
    (make-instance 'epmd-server
                   :listen-socket listen-socket
                   :listen-port +port+)))

(defun server-loop (server)
  (handler-case
      (unwind-protect
           (loop
              (format t "Listening...~%")
              (finish-output)
              (let* ((socket (usocket:socket-accept (listen-socket server)))
                     (stream (usocket:socket-stream socket))
                     (request (read-request stream))
                     (response (response server request)))
                (format t "Got request: ~a~%" request)
                (finish-output)
                (write-message stream response)
                (post-response-action server response socket)))
        (usocket:socket-close (listen-socket server)))
    (shutdown-server ()
      (return-from server-loop))))



;;;
;;; CLOSE-CONNECTION implementation
;;;

(defmethod close-connection ((connection usocket:usocket))
  (usocket:socket-close connection))


;;;
;;; CONNECTION-STREAM implementation
;;;

(defmethod connection-stream ((connection usocket:usocket))
  (usocket:socket-stream connection))


;;;
;;; KILL-SERVER implementation
;;;

(defmethod kill-server ((server epmd-server))
  (declare (ignore server))
  (signal 'shutdown-server))
