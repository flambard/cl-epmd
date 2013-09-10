(defpackage #:epmd-test
  (:documentation "Unit tests for the EPMD functions.")
  (:use #:cl
        #:epmd-protocol
        #:epmd-client
        #:epmd-server
        #:fiveam
        #:flexi-streams)
  (:shadow #:run-all-tests)
  (:export

   #:all-tests
   #:run-all-tests

   ))

(in-package :epmd-test)

(def-suite all-tests)

(def-suite epmd-protocol :in all-tests)
(def-suite epmd-client   :in all-tests)
(def-suite epmd-server   :in all-tests)

(defun run-all-tests ()
  (run! 'all-tests))
