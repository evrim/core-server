(in-package :core-server.test)
;; sysv-standard method combination

(defclass a-server (server)
  ())

(defmethod start ((self a-server))
  (format t "Starting server a~%"))

(defclass b-server (server)
  ())

(defmethod start ((self b-server))
  (format t "Starting server b~%"))

(defclass c-server (a-server)
  ())

(defmethod start ((self c-server))
  (format t "Starting server c~%"))

(defclass test-server (c-server b-server)
  ())

(defmethod start ((self test-server))
  (format t "starting server test-server~%"))

(start (make-instance 'test-server))
;; TEST> (start (make-instance 'core-server::core-server))
;; c-server init-instance
;; core-server init-instance
;; around start core-server
;; starting ucw-server
;; starting b-server
;; starting a-server
;; starting c-server
;; starting core-server
;; NIL
;; TEST> (stop (make-instance 'core-server::core-server))
;; c-server init-instance
;; core-server init-instance
;; around stop core-server
;; stopping core-server
;; stopping c-server
;; stopping a-server
;; stopping b-server
;; stopping ucw-server
;; T
