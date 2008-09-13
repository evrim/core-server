(in-package :core-server.test)
;; sysv-standard method combination

(defclass a-server (server)
  ((a-server.status :initform nil)))

(defmethod start ((self a-server))
  (format t "Starting server a~%")
  (setf (slot-value self 'a-server.status) t)
  'a-server-start)

(defmethod stop ((self a-server))
  (format t "Stopping server a~%")
  (setf (slot-value self 'a-server.status) nil)
  'a-server-stop)

(defmethod status ((self a-server))
  (slot-value self 'a-server.status))

(defclass b-server (server)
  ((b-server.status :initform nil)))

(defmethod start ((self b-server))
  (format t "Starting server b~%")
  'b-server-start)

(defmethod stop ((self b-server))
  (format t "Stopping server b~%")
  (setf (slot-value self 'b-server.status) nil)
  'b-server-stop)

(defclass c-server (a-server)
  ())

(defmethod start ((self c-server))
  (format t "Starting server c~%")
  'c-server-start)

(defclass test-server (c-server b-server a-server)
  ())

(defmethod start ((self test-server))
  (format t "starting server test-server~%")
  'test-server-start)

(deftest sysv-1
    (let ((s (make-instance 'test-server)))
      (multiple-value-list (start s)))
  (test-server-start c-server-start b-server-start a-server-start t))

(deftest sysv-2
    (let ((s (make-instance 'test-server)))
      (start s)
      (multiple-value-list (start s)))
  (test-server-start c-server-start b-server-start t t))

(deftest sysv-3
    (let ((s (make-instance 'test-server)))
      (start s)
      (multiple-value-list (stop s)))
  (b-server-stop a-server-stop t))

(deftest sysv-4
    (let ((s (make-instance 'test-server)))
      (start s)
      (stop s)
      (multiple-value-list (stop s)))
  (b-server-stop t t))

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
