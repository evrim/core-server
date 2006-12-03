(in-package :core-server.test)

(defparameter *ucw-server* (make-instance 'ucw-server))
(describe *ucw-server*)
(start *ucw-server*)
(status *ucw-server*)
(stop *ucw-server*)

(defclass my-web-app (ucw-web-application apache-web-application)
  ())

(defparameter *app* (make-instance 'my-web-app
				   :fqdn "www.core.gen.tr"
				   :admin-email "abc"))

(describe *app*)
(register *ucw-server* *app*)
