(in-package :core-server.test)

(defparameter *core-server* (make-instance 'core-server))
(describe *core-server*)
(start *core-server*)
(defclass my-app2 (ucw-web-application apache-web-application)
  ()
  (:default-initargs 
   :fqdn "www.myweb.com"
    :admin-email "evrim@core.gen.tr"))

(defparameter *app* (make-instance 'my-app2))
(describe *app*)
(register *core-server* *app*)