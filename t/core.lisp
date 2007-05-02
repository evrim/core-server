(in-package :core-server.test)

(defparameter *core-server* (make-instance 'core-server))
(describe *core-server*)
(start *core-server*)
(setf ucw::*default-server* core-server.test::*core-server*)
(defclass my-app2 (ucw-web-application apache-web-application)
  ()
  (:default-initargs 
   :fqdn "www.myweb.com"
    :admin-email "evrim@core.gen.tr"))

(defparameter *app* (make-instance 'my-app2))
(describe *app*)
(register *core-server* *app*)


(defparameter *core-web-server* (make-instance 'core-web-server))
(describe *core-web-server*)
(start *core-web-server*)
(status *core-web-server*)
(setf ucw::*default-server* *core-web-server*)
(setf *core-server* *core-web-server*)