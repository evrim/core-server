(in-package :core-server.test)

(defparameter *ucw-server* (make-instance 'ucw-server :backend (ucw::make-backend :httpd :host "127.0.0.1" :port 8080)))
(describe *ucw-server*)
(start *ucw-server*)
(status *ucw-server*)
(stop *ucw-server*)

(defclass my-web-app (ucw-web-application apache-web-application)
  ())

(defparameter *app* (make-instance 'my-web-app
				   :fqdn "www.core.gen.tr"
				   :admin-email "abc"
				   :url-prefix "/www/"))
(defparameter *app2* (make-instance 'my-web-app
				    :fqdn "labs.core.gen.tr"
				    :admin-email "aycan@core.gen.tr"
				    :url-prefix "/labs/"))

(describe *app*)
(describe *app2*)
(register *ucw-server* *app*)
(register *ucw-server* *app2*)