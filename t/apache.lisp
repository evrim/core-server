(in-package :core-server.test)

(defparameter *apache-server* (make-instance 'apache-server))
(describe *apache-server*)
(if (status *apache-server*)
    (progn
      (stop *apache-server*)
      (start *apache-server*)
      (unless (status *apache-server*)
	(error "Error occured while starting apache server.")))
    (progn
      (start *apache-server*)
      (unless (status *apache-server*)
	(error "Error occured while starting apache server."))))

(defparameter *apache-application* (make-instance 'apache-web-application
						  :fqdn (format nil "www.test-~A.com" (string-downcase (gensym)))
						  :admin-email "evrim@core.gen.tr"))
(describe *apache-application*)
(register *apache-server* *apache-application*)
(unregister *apache-server* *apache-application*)
