(in-package :tr.gen.core.server)

(defmethod start ((self ucw-server))
  (start (ucw-server.ucw-db self))
  (startup-server self)
  (maphash #'(lambda (fqdn app)
	       (when (find-class (car app) nil)
		 (register-application self (apply #'make-instance (car app) (cons :fqdn
										   (cons fqdn
											 (cdr app)))))))
	   (ucw-model.applications (model (ucw-server.ucw-db self)))))

(defmethod stop ((self ucw-server))
  (mapcar #'(lambda (app)
	      (unregister-application self app))
	  (ucw::server.applications self))
  (shutdown-server self)
  (stop (ucw-server.ucw-db self)))

(defmethod status ((self ucw-server))
  (and (status (ucw-server.ucw-db self)) 
       (ucw::server.started self)))

;; Tx
(defun tx-register-app (system fqdn class initargs)
  (let ((model (model system)))
    (symbol-macrolet ((entry (gethash fqdn (ucw-model.applications model))))
      (setf entry (cons class initargs)))))

(defun tx-unregister-app (system fqdn)
  (let ((model (model system)))
    (symbol-macrolet ((entry (gethash fqdn (ucw-model.applications model))))           
      (remhash fqdn (ucw-model.applications model)))))

(defmethod register ((self ucw-server) (app ucw-web-application))
  (execute (ucw-server.ucw-db self) 
	   (make-transaction 'tx-register-app 
			     (web-application.fqdn app)
			     (class-name (class-of app))
			     (application.initargs app)))
  (register-application self app))

(defmethod unregister ((self ucw-server) (app ucw-web-application))
  (execute (ucw-server.ucw-db self)
	   (make-transaction 'tx-unregister-app
			     (web-application.fqdn app)))
  (unregister-application self app))

(defmethod ucw-server-fqdns ((self ucw-server))
  (arnesi:hash-table-keys (ucw-model.applications (model (ucw-server.ucw-db self)))))