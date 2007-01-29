(in-package :tr.gen.core.server)

(defun marshall-symbol (sym)
  `(intern ,(string sym) (find-package (make-keyword ,(package-name (symbol-package sym))))))

(defmethod load-application ((self ucw-server) fqdn app)  
  (awhen (getf (cdr app) :project-pathname)
    (pushnew it asdf:*central-registry*)
    (asdf:oos 'asdf:load-op (make-keyword (getf (cdr app) :project-name))))
  (let ((app (reduce #'(lambda (acc val)
			 (append acc
				 (if (and (listp val) (eq 'intern (car val)))
				     (list (eval val))
				     (list val))))
		     app :initial-value nil)))
    (let ((application-class (car app))
	  (initargs (cdr app)))
      (remf initargs :fqdn)
      (register-application self (apply #'make-instance application-class
					(cons :fqdn (cons fqdn initargs)))))))

(defmethod start ((self ucw-server))
  (start (ucw-server.ucw-db self))
  (startup-server self)
  (maphash (arnesi:curry #'load-application self)
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
  (when (ucw-web-application.persistent app)
    (execute (ucw-server.ucw-db self) 
	     (make-transaction 'tx-register-app 
			       (web-application.fqdn app)
			       (marshall-symbol (class-name (class-of app)))
			       (mapcar #'(lambda (arg)
					   (if (and (not (keywordp arg))
						    (symbolp arg)
						    (eq (find-package (make-keyword (getf (application.initargs app) :project-name)))
							(symbol-package arg)))
					       (marshall-symbol arg)
					       arg))
				       (application.initargs app)))))
  (register-application self app))

(defmethod unregister ((self ucw-server) (app ucw-web-application))
  (when (ucw-web-application.persistent app)
    (execute (ucw-server.ucw-db self)
	     (make-transaction 'tx-unregister-app
			       (web-application.fqdn app))))
  (unregister-application self app))

(defmethod ucw-server-fqdns ((self ucw-server))
  (arnesi:hash-table-keys (ucw-model.applications (model (ucw-server.ucw-db self)))))

(defmethod find-ucw-application ((self ucw-server) path)
  (find (if (and (starts-with path "/")
		 (ends-with path "/"))
	    path
	    (format nil "/~A/" path))
	(ucw::server.applications self) :key #'ucw::application.url-prefix :test #'equal))