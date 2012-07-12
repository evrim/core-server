(in-package :core-server)

;; -------------------------------------------------------------------------
;; Persistent HTTP Server
;; -------------------------------------------------------------------------
(defclass+ persistent-http-server (http-server database-server)
  ())

;; --------------------------------------------------------------------------
;; Overriden get-directory method: This allows us to use default
;; database directory for database driven http server.
;; --------------------------------------------------------------------------
(defmethod database.directory ((server persistent-http-server))
  (ensure-directories-exist
   (merge-pathnames
    (make-pathname :directory (list :relative "var"
				    (format nil "core-server-~A:~A"
					    (socket-server.host server)
					    (socket-server.port server))
				    "db"))
    (bootstrap::home))))

(deftransaction register-to-database ((self persistent-http-server)
				      (application web-application))
  (setf (database.get self :applications)
	(cons application (unregister-from-database self application))))

(deftransaction unregister-from-database ((self persistent-http-server)
					  (application web-application)) 
  (setf (database.get self :applications)
	(remove (web-application.fqdn application) 
		(database.get self :applications)
		:test #'equal :key #'web-application.fqdn)))

(defmethod register ((self persistent-http-server)
		     (application web-application))
  (describe application)
  (when (web-application.persistent application)
    (register-to-database self application)))

(defmethod unregister ((self persistent-http-server)
		       (application web-application))
  (when (web-application.persistent application)
    (unregister-from-database self application)))

(defmethod start ((self persistent-http-server))
  (mapcar (lambda (application) (register self application))
  	  (filter (lambda (app)
		    (not (find-application self (web-application.fqdn app))))
		  (database.get self :applications))))

(defmethod stop ((self persistent-http-server))
  ;; (mapcar (lambda (application) (unregister self application))
  ;; 	  (filter (lambda (app)
  ;; 		    (find-application self (web-application.fqdn app)))
  ;; 		  (database.get self :applications)))
  )

;; (defclass+ sample-persistent-application (http-application)
;;   ()
;;   (:metaclass persistent-http-application+))

;; (defparameter *persistent-application*
;;   (make-instance 'sample-persistent-application
;; 		 :fqdn "persistent-application"
;; 		 :admin-email "evrim@core.gen.tr"
;; 		 :persistent t))