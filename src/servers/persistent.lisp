(in-package :core-server)

;; -------------------------------------------------------------------------
;; Persistent Server
;; -------------------------------------------------------------------------
(defclass+ persistent-server (database-server)
  ())

;; --------------------------------------------------------------------------
;; Overriden get-directory method: This allows us to use default
;; database directory for database driven http server.
;; --------------------------------------------------------------------------
(defmethod database.directory ((server persistent-server))
  (ensure-directories-exist
   (merge-pathnames
    (make-pathname :directory (list :relative "var"
				    (format nil "core-server-~A:~A"
					    (socket-server.host server)
					    (socket-server.port server))
				    "db"))
    (bootstrap::home))))

(deftransaction persistent-server.register ((self persistent-server)
					    (application web-application))
  (setf (database.get self :applications)
	(cons application (persistent-server.unregister self application))))

(deftransaction persistent-server.unregister ((self persistent-server)
					      (application web-application)) 
  (setf (database.get self :applications)
	(remove (web-application.fqdn application)
		(database.get self :applications))))

(defmethod persistent-server.persistent-p ((self persistent-server)
					   (app persistent-application))
  (member app (database.get self :applications)))

(defmethod register ((self persistent-server) (app persistent-application))
  (when (not (persistent-server.persistent-p self app))
    (persistent-server.register self app)))

(defmethod unregister ((self persistent-server) (app persistent-application))
  (when (persistent-server.persistent-p self app)
    (stop app)
    (persistent-server.unregister self app)))

(defmethod start ((self persistent-server))
  (mapcar (lambda (application) (start application) (register self application))
	  (database.get self :applications)))

(defmethod stop ((self persistent-server))
  ;; (mapcar (lambda (application) (unregister self application))
  ;; 	  (database.get self :applications))
  )

;; (defclass+ sample-persistent-application (http-application)
;;   ()
;;   (:metaclass persistent-http-application+))

;; (defparameter *persistent-application*
;;   (make-instance 'sample-persistent-application
;; 		 :fqdn "persistent-application"
;; 		 :admin-email "evrim@core.gen.tr"
;; 		 :persistent t))
