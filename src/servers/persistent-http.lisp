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


(defmethod register ((self http-server) (application persistent-application))
  (warn "Registering a peristent application to a htt-server, changel server-class
to peristent-httpserver to fix."))

(defmethod register ((self persistent-http-server)
		     (application persistent-application))
  (setf (database.get self 'applications)
	(cons (cons (class-name (class-of self))
		    (slot-value self 'initialization-arguments))
	      (remove application (database.get self 'applications)
		      :key #'web-application.fqdn
		      :test #'equal))))