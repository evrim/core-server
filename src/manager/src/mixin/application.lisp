(in-package :manager)

(defmethod server.manager ((self server))
  (aif (any (lambda (app) (and (typep app 'manager-application) app))
	    (server.applications self))
       it
       (error "Couldn't find manager application in ~A" self)))

;; -------------------------------------------------------------------------
;; Manager Web Application Mixin
;; -------------------------------------------------------------------------
(defapplication manager-web-application-mixin ()
  ((oauth-handler-uri :host local :initform nil
		      :accessor web-application.oauth-handler-uri)
   ;; (oauth-api-uri :host remote :initform (error "Provide :api-uri")
   ;; 		  ;;		 "http://localhost:8080/api/oauth.core"
   ;; 		  :accessor web-application.api-uri)
   ))


(defmethod web-application.oauth-handler-uri ((self manager-web-application-mixin))
  (or (slot-value self 'oauth-handler-uri) "auth.core"))

(defmethod web-application.oauth-uri ((self manager-web-application-mixin))
  (manager.oauth-uri (server.manager (application.server self))))
