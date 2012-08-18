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
   (oauth-key :host local :initform nil :accessor web-application.oauth-key)
   (oauth-secret :host local :initform nil :accessor web-application.oauth-secret)   
   ;; (oauth-api-uri :host remote :initform (error "Provide :api-uri")
   ;; 		  ;;		 "http://localhost:8080/api/oauth.core"
   ;; 		  :accessor web-application.api-uri)
   ))

(defmethod web-application.oauth-handler-uri ((self manager-web-application-mixin))
  (or (slot-value self 'oauth-handler-uri) "auth.core"))

(defmethod web-application.oauth-uri ((self manager-web-application-mixin))
  (manager.oauth-uri (server.manager (application.server self))))

(defmethod web-application.api-uri ((self manager-web-application-mixin) method-name)
  (let ((url (manager.api-uri (server.manager (application.server self)))))
    (prog1 url
      (setf (uri.paths url) (append (uri.paths url) (list (list method-name)))))))


;; Tell compiler to treat doLogin properly in mixin/plugin.lisp
(EVAL-WHEN (:LOAD-TOPLEVEL :COMPILE-TOPLEVEL :EXECUTE)
   (SETF (GETHASH 'DO-LOGIN TR.GEN.CORE.SERVER::+JAVASCRIPT-CPS-FUNCTIONS+) T
         (GETHASH '.DO-LOGIN TR.GEN.CORE.SERVER::+JAVASCRIPT-CPS-FUNCTIONS+) T)
   (DEFJSMACRO DO-LOGIN (&REST TR.GEN.CORE.SERVER::ARGS)
     `(.DO-LOGIN ,@TR.GEN.CORE.SERVER::ARGS)))

(defhandler "auth\.core" ((self manager-web-application-mixin) (token "token")
			  (hash "__hash"))
  (<:html
   (<:head
    (<:script :type "text/javascript" :src "library.core")
    (<:script :type "text/javascript"
     (lambda (stream)
       (rebinding-js/cc (hash token) stream
	 (let ((opener window.opener))
	   (cond
	     ((null opener)
	      (alert "An error ocurred. window.opener is null. -Coretal.net")
	      (window.close))
	     ((null token) (window.close))
	     ((and hash (slot-value opener hash))
	      (call/cc (slot-value opener hash) token)
	      (window.close))
	     (t
	      (let ((controller (slot-value opener 'controller)))
		(do-login controller token)
		(window.close)))))))))
   (<:body)))