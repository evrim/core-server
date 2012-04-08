(in-package :manager)

;; -------------------------------------------------------------------------
;; Sites API
;; -------------------------------------------------------------------------
(defmethod authenticate-sites-api ((self manager-application) username password)
  (and (site.find self :username username :password password) t))

(defvar +unauthorized-message+
  (<:html (<:body (<:h1 "Unauthorized")
		  (<:h2 "Core Server - "
			(<:a :href "http://labs.core.gen.tr/"
			     "http://labs.core.gen.tr/")))))

(defmacro with-sites-api ((manager username password) &body body)
  `(cond
     ((authenticate-sites-api ,manager ,username ,password) ,@body)
     (t
      (setf (http-response.status-code (context.response +context+))
	    (core-server::make-status-code 403))
      +unauthorized-message+)))

(defmacro defsites-api (name (&rest args) &body body)
  `(defhandler ,(format nil "^/api/~A\\.*" name)
       ((application manager-application) (username "api-key")
	(password "api-password") (output "output") ,@args)
     (flet ((output ()
	      (with-sites-api (application username password)
		,@body)))
       (cond
	 ((equal output "json")
	  (json-serialize (output)))
	 (t
	  (output))))))

;; -------------------------------------------------------------------------
;; Authentication - Server Side
;; -------------------------------------------------------------------------
(defclass+ authentication-token ()
  ((token)
   (user :type user)
   (manager-session-id :type string)))

(defmethod authenticate-token ((self manager-application) token)
  (let ((tokens (or (database.get self 'tokens)
		    (setf (database.get self 'tokens)
			  (make-hash-table :test #'equal :synchronized t)))))
    (gethash token tokens)))

(defmethod unauthenticate-token ((self manager-application) token)
  (let* ((tokens (or (database.get self 'tokens)
		     (setf (database.get self 'tokens)
			   (make-hash-table :test #'equal :synchronized t))))
	 (authentication-token (gethash token tokens)))
    (when authentication-token
      (remhash (authentication-token.manager-session-id authentication-token)
	       (http-application.sessions self))
      (remhash token tokens))))

(defsites-api "authenticate" ((token "token"))
  (aif (authenticate-token application token)
       (<core-server:response
	(<core-server:authentication :status "TRUE"
	  (<core-server:user :email (user.email it) :name (user.name it))))
       (<core-server:response
	(<core-server:authentication :status "FALSE"))))

(defsites-api "unauthenticate" ((token "token"))  
  (<core-server:response
   (<core-server:authentication
    :status (if (unauthenticate-token application token) "TRUE" "FALSE"))))

;; -------------------------------------------------------------------------
;; Manager API
;; -------------------------------------------------------------------------
(defmethod authenticate-manager-api ((self manager-application) username password)
  (and (user.find self :username username :password password) t))

(defmacro with-manager-api ((manager username password) &body body)
  `(cond
     ((authenticate-manager-api ,manager ,username ,password) ,@body)
     (t
      (setf (http-response.status-code (context.response +context+))
	    (core-server::make-status-code 403))
      +unauthorized-message+)))

(defmacro defmanager-api (name (&rest args) &body body)
  `(defhandler ,(format nil "^/api/~A\\.*" name)
       ((application manager-application) (username "username")
	(password "password") (output "output") ,@args)
     (flet ((output ()
	      (with-manager-api (application username password)
		,@body)))
       (cond
	 ((equal output "json")
	  (json-serialize (output)))
	 (t
	  (output))))))

(defmanager-api "add" ((site "site"))
  (let ((site (or (site.find application :fqdn site)
		  (site.add application :fqdn site))))
    (<core-server:response
     (<core-server:site :fqdn (site.fqdn site)
			:api-key (site.api-key site)
			:api-password (site.api-password site)))))

(defmanager-api "delete" ((site site))
  (let ((site (site.find application :fqdn site)))
    (<core-server:response
     (if site
	 (prog1 t (site.delete application site))
	 nil))))

(defmanager-api "query" ((site site))
  (let ((site (site.find application :fqdn site)))
    (<core-server:response
     (if site
	 (<core-server:site :fqdn (site.fqdn site)
			    :api-key (site.api-key site)
			    :api-password (site.api-password site))
	 nil))))

