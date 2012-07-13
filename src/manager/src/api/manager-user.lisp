(in-package :manager)

(defrest site-rest ()
  ()
  (:url "site")
  (:class site)
  (:authenticate t))

;; (core-server::defrest-crud/lift site-rest/anonymous site-rest)
;; (defrest site-rest-5 ()
;;   ()
;;   (:url "tite")
;;   (:class site)
;;   (:authenticate t))

;; (DEFMETHOD/LOCAL SITE-REST.LIST ((IT.BESE.ARNESI::SELF SITE-REST))
;;   (SITE.LIST TR.GEN.CORE.SERVER::CRUD-LIST
;; 	     (TR.GEN.CORE.SERVER::REST.APPLICATION
;; 	      IT.BESE.ARNESI::SELF)))

;; -------------------------------------------------------------------------
;; Manager Users API
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

