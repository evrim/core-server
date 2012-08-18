;; +-------------------------------------------------------------------------
;; | Core Server API
;; +-------------------------------------------------------------------------
(in-package :manager)

;; -------------------------------------------------------------------------
;; API Call Definition Macro
;; -------------------------------------------------------------------------
(defmacro defapi-call (name (&rest args) &body body)
  (let ((url (format nil "^/api/~A\\.*" name)))
    `(progn
       (defauth ,url manager-application)
       (defhandler ,url ((application manager-application) (output "output")
			 ,@args)
	 (flet ((output () ,@body))
	   (cond
	     ((equal output "json") (json-serialize (output)))
	     (t (output))))))))


;; -------------------------------------------------------------------------
;; Login API CAll
;; -------------------------------------------------------------------------
(defapi-call "login" ((token "token"))
  (let* ((request (context.request +context+))
	 (user (core-server::http-request.authenticated-user request)) 
	 (access-token (gethash token (manager.token-cache application)))
	 (association (and access-token (access-token.association access-token))))

    ;; (describe
    ;;  (list 'login
    ;; 	   token access-token association user
    ;; 	   (and association (secure.owner (account-association.realm association)))
    ;; 	   (and user (admin.find application :username user))))
    
    (cond
      ((null access-token) ;; return error
       (<core-server:response
	(<core-server:error :code 401 "Invalid token.")))
      ((not (eq (secure.owner (account-association.realm association))
		(admin.find application :username user)))
       ;; No access
       (<core-server:response
	(<core-server:error :code 402 "You are not the realm owner.")))
      (t
       (with-slots (account) association
	 (<core-server:response
	  (<core-server:authentication :status "TRUE"
	   (<core-server:user
	    :id (get-database-id (account.user account))
	    :name (account.name account)
	    :last-update (account.last-update account)))))))))

;; -------------------------------------------------------------------------
;; Login Client
;; -------------------------------------------------------------------------
(defcommand <core-server:login (http)
  ((token :host local :initform (error "Provide :token"))))

(defmethod http.setup-uri ((self <core-server:login))
  (prog1 (call-next-method self)
    (http.add-query self "token" (slot-value self 'token))))

(defmethod http.evaluate ((self <core-server:login) result response)
  (let* ((response result)
	 (authentication (car (xml.children response)))
	 (user (and authentication (car (xml.children authentication)))))
    (if (and authentication (equal (authentication.status authentication)
				   "TRUE"))
	user
	(error "Authentication failed."))))

;; -------------------------------------------------------------------------
;; Logout API Call
;; -------------------------------------------------------------------------
(defapi-call "logout" ((token "token"))
  (let* ((request (context.request +context+))
	 (user (core-server::http-request.authenticated-user request)) 
	 (access-token (gethash token (manager.token-cache application)))
	 (association (and access-token (access-token.association access-token))))

    ;; (describe (list 'logout
    ;; 		    token access-token association user
    ;; 		    (secure.owner (account-association.realm association))
    ;; 		    (admin.find application :username user)))
    
    (cond
      ((null access-token) ;; return error
       (<core-server:response
	(<core-server:error :code 401 "Invalid token.")))
      ((not (eq (secure.owner (account-association.realm association))
		(admin.find application :username user)))
       ;; No access
       (<core-server:response
	(<core-server:error :code 402 "You are not the realm owner.")))
      (t
       (with-slots (account) association
	 (with-slots (session-id) access-token
	   (remhash session-id (http-application.sessions application))
	   (remhash token (manager.token-cache application))
	   (<core-server:response
	    (<core-server:authentication :status "FALSE"))))))))

;; -------------------------------------------------------------------------
;; Login Client
;; -------------------------------------------------------------------------
(defcommand <core-server:logout (http)
  ((token :host local :initform (error "Provide :token"))))

(defmethod http.setup-uri ((self <core-server:logout))
  (prog1 (call-next-method self)
    ;; (describe  (slot-value self 'token))
    (http.add-query self "token" (slot-value self 'token))))

(defmethod http.evaluate ((self <core-server:logout) result response)
  (let* ((response result)
	 (authentication (car (xml.children response))))
    (if (and authentication (equal (authentication.status authentication)
				   "FALSE"))
	t
	(error "Authentication failed."))))

;; -------------------------------------------------------------------------
;; Manager Application API a.k.a. Core Server API
;; -------------------------------------------------------------------------
(defrest realm-rest ()
  ()
  (:url "api\\/realm")
  (:class realm)
  (:authenticate t))

(defrest-client "http://localhost:8080/api/realm" realm <manager)


;; (defrest site-rest ()
;;   ()
;;   (:url "api\/site")
;;   (:class site)
;;   (:authenticate t))

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

;; ;; -------------------------------------------------------------------------
;; ;; Sites API
;; ;; -------------------------------------------------------------------------
;; (defmethod authenticate-sites-api ((self manager-application) username password)
;;   (and (site.find self :username username :password password) t))

;; (defvar +unauthorized-message+
;;   (<:html (<:body (<:h1 "Unauthorized")
;; 		  (<:h2 "Core Server - "
;; 			(<:a :href "http://labs.core.gen.tr/"
;; 			     "http://labs.core.gen.tr/")))))

;; (defmacro with-sites-api ((manager username password) &body body)
;;   `(cond
;;      ((authenticate-sites-api ,manager ,username ,password) ,@body)
;;      (t
;;       (setf (http-response.status-code (context.response +context+))
;; 	    (core-server::make-status-code 403))
;;       +unauthorized-message+)))

;; (defmacro defsites-api (name (&rest args) &body body)
;;   `(defhandler ,(format nil "^/api/~A\\.*" name)
;;        ((application manager-application) (username "api-key")
;; 	(password "api-password") (output "output") ,@args)
;;      (flet ((output ()
;; 	      (with-sites-api (application username password)
;; 		,@body)))
;;        (cond
;; 	 ((equal output "json")
;; 	  (json-serialize (output)))
;; 	 (t
;; 	  (output))))))

;; ;; -------------------------------------------------------------------------
;; ;; Authentication - Server Side
;; ;; -------------------------------------------------------------------------
;; (defclass+ authentication-token ()
;;   ((token)
;;    (user :type user)
;;    (manager-session-id :type string)))

;; (defmethod authenticate-token ((self manager-application) token)
;;   (let ((tokens (or (database.get self 'tokens)
;; 		    (setf (database.get self 'tokens)
;; 			  (make-hash-table :test #'equal :synchronized t)))))
;;     (gethash token tokens)))

;; (defmethod unauthenticate-token ((self manager-application) token)
;;   (let* ((tokens (or (database.get self 'tokens)
;; 		     (setf (database.get self 'tokens)
;; 			   (make-hash-table :test #'equal :synchronized t))))
;; 	 (authentication-token (gethash token tokens)))
;;     (when authentication-token
;;       (remhash (authentication-token.manager-session-id authentication-token)
;; 	       (http-application.sessions self))
;;       (remhash token tokens))))

;; (defsites-api "authenticate" ((token "token"))
;;   (aif (authenticate-token application token)
;;        (<core-server:response
;; 	(<core-server:authentication :status "TRUE"
;; 	  (<core-server:user :email (user.email it) :name (user.name it))))
;;        (<core-server:response
;; 	(<core-server:authentication :status "FALSE"))))

;; (defsites-api "unauthenticate" ((token "token"))  
;;   (<core-server:response
;;    (<core-server:authentication
;;     :status (if (unauthenticate-token application token) "TRUE" "FALSE"))))

