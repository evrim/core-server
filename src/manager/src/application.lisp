;; -------------------------------------------------------------------------
;; Manager Application
;; -------------------------------------------------------------------------
(in-package :manager)

(defapplication manager-application (root-web-application-mixin
				     http-application database-server
				     logger-server
				     serializable-web-application)
  ((token-cache :accessor manager.token-cache
		:initform (make-hash-table :test #'equal :synchronized t)))
  (:default-initargs
   :database-directory (merge-pathnames
			(make-pathname :directory '(:relative "var" "localhost" "db"))
			(tr.gen.core.server.bootstrap:home))
   :db-auto-start t :fqdn "localhost" :admin-email "root@localhost"
   :project-name "manager"
   :project-pathname #p"/home/aycan/core-server/projects/manager/"
   :htdocs-pathname (make-project-path "manager" "wwwroot")))

(defmethod web-application.password-of ((self manager-application) (user string))
  (aif (admin.find self :username user)
       (admin.password it)))

(defmethod web-application.find-user ((self manager-application) (user string))
  (admin.find self :username user))

(defmethod manager.oauth-uri ((self manager-application))
  (let ((server (application.server self)))
    (make-uri :scheme "http"
	      :server (web-application.fqdn self)
	      :port (core-server::socket-server.port server)
	      :paths '(("oauth.html")))))

(defmethod manager.api-uri ((self manager-application))
  (let ((server (application.server self)))
    (make-uri :scheme "http"
	      :server (web-application.fqdn self)
	      :port (core-server::socket-server.port server)
	      :paths '(("api")))))

;; -------------------------------------------------------------------------
;; Access Token Interface
;; -------------------------------------------------------------------------
(defmethod manager.remove-token ((self manager-application) (token access-token))
  (remhash (access-token.token token) (manager.token-cache self)))

(defmethod manager.gc-tokens ((self manager-application))
  (let ((cache (manager.token-cache self)))
    (prog1 cache
      (maphash (lambda (key access-token)
		 (if (access-token.expired-p access-token)
		     (remhash key cache)))
	       (manager.token-cache self)))))

(defmethod manager.create-token ((self manager-application) (realm realm)
				 (account account) (session-id string))

  ;; Garbage Collect First
  (when (> (random 100) 40) (manager.gc-tokens self))

  ;; Create Access Token
  (let ((association (or (account-association.find self :realm realm :account account)
			 (account-association.add self :realm realm :account account))))
    (let ((access-token (make-access-token :association association
					   :session-id session-id)))
      (with-slots (token) access-token
	(setf (gethash token (manager.token-cache self)) access-token)
	access-token))))

(deftransaction facebook-account.update-from-jobject ((self database)
						      (account facebook-account)
						      (data jobject))
  (with-attributes (updated_time verified locale timezone email
				 location gender link first_name
				 last_name name username) data
    (facebook-account.update self account
			     :name name :username username
			     :first-name first_name :last-name last_name
			     :email email :verified verified
			     :last-update updated_time :timezone timezone
			     :locale locale :location location :gender gender
			     :link link)))

(deftransaction facebook-account.add-from-jobject ((self database) (data jobject))
  (with-attributes (id) data
    (let ((account (facebook-account.add self :account-id id)))
      (facebook-account.update-from-jobject self account data))))

(deftransaction twitter-account.update-from-jobject ((self database)
						     (account twitter-account)
						     (data jobject))
  (with-attributes (lang verified geo_enabled time_zone
			 utc_offset created_at protected description
			 url location screen_name name) data
    (twitter-account.update self account
			    :lang lang
			    :verified verified
			    :geo-enabled geo_enabled
			    :time-zone time_zone
			    :utc-offset utc_offset
			    :created-at created_at
			    :protected protected
			    :description description
			    :url url
			    :location location
			    :screen-name screen_name
			    :name name
			    :last-update (get-universal-time))))

(deftransaction twitter-account.add-from-jobject ((self database) (data jobject))
  (with-attributes (id_str) data
    (let ((account (twitter-account.add self :account-id id_str)))
      (twitter-account.update-from-jobject self account data))))


(deftransaction google-account.update-from-jobject ((self database)
						    (account google-account)
						    (data jobject))
  (with-attributes (locale gender picture link family_name
			   given_name name verified_email email
			   last-update) data
    (google-account.update self account
			   :locale locale
			   :gender gender
			   :picture picture
			   :link link
			   :last-name family_name
			   :first-name given_name
			   :name name
			   :verified verified_email
			   :email email
			   :last-update last-update)))

(deftransaction google-account.add-from-jobject ((self database) (data jobject))
  (with-attributes (id) data
    (let ((account (google-account.add self :account-id id)))
      (google-account.update-from-jobject self account data))))

(deftransaction user.register ((self database) &key name email password
			       (timestamp (get-universal-time)))
  (let ((local-account (local-account.add self
					  :password password
					  :name name
					  :email email
					  :last-update timestamp)))
    (values (user.add self :accounts (list local-account)
			   :group (simple-group.find self :name "user"))
	    local-account)))

;; -------------------------------------------------------------------------
;; Sendmail
;; -------------------------------------------------------------------------
(defmethod manager.sendmail ((self manager-application) to subject body)
  (sendmail (application.server self)
	    (format nil "no-reply@~A" (web-application.fqdn self))
	    to (format nil "[Core-serveR] ~A" subject)
	    (<:html
	     (<:head
	      (<:meta :http--equiv "content-type"
		      :content "text/html; charset=utf-8")
	      (<:title "[Core serveR] - " subject))
	     (<:body
	      (<:h1 "[Core Server]")
	      (<:h2 subject)
	      (<:p "Dear Sir/Madam,")
	      body
	      (<:p :class "footer" "--" (<:br) "Kind regards," (<:br)
		   "Core Server Team "
		   (let ((email (format nil "info@~A" (web-application.fqdn self))))
		     (<:a :href (concatenate 'string "mailto:" email)
			  (concatenate 'string "&lt;" email "&gt;"))))))
	    nil
	    nil
	    "[Core-serveR]"))

(defmethod manager.send-password-recovery-email ((self manager-application)
						 email url)
  (manager.sendmail self email "Password Recovery"
    (list
     (<:p
      "You declared that you have just lost your password. "
      "To recover your password please click the link below.")
     (<:p
      (<:a :href (typecase url
		   (uri (uri->string url))
		   (string url))
	   "Recover Password")))))

;; -------------------------------------------------------------------------
;; Init Database
;; -------------------------------------------------------------------------
(defmethod init-database ((self manager-application))
  (assert (null (database.get self 'initialized)))
  (setf (database.get self 'api-secret) (random-string))
  (let ((group (simple-group.add self :name "admin")))
    (admin.add self :name "Root User" :username "root" :password "core-server"
	       :owner nil :group group))
  (simple-group.add self :name "user")
  (setf (database.get self 'initialized) t)
  self)

;; -------------------------------------------------------------------------
;; Server API Hook
;; -------------------------------------------------------------------------
(defmethod start ((self manager-application))
  (if (not (database.get self 'initialized))
      (prog1 t (init-database self))
      nil))

(defun hostname ()
  #+sbcl (sb-unix:unix-gethostname)
  #-sbcl "N/A")

(defun core-server-version ()
  (slot-value (asdf::find-system "core-server") 'asdf::version))


;; (deftransaction user.add-from-twitter ((self database) (data jobject))
;;   (with-attributes (id_str) data
;;     (aif (twitter-account.find self :account-id id_str)
;; 	 (error "User already exists ~A, Data: ~A" it data))

;;     (let ((account (twitter-account.add-from-jobject self data)))
;;       (values (user.add self
;; 			:accounts (list account)
;; 			:group (simple-group.find self :name "user"))
;; 	      account))))

;; (deftransaction user.add-from-google ((self database) (data jobject))
;;   (with-attributes (id) data
;;     (aif (google-account.find self :account-id id)
;; 	 (error "Google account already exists ~A, Data: ~A" it data))

;;     (let ((account (google-account.add-from-jobject self data)))
;;       (values (user.add self
;; 			:accounts (list account)
;; 			:group (simple-group.find self :name "user"))
;; 	      account))))

;; (deftransaction user.add-from-facebook ((self database) (data jobject))
;;   (with-attributes (id) data
;;     (aif (facebook-account.find self :account-id id)
;; 	 (error "User already exists ~A, Data: ~A" it data))

;;     (let ((account (facebook-account.add-from-jobject self data)))
;;       (values (user.add self
;; 			:accounts (list account)
;; 			:group (simple-group.find self :name "user"))
;; 	      account))))