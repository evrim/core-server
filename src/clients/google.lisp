;; -------------------------------------------------------------------------
;; Google OAuth 2.0 API
;; -------------------------------------------------------------------------
;; https://developers.google.com/accounts/docs/OAuth2Login
;; https://developers.google.com/accounts/docs/OAuth2WebServer
;; Author: Evrim Ulu <evrim@core.gen.tr>
;; Date: July 2012
(in-package :core-server)

;; -------------------------------------------------------------------------
;; OAuth Dialog
;; -------------------------------------------------------------------------
;; 1. Generate an OAuth Dialog URI
;; https://accounts.google.com/o/oauth2/auth
(defun make-google-oauth-uri (&key paths queries)
  (make-uri :scheme "https" :server "accounts.google.com"
	    :paths paths :queries queries))

(defvar +google-oauth-scope+
  (concat "https://www.googleapis.com/auth/userinfo.profile "
	  "https://www.googleapis.com/auth/userinfo.email"))

(defun <google:oauth-uri (&key (client-id (error "Provide :client-id"))
			       (redirect-uri (error "provide :redirect-uri"))
			       (scope +google-oauth-scope+)
			       (response-type "code")
			       (state nil))
  (<oauth2:oauth-uri (make-google-oauth-uri :paths '(("o") ("oauth2") ("auth")))
		     :client-id client-id :redirect-uri redirect-uri
		     :scope scope :response-type response-type :state state))

;; -------------------------------------------------------------------------
;; Get Access Token
;; -------------------------------------------------------------------------
;; 3. Get Access Token
;; (<google:get-access-token :client-id ".." :client-secret "..."
;;                           :code *code :redirect-uri *uri)
;; ((timestamp . 3187371)
;;  ("access_token" . "AAACT3RIWEo0BANaEBCQDRBb.. ..glX8ZC6iZBpNue3uWRBiubZBdYMtQZDZD")
;;  ("expires" . "4889"))
(defcommand <google:get-access-token (<oauth2:get-access-token)
  ()
  (:default-initargs :method 'post
   :url (make-google-oauth-uri :paths `(("o") ("oauth2") ("token")))))

(defmethod http.setup-uri ((self <google:get-access-token))
  (if (stringp (s-v 'url))
      (setf (s-v 'url) (uri? (make-core-stream (s-v 'url)))))

  #+ssl
  (if (and (http.ssl-p self) (null (uri.port (s-v 'url))))
      (setf (uri.port (s-v 'url)) 443))

  (with-slots (client-id client-secret code redirect-uri) self
    (mapcar (lambda (a)
	      (destructuring-bind (key . value) a
		(http.add-post self key value)))
	    `(("client_id" . ,client-id)
	      ("client_secret" . ,client-secret)
	      ("code" . ,code)
	      ("redirect_uri" . ,(typecase redirect-uri
				   (string redirect-uri)
				   (uri (uri->string redirect-uri))))
	      ("grant_type" . "authorization_code")))
    (s-v 'url)))

(defcommand <google:userinfo (<oauth2:authorized-funkall)
  ()
  (:default-initargs
   :url (make-uri :scheme "https" :server "www.googleapis.com"
		  :paths `(("oauth2") ("v1") ("userinfo")))))

;; +-------------------------------------------------------------------------
;; | Depreciated OpenID Code
;; +-------------------------------------------------------------------------
;; ;; Below code is the previous version of this api, namely OpenID (./openid.lisp).
;; ;; It is depreciated. Google and fellows now use OAuth 2.0 now. -evrim.

;; ;; -------------------------------------------------------------------------
;; ;; Google OpenId Associate
;; ;; -------------------------------------------------------------------------
;; (defcommand <google:associate (<openid:associate)
;;   ()
;;   (:default-initargs :url "https://www.google.com/accounts/o8/ud"))

;; ;; (+ "http://www.google.com/accounts/o8/ud?"
;; ;;    "&openid.ns=http://specs.openid.net/auth/2.0"
;; ;;    "&openid.claimed_id=http://specs.openid.net/auth/2.0/identifier_select"
;; ;;    "&openid.identity=http://specs.openid.net/auth/2.0/identifier_select"
;; ;;    "&openid.return_to=http://node1.coretal.net/auth_3rd_party.core?type=google"
;; ;;    "&openid.realm=http://node1.coretal.net/"
;; ;;    "&openid.mode=checkid_setup"
;; ;;    "&openid.assoc_handle=" (encode-u-r-i-component (google-assoc-handle self))
;; ;;    "&openid.ui.ns=http://specs.openid.net/extensions/ui/1.0"
;; ;;    "&openid.ui.mode=popup&openid.ui.icon=true"
;; ;;    "&openid.ns.ax=http://openid.net/srv/ax/1.0"
;; ;;    "&openid.ax.mode=fetch_request"
;; ;;    "&openid.ax.type.language=http://axschema.org/pref/language"
;; ;;    "&openid.ax.type.country=http://axschema.org/contact/country/home"
;; ;;    "&openid.ax.type.email=http://axschema.org/contact/email"
;; ;;    "&openid.ax.type.firstname=http://axschema.org/namePerson/first"
;; ;;    "&openid.ax.type.lastname=http://axschema.org/namePerson/last"
;; ;;    "&openid.ax.required=language,country,lastname,firstname,email")

;; ;; -------------------------------------------------------------------------
;; ;; Google OpenId Request Authentication
;; ;; -------------------------------------------------------------------------
;; (defcommand <google:request-authentication (<openid:request-authentication)
;;   ())

;; (defmethod run ((self <google:request-authentication))
;;   (http.add-post self "openid.ui.ns"
;; 		 "http://specs.openid.net/extensions/ui/1.0")
;;   (http.add-post self "openid.ui.mode" "popup")
;;   (http.add-post self "openid.ui.icon" "true")
;;   (http.add-post self "openid.ns.ax" "http://openid.net/srv/ax/1.0")
;;   (http.add-post self "openid.ax.mode" "fetch_request")
;;   (http.add-post self "openid.ax.type.language"
;; 		 "http://axschema.org/pref/language")
;;   (http.add-post self "openid.ax.type.country"
;; 		 "http://axschema.org/contact/country/home")
;;   (http.add-post self "openid.ax.type.email"
;; 		 "http://axschema.org/contact/email")
;;   (http.add-post self "openid.ax.type.firstname"
;; 		 "http://axschema.org/namePerson/first")
;;   (http.add-post self "openid.ax.type.lastname"
;; 		 "http://axschema.org/namePerson/last")
;;   (http.add-post self "openid.ax.required"
;; 		 "language,country,lastname,firstname,email")
;;   (call-next-method self))


;; ;; -------------------------------------------------------------------------
;; ;; Google OpenId Verify Authentication
;; ;; -------------------------------------------------------------------------
;; (defcommand <google:verify-authentication (<openid:verify-authentication)
;;   ())

;; ;; -------------------------------------------------------------------------
;; ;; Google exTract Authentication
;; ;; -------------------------------------------------------------------------
;; (defcommand <google:extract-authentication (<openid:funkall)
;;   ()
;;   (:default-initargs :url (error "Provide :url")
;;     :mode t))

;; (defmethod run ((self <google:extract-authentication))
;;   (with-slots (url) self
;;     (list :name (format nil "~A ~A"
;; 			(uri.query url "openid.ext1.value.firstname")
;; 			(uri.query url "openid.ext1.value.lastname"))
;; 	  :email (uri.query url "openid.ext1.value.email")
;; 	  :country (uri.query url "openid.ext1.value.country")
;; 	  :language (uri.query url "openid.ext1.value.language"))))