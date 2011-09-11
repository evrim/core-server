(in-package :core-server)

;; -------------------------------------------------------------------------
;; Google OpenId Associate
;; -------------------------------------------------------------------------
(defcommand <google:associate (<openid:associate)
  ()
  (:default-initargs :url "https://www.google.com/accounts/o8/ud"))

;; (+ "http://www.google.com/accounts/o8/ud?"
;;    "&openid.ns=http://specs.openid.net/auth/2.0"
;;    "&openid.claimed_id=http://specs.openid.net/auth/2.0/identifier_select"
;;    "&openid.identity=http://specs.openid.net/auth/2.0/identifier_select"
;;    "&openid.return_to=http://node1.coretal.net/auth_3rd_party.core?type=google"
;;    "&openid.realm=http://node1.coretal.net/"
;;    "&openid.mode=checkid_setup"
;;    "&openid.assoc_handle=" (encode-u-r-i-component (google-assoc-handle self))
;;    "&openid.ui.ns=http://specs.openid.net/extensions/ui/1.0"
;;    "&openid.ui.mode=popup&openid.ui.icon=true"
;;    "&openid.ns.ax=http://openid.net/srv/ax/1.0"
;;    "&openid.ax.mode=fetch_request"
;;    "&openid.ax.type.language=http://axschema.org/pref/language"
;;    "&openid.ax.type.country=http://axschema.org/contact/country/home"
;;    "&openid.ax.type.email=http://axschema.org/contact/email"
;;    "&openid.ax.type.firstname=http://axschema.org/namePerson/first"
;;    "&openid.ax.type.lastname=http://axschema.org/namePerson/last"
;;    "&openid.ax.required=language,country,lastname,firstname,email")

;; -------------------------------------------------------------------------
;; Google OpenId Request Authentication
;; -------------------------------------------------------------------------
(defcommand <google:request-authentication (<openid:request-authentication)
  ())

(defmethod run ((self <google:request-authentication))
  (http.add-post self "openid.ui.ns"
		 "http://specs.openid.net/extensions/ui/1.0")
  (http.add-post self "openid.ui.mode" "popup")
  (http.add-post self "openid.ui.icon" "true")
  (http.add-post self "openid.ns.ax" "http://openid.net/srv/ax/1.0")
  (http.add-post self "openid.ax.mode" "fetch_request")
  (http.add-post self "openid.ax.type.language"
		 "http://axschema.org/pref/language")
  (http.add-post self "openid.ax.type.country"
		 "http://axschema.org/contact/country/home")
  (http.add-post self "openid.ax.type.email"
		 "http://axschema.org/contact/email")
  (http.add-post self "openid.ax.type.firstname"
		 "http://axschema.org/namePerson/first")
  (http.add-post self "openid.ax.type.lastname"
		 "http://axschema.org/namePerson/last")
  (http.add-post self "openid.ax.required"
		 "language,country,lastname,firstname,email")
  (call-next-method self))


;; -------------------------------------------------------------------------
;; Google OpenId Verify Authentication
;; -------------------------------------------------------------------------
(defcommand <google:verify-authentication (<openid:verify-authentication)
  ())

;; -------------------------------------------------------------------------
;; Google exTract Authentication
;; -------------------------------------------------------------------------
(defcommand <google:extract-authentication (<openid:funkall)
  ()
  (:default-initargs :url (error "Provide :url")
    :mode t))

(defmethod run ((self <google:extract-authentication))
  (with-slots (url) self
    (list :name (format nil "~A ~A"
			(uri.query url "openid.ext1.value.firstname")
			(uri.query url "openid.ext1.value.lastname"))
	  :email (uri.query url "openid.ext1.value.email")
	  :country (uri.query url "openid.ext1.value.country")
	  :language (uri.query url "openid.ext1.value.language"))))