;; -------------------------------------------------------------------------
;; Yahoo API Implementation
;; -------------------------------------------------------------------------
;; Date: Aug 2012
;; Author: Evrim Ulu <evrim@core.gen.tr>
;; http://developer.yahoo.com/oauth/guide/oauth-requesttoken.html
(in-package :core-server)

;; -------------------------------------------------------------------------
;; Yahoo Request Token
;; -------------------------------------------------------------------------
(defclass+ <yahoo:request-token (<oauth1:request-token)
  ((expires-in :host local)
   (request-auth-url :host local))
  (:ctor <yahoo:%make-request-token))

;; -------------------------------------------------------------------------
;; Get Request Token
;; -------------------------------------------------------------------------
(defcommand <yahoo:get-request-token (<oauth1:get-request-token)
  ((lang-pref :host local :initform "en-us"))
  (:default-initargs :url "https://api.login.yahoo.com/oauth/v2/get_request_token"))

(defmethod <oauth1:funkall.parameters ((self <yahoo:get-request-token))
  `(,@(call-next-method self)
    ("xoauth_lang_pref" . ,(slot-value self 'lang-pref))))

(defmethod http.evaluate ((self <yahoo:get-request-token) result response)
  (flet ((get-key (name) (cdr (assoc name result :test #'equal))))
    (if (eq (http-response.status-code response) 200)
	(values (<yahoo:%make-request-token
		 :token (get-key "oauth_token")
		 :token-secret (get-key "oauth_token_secret")
		 :callback-confirmed (json-deserialize
				      (get-key "oauth_callback_confirmed"))
		 :expires-in (json-deserialize (get-key "oauth_expires_in"))
		 :request-auth-url (get-key "xoauth_request_auth_url"))
		response)
	(values result response))))

;; -------------------------------------------------------------------------
;; Authorize URL
;; -------------------------------------------------------------------------
;; https://api.login.yahoo.com/oauth/v2/request_auth?oauth_token=hwyun4h
(defun <yahoo:authorize-url (&key (token (error "Provide request :token")))
  (<oauth1:authorize-url (make-uri :scheme "https"
				   :server "api.login.yahoo.com"
				   :paths '(("oauth") ("v2") ("request_auth")))
			 :token token))

;; -------------------------------------------------------------------------
;; Yahoo Access Token
;; -------------------------------------------------------------------------
(defclass+ <yahoo:access-token (<oauth1:access-token)
  ((session-handle :host local)
   (expires-in :host local)
   (authorization-expires-in :host local)
   (yahoo-guid :host local))
  (:ctor <yahoo:%make-access-token))

;; -------------------------------------------------------------------------
;; Get Access Token
;; -------------------------------------------------------------------------
(defcommand <yahoo:get-access-token (<oauth1:get-access-token)
  ()
  (:default-initargs :url "https://api.login.yahoo.com/oauth/v2/get_token"
		     :method 'get))

(defmethod <oauth1:funkall.parameters ((self <yahoo:get-access-token))
  (with-slots (request-token verifier) self
    (with-slots (token-secret) request-token
      `(,@(call-next-method self) ("oauth_verifier" . ,verifier)))))

(defmethod http.evaluate ((self <yahoo:get-access-token) result response)
  (flet ((get-key (name) (cdr (assoc name result :test #'equal))))
    (if (eq (http-response.status-code response) 200)
	(values (<yahoo:%make-access-token
		 :token (get-key "oauth_token")
		 :token-secret (get-key "oauth_token_secret")
		 :expires-in (get-key "oauth_expires_in")
		 :session-handle (get-key "oauth_session_handle")
		 :authorization-expires-in (get-key "oauth_authorization_expires_in")
		 :yahoo-guid (get-key "xoauth_yahoo_guid"))
		response)
	(values result response))))

;; -------------------------------------------------------------------------
;; Secure Get User
;; -------------------------------------------------------------------------
(defcommand <yahoo:secure-get-user (<oauth1:secure-funkall)
  ((guid :host local))
  (:default-initargs :url "http://social.yahooapis.com/v1/user/"
		     :method 'get))

(defmethod http.setup-uri ((self <yahoo:secure-get-user))
  (with-slots (url) self
    (let ((url (if (stringp url)
		   (uri? (make-core-stream url))
		   url)))
      (with-slots (guid) self
	(setf (uri.paths url) (append (uri.paths url) `((,guid) ("profile")))
	      (slot-value self 'url) url)
	(http.add-query self "format" "json")
	(http.add-query self "count" "max")
	(call-next-method self)))))

;; MANAGER> (json! *core-output* *f)
;; { 
;; "profile": { 
;; "isConnected": false, 
;; "profileUrl": 'http://profile.yahoo.com/2GH5FHPKWXNH4V4M2CYGPYESZY', 
;; "nickname": 'Evrim', 
;; "memberSince": '2011-07-06T20:56:34Z', 
;; "image": { 
;; "width": 192, 
;; "size": '192x192', 
;; "imageUrl": 'http://l.yimg.com/a/i/identity2/profile_192b.png', 
;; "height": 192 }
;; , 
;; "created": '2012-08-19T17:54:16Z', 
;; "bdRestricted": true, 
;; "guid": '2GH5FHPKWXNH4V4M2CYGPYESZY', 
;; "uri": 'http://social.yahooapis.com/v1/user/2GH5FHPKWXNH4V4M2CYGPYESZY/profile' }
;;  }