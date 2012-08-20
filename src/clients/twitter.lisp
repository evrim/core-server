;; -------------------------------------------------------------------------
;; Twitter API Implementation
;; -------------------------------------------------------------------------
;; Date: Aug 2012
;; Author: Evrim Ulu <evrim@core.gen.tr>
;; https://dev.twitter.com/docs/auth/implementing-sign-twitter
(in-package :core-server)

;; -------------------------------------------------------------------------
;; Get Request Token
;; -------------------------------------------------------------------------
(defcommand <twitter:get-request-token (<oauth1:get-request-token)
  ()
  (:default-initargs :url "https://api.twitter.com/oauth/request_token"))

;; -------------------------------------------------------------------------
;; Authorize URL
;; -------------------------------------------------------------------------
;; https://api.twitter.com/oauth/authenticate?oauth_token=NPcudxy0yU5T3...
(defun <twitter:authorize-url (&key (token (error "Provide request :token")))
  (<oauth1:authorize-url (make-uri :scheme "https"
				   :server "api.twitter.com"
				   :paths '(("oauth") ("authorize")))
			 :token token))

;; -------------------------------------------------------------------------
;; Twitter Access Token
;; -------------------------------------------------------------------------
(defclass+ <twitter:access-token (<oauth1:access-token)
  ((user-id :host local :accessor <twitter:access-token.user-id)
   (screen-name :host local :accessor <twitter:access-token.screen-name))
  (:ctor <twitter:%make-access-token))

;; -------------------------------------------------------------------------
;; Get Access Token
;; -------------------------------------------------------------------------
;; An example answer to authorize-url:
;; https://node1.coretal.net/auth.html?oauth_token=6u..&oauth_verifier=d8..
;; (with-slots (consumer-key consumer-secret) (database.get *app* :twitter)
;;   (octets-to-string 
;;    (<twitter:get-access-token :verifier "E5RBHljp1UgFgLTxAyjPQLfOSNtPZokg13PhJlF2wdI"
;; 			      :consumer-key consumer-key
;; 			      :consumer-secret consumer-secret
;; 			      :request-token *x :parse-p nil
;; 			      :debug-p t)
;;    :utf-8))
;; "oauth_token=70423595-EJfrzSNjfoAkoLrJAKjhT6g8xFbpp4LzBGCoFE8eU&oauth_token_secret=yPUUQGuBATaYriBUSoO3LfH0nHRGtxdVyUFk8mOQnyc&user_id=70423595&screen_name=evrimulu"
(defcommand <twitter:get-access-token (<oauth1:get-access-token)
  ()
  (:default-initargs :url "https://api.twitter.com/oauth/access_token"))

(defmethod http.evaluate ((self <twitter:get-access-token) result response)
  (flet ((get-key (name) (cdr (assoc name result :test #'equal))))
    (if (eq (http-response.status-code response) 200)
	(values (<twitter:%make-access-token
		 :token (get-key "oauth_token")
		 :token-secret (get-key "oauth_token_secret")
		 :user-id (get-key "user_id")
		 :screen-name (get-key "screen_name"))
		response)
	(values result response))))

;; -------------------------------------------------------------------------
;; Secure Get User
;; -------------------------------------------------------------------------
(defcommand <twitter:secure-get-user (<oauth1:secure-funkall)
  ((screen-name :host local)
   (user-id :host local)
   (include-entities :host local :initform t))
  (:default-initargs :url "https://api.twitter.com/1/users/show.json"
		     :method 'get))

(defmethod <oauth1:funkall.parameters ((self <twitter:secure-get-user))
  (with-slots (screen-name user-id include-entities) self
    (cons (if screen-name
	      `("screen_name" . ,screen-name)
	      `("user_id" . ,user-id))
	  (if include-entities
	      (cons `("include_entities" . "true") (call-next-method self))
	      (call-next-method self)))))

(defmethod http.setup-uri ((self <twitter:secure-get-user))
  (with-slots (screen-name user-id include-entities) self
    (when (and (null screen-name) (null user-id))
      (error "One of :screen-name or :user-id must be provided.")))
  
  (call-next-method self))

;; -------------------------------------------------------------------------
;; Twitter Funkall (depreciated)
;; -------------------------------------------------------------------------
;; https://dev.twitter.com/docs/api
(defcommand <twitter:funkall (http)
  ((cmd :host local :initform (error "Provide :cmd")))
  (:default-initargs :url "http://api.twitter.com/1/"))

(defcommand <twitter:get-user-lists (<twitter:funkall)
  ((username :host local :initform (error "Provide :username")))
  (:default-initargs
      :cmd t
      :url "http://twitter.com/goodies/list_of_lists"))

(defmethod run ((self <twitter:get-user-lists))
  (http.add-query self "screen_name" (s-v 'username))
  (let ((result (call-next-method self)))
    (awhen result
      (getf (jobject.attributes (json-deserialize (octets-to-string result :utf-8))) :lists))))


;; STAGE 1.
;; MANAGER> (with-slots (consumer-key consumer-secret) (database.get *app* :twitter)
;; 	   (<twitter:get-request-token :callback "http://node1.coretal.net/auth.html"
;; 				       :consumer-key consumer-key
;; 				       :consumer-secret consumer-secret
;; 				       :debug-p nil))
;; #<<OAUTH1:REQUEST-TOKEN  {100A32A843}>
;; #<HTTP-RESPONSE (200 . OK) {100A31F063}>
;; MANAGER> (setf *x *)
;; #<<OAUTH1:REQUEST-TOKEN  {100A32A843}>
;; MANAGER> (describe *x)
;; #<<OAUTH1:REQUEST-TOKEN  {100A32A843}>
;;   [standard-object]

;; Slots with :INSTANCE allocation:
;;   TOKEN               = "sbkzpK6VGbkC2wpAUW4AkWvwTwXEGyNlbNzVwfjO1Qc"
;;   TOKEN-SECRET        = "82SvLHDgGPdFrn19asizzmRoW0c7Mx14nRdQXWNBs"
;;   CALLBACK-CONFIRMED  = TRUE
;; ; No value

;; STAGE 2.
;; MANAGER> (uri->string (<twitter:authorize-url :token *x))
;; "https://api.twitter.com/oauth/authorize?oauth_token=sbkzpK6VGbkC2wpAUW4AkWvwTwXEGyNlbNzVwfjO1Qc"
;; MANAGER> (setf *verifier "zlpTUE70IDvWXcKeHTPX8F6g44JsDEd35XgZT3aNE6g")
;; "zlpTUE70IDvWXcKeHTPX8F6g44JsDEd35XgZT3aNE6g"

;; STAGE 3.
;; Twitter redirects to:
;; http://node1.coretal.net/auth.html?oauth_token=M5JgfX6wlMbIqx9KoV0MXCpHNLsleziMmvTEtQqA&oauth
;; _verifier=qCfX9zfDWrWwaG8Zp2u1zlg76vcby6ssdGLivmcrQjY

;; Let's evaluate:
;; MANAGER> (with-slots (consumer-key consumer-secret) (database.get *app* :twitter)
;; 	   (<twitter:get-access-token :verifier *verifier
;; 				      :consumer-key consumer-key
;; 				      :consumer-secret consumer-secret
;; 				      :request-token *x
;; 				      :debug-p nil))
;; #<<TWITTER:ACCESS-TOKEN  {1004811673}>
;; #<HTTP-RESPONSE (200 . OK) {10047341D3}>
;; MANAGER> (describe *)
;; #<<TWITTER:ACCESS-TOKEN  {1004811673}>
;;   [standard-object]

;; Slots with :INSTANCE allocation:
;;   TOKEN         = "70423595-EJfrzSNjfoAkoLrJAKjhT6g8xFbpp4LzBGCoFE8eU"
;;   TOKEN-SECRET  = "yPUUQGuBATaYriBUSoO3LfH0nHRGtxdVyUFk8mOQnyc"
;;   USER-ID       = "70423595"
;;   SCREEN-NAME   = "evrimulu"
;; ; No value
