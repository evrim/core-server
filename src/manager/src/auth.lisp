(in-package :manager)

;; -------------------------------------------------------------------------
;; Authentication Controller
;; -------------------------------------------------------------------------
(defmethod/cc make-auth-controller/anonymous ((self manager-application)
					      &key (page "login") error-message)
  (<core:simple-controller :default-page page 
   (<core:simple-page :name "login"
    (<core:simple-widget-map :selector "top-right"
			     :widget (<widget:simple-content
				      (<:p "Would you like to have a new account?"
				       (<:a :class "pad5"
					    :href "#page:register" "Sign Up"))))
    (<core:simple-widget-map :selector "middle" :widget (<manager:login))
    (<core:simple-widget-map :selector "bottom"
			     :widget (<widget:simple-content
				      "Our services in other languages: "
				      (<:a :href "#" "English") " | "
				      (<:a :href "#" "Turkce"))))
   (<core:simple-page :name "register"
    (<core:simple-widget-map :selector "top-right"
			     :widget (<widget:simple-content
				      (<:p "Already have an account?"
				       (<:a :class "pad5"
					    :href "#page:login"
					    "Sign In"))))
    (<core:simple-widget-map :selector "middle" :widget (<manager:registration))
    (<core:simple-widget-map :selector "bottom"
			     :widget (<widget:simple-content
				      "Our services in other languages: "
				      (<:a :href "#" "English") " | "
				      (<:a :href "#" "Turkce"))))
   (<core:simple-page :name "error"
    (<core:simple-widget-map :selector "top-right"
			     :widget (<widget:simple-content
				      (<:p "Would you like to have a new account?"
				       (<:a :class "pad5"
					    :href "#page:register" "Sign Up"))))
    (<core:simple-widget-map :selector "middle"
			     :widget (<manager:authentication-error
				      :message error-message)))))

(defmethod/cc make-auth-controller/user ((self manager-application)
					 (account account) (realm realm))
  (<core:simple-controller :default-page "accounts"
   (<core:simple-page :name "accounts"
    (<core:simple-widget-map :selector "top-right"
			     :widget (<widget:simple-content
				      (<:p "You are about to login to "
				       (realm.fqdn realm) ".")))
    (<core:simple-widget-map :selector "middle"
   			     :widget (<manager:accounts :default-account account))
    (<core:simple-widget-map :selector "bottom"
   			     :widget (<widget:simple-content
   				      "Our services in other languages: "
   				      (<:a :href "#" "English") " | "
   				      (<:a :href "#" "Turkce"))))))


(defmethod make-oauth-answer-uri ((self manager-application) (provider symbol))
  (let ((url (manager.oauth-uri self)))
    (setf (uri.queries url) `(("action" . "answer")
			      ("provider" . ,(symbol->js provider))))
    url))

;; http://node1.coretal.net:8080/auth.core?mode=return&type=facebook&code=CODE#_=_
;; http://localhost:8080/auth.core?action=login&return-to=http%3A%2F%2Fnode1.coretal.net%3A8080%2Fauthentication%2Fauth.core
(defmethod make-oauth-uri ((self manager-application) (provider symbol) (state string))
  (let ((credentials (database.get self (make-keyword provider))))
    (when credentials
      (case provider
	(facebook
	 (with-slots (app-id) credentials
	   (<fb:oauth-uri :client-id app-id
			  :redirect-uri (make-oauth-answer-uri self provider)
			  :state state)))
	(google
	 (with-slots (client-id client-secret) credentials
	   (<google:oauth-uri :client-id client-id
			      :redirect-uri (make-oauth-answer-uri self provider)
			      :state state)))
	(twitter
	 (with-slots (consumer-key consumer-secret) credentials
	   (let ((callback (make-oauth-answer-uri self provider)))
	     (uri.add-query callback "state" state)
	     (let ((token (<twitter:get-request-token 
			   :callback callback
			   :consumer-key consumer-key
			   :consumer-secret consumer-secret)))
	       (setf (gethash (<oauth1:request-token.token token)
			      (manager.request-token-cache self)) token)
	       (<twitter:authorize-url :token token)))))))))


(defmethod find-oauth1-account ((self manager-application) (provider symbol)
				(token string) (verifier string))
  (case provider
    (twitter
     (let ((credentials (database.get self (make-keyword provider))))
       (with-slots (consumer-key consumer-secret) credentials
	 (let ((req-token (gethash token (manager.request-token-cache self))))
	   (cond
	     ((null req-token)
	      (error "Twitter request token not found: ~A, verifier: ~A"
		     token verifier))
	     (t
	      (let* ((access-token
		       (<twitter:get-access-token :verifier verifier
						  :consumer-key consumer-key
						  :consumer-secret consumer-secret
						  :request-token req-token))
		     (twitter-id (<twitter:access-token.user-id access-token))
		     (screen-name (<twitter:access-token.screen-name access-token))
		     (twitter-user
		       (<twitter:secure-get-user :token access-token
						 :consumer-key consumer-key
						 :consumer-secret consumer-secret
						 :screen-name screen-name))
		     (account
		       (twitter-account.find self :account-id twitter-id)))

		(remhash token (manager.request-token-cache self))
		
		(if account
		    account
		    (multiple-value-bind (user account) (user.add-from-twitter self twitter-user)
		      (declare (ignore user))
		      account)))))))))))

(defmethod manager.get-oauth2-user ((self manager-application) (provider (eql 'facebook))
				    (code string))
  (with-slots (app-id app-secret) (database.get self :facebook)
     (<fb:me :token
      (<fb:get-access-token :client-id app-id
			    :client-secret app-secret
			    :code code
			    :redirect-uri (make-oauth-answer-uri self 'facebook)))))

(defmethod manager.get-oauth2-user ((self manager-application) (provider (eql 'google))
				    (code string))
  (with-slots (client-id client-secret) (database.get self :google)
    (<google:userinfo :token
     (<google:get-access-token :client-id client-id
			       :client-secret client-secret
			       :code code
			       :redirect-uri (make-oauth-answer-uri self 'google)))))

(defmethod manager.get-oauth1-user ((self manager-application) (provider (eql 'twitter))
				    (req-token <oauth1:request-token)
				    (token string) (verifier string))
  (with-slots (consumer-key consumer-secret) (database.get self :twitter)
    (let* ((access-token (<twitter:get-access-token :verifier verifier
						    :consumer-key consumer-key
						    :consumer-secret consumer-secret
						    :request-token req-token))
	   (twitter-id (<twitter:access-token.user-id access-token)))
      (<twitter:secure-get-user :token access-token :consumer-key consumer-key
				:consumer-secret consumer-secret
				:user-id twitter-id))))

(defmethod manager.create-oauth2-account ((self manager-application) (provider symbol)
					  (code string))
  (let ((credentials (database.get self (make-keyword provider))))
    (case provider
      (facebook
       (with-slots (app-id app-secret) credentials
	 (let* ((url (make-oauth-answer-uri self 'facebook))
		(token (<fb:get-access-token :client-id app-id
					     :client-secret app-secret
					     :code code :redirect-uri url))
		(fb-user (<fb:me :token token))
		(facebook-id (get-attribute fb-user :id))
		(account (facebook-account.find self :account-id facebook-id)))
   
	   (aif account
		account
		(multiple-value-bind (user account) (user.add-from-facebook self fb-user)
		  (declare (ignore user))
		  account)))))
      (google
       (with-slots (client-id client-secret) credentials
	 (let* ((url (make-oauth-answer-uri self 'google))
		(token (<google:get-access-token :client-id client-id
						 :client-secret client-secret
						 :code code :redirect-uri url))
		(google-user (<google:userinfo :token token))
		(google-id (get-attribute google-user :id))
		(account (google-account.find self :account-id google-id)))

	   (aif (google-account.find self :account-id google-id)
		account
	        (multiple-value-bind (user account) (user.add-from-google self google-user)
		  (declare (ignore user))
		  account))))))))

(defmethod manager.add-oauth2-account ((self manager-application) (provider symbol)
				       (code string))
  (let ((credentials (database.get self (make-keyword provider))))
    (case provider
      (facebook
       (with-slots (app-id app-secret) credentials
	 (let* ((url (make-oauth-answer-uri self 'facebook))
		(token (<fb:get-access-token :client-id app-id
					     :client-secret app-secret
					     :code code :redirect-uri url))
		(fb-user (<fb:me :token token))
		(facebook-id (get-attribute fb-user :id))
		(account (facebook-account.find self :account-id facebook-id)))
   
	   (aif account
		account
		(multiple-value-bind (user account) (user.add-from-facebook self fb-user)
		  (declare (ignore user))
		  account)))))
      (google
       (with-slots (client-id client-secret) credentials
	 (let* ((url (make-oauth-answer-uri self 'google))
		(token (<google:get-access-token :client-id client-id
						 :client-secret client-secret
						 :code code :redirect-uri url))
		(google-user (<google:userinfo :token token))
		(google-id (get-attribute google-user :id))
		(account (google-account.find self :account-id google-id)))

	   (aif (google-account.find self :account-id google-id)
		account
	        (multiple-value-bind (user account) (user.add-from-google self google-user)
		  (declare (ignore user))
		  account))))))))

;; -------------------------------------------------------------------------
;; Authentication Controller
;; -------------------------------------------------------------------------
(defhandler "auth\.core" ((self manager-application))
  (flet ((find-realm (return-to)
	   (let ((url (uri? (make-core-stream return-to))))
	     (if url (realm.find self :fqdn (uri.server url))))))
    (let* ((request (context.request +context+))
	   (referer (or (http-request.header request 'referer) (make-uri)))
	   (action (or (uri.query referer "action") "login"))
	   (return-to (uri.query referer "return-to"))
	   (realm (find-realm return-to)))
      (labels ((handle-action (component action &rest args)
		 (declare (ignore component))
		 (case action
		   (:login
		    (destructuring-bind (provider) args
		      (case provider
			((facebook google)
			 (let ((k-url (action/hash ((code "code"))
					(send/user
					 (setf (query-session :account)
					       (find-oauth2-account self provider code))))))
			   (continue/js (make-oauth-uri self provider k-url))))
			((yahoo twitter)
			 (let ((k-url (action/hash ((token "oauth_token")
						    (verifier "oauth_verifier"))
					(send/user
					 (setf (query-session :account)
					       (find-oauth1-account self provider
								    token verifier))))))
			   (continue/js (make-oauth-uri self provider k-url))))
			(t
			 (send/anonymous "error" (format nil "Unknown provider ~A (3)"
							 provider))))))
		   (:use
		    (destructuring-bind (account) args
		      (let* ((session-id (session.id (context.session +context+)))
			     (access-token (or (query-session :token)
					       (setf (query-session :token)
						     (manager.create-token self realm account
									   session-id)))))
			(with-slots (token) access-token
			  (continue/js
			   (jambda (self)
			     (setf (slot-value window 'location)
				   (+ return-to "?token=" token))))))))))
	       (send/user (account)
		 (apply #'handle-action
			(javascript/suspend
			 (lambda (stream)
			   (let* ((kontroller (make-auth-controller/user self account realm))
				  (kontroller
				    (authorize self (account.user account) kontroller)))
			     (rebinding-js/cc (kontroller) stream
			       (setf (slot-value window 'controller) (kontroller nil))))))))
	       (send/anonymous (page &optional error)
		 (apply #'handle-action
			(javascript/suspend
			 (lambda (stream)
			   (let* ((kontroller
				    (make-auth-controller/anonymous self :page page
									 :error-message error))
				  (kontroller (authorize self (make-anonymous-user) kontroller)))
			     (rebinding-js/cc (kontroller) stream
			       (setf (slot-value window 'controller) (kontroller nil)))))))))
	(cond	  
	  ((equal action "answer") ;; Redirect to exact continuation + avoid CSRF
	   (let ((state (uri.query referer "state"))
		 (url (http-request.uri request)))
	     (uri.add-query url +continuation-query-name+ state)
	     (mapcar (lambda (param)
		       (aif (uri.query referer param)
			    (uri.add-query url param it)))
		     '("code" "provider" "oauth_token" "oauth_verifier"))
	     (send/redirect (uri->string url))))
	  ((null realm)
	   (send/anonymous "error" "Sorry, realm not found. (3)"))	  
	  ((null return-to) ;; Show Error Message
	   (send/anonymous "error" "Sorry, return-to is missing. (1)"))
	  ((query-session :account)
	   (send/user (query-session :account)))
	  (t (send/anonymous action nil)))))))


;; http://node1.coretal.net:8080/auth.core?action=answer&provider=facebook&return-to=http%3A%2F%2Flocalhost%3A8080%2Fauthentication%2Fauth.core&code=AQBbuPsNJLwAvLrd7VgD-zpPlthxpIf4is3VsgBd-VHC6dIGQ8J7Fw9o3-zjrYmTpI9XgKZO62oFkhrK_CYQQajN14mzrfaotncX_KLBhykz3kMe_VpnWbrDcou0c0c9Ew4MWN4E-pHyCBz9fT_ysspl032fsY8oR-UFYYym_1nE4gcIrcZQjF6bNss5UDI9_4A#_=_
;; https://www.facebook.com/dialog/oauth?client_id=162577700491917&response-type=token&display=popup&scope=email&redirect_uri=http%3A%2F%2Fnode1%2Ecoretal%2Enet%3A8080%2Fauth%2Ecore%3Faction%3Danswer%26provider%3Dfacebook%26return%2Dto%3Dhttp%253A%252F%252Flocalhost%253A8080%252Fauthentication%252Fauth%252Ecore


;; (let ((account
;; 	(javascript/suspend
;; 	 (lambda (stream)
;; 	   (let* ((k-url
;; 		    (action/hash ((provider "provider") (code "code")
;; 				  (token "oauth_token")
;; 				  (verifier "oauth_verifier"))
;; 		      (let* ((provider (find-provider-symbol provider))
;; 			     (account
;; 			       (case provider
;; 				 ((or facebook google)
;; 				  (find-oauth2-account self provider code))
;; 				 (twitter
;; 				  (find-oauth1-account self provider
;; 						       token verifier)))))
;; 			(answer
;; 			 (or account
;; 			     (send/anonymous "error"
;; 					     "Sorry, user not found. (2)"))))))
;; 		  (kontroller
;; 		    (make-auth-controller/anonymous self :page page :state k-url
;; 							 :error-message error))
;; 		  (kontroller
;; 		    (authorize self (make-anonymous-user) kontroller)))
;; 	     (rebinding-js/cc (kontroller) stream
;; 	       (setf (slot-value window 'controller) (kontroller nil))))))))
;;   (setf (query-session :user) (account.user account))
;;   (send/user account))