;; -------------------------------------------------------------------------
;; OAuth Controller
;; -------------------------------------------------------------------------
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
      (labels ((handle-oauth2 (provider code)
		 (send/user
		  (setf (query-session :account)
			(oauth.handle-v2 self provider code (query-session :account)))))
	       (handle-oauth1 (provider request-token token verifier)
		 (send/user
		  (setf (query-session :account)
			(oauth.handle-v1 self provider request-token token verifier
					 (query-session :account)))))
	       (handle-action (component action &rest args)
		 (declare (ignore component))
		 (case action
		   (:login
		    (destructuring-bind (provider) args
		      (case provider
			((facebook google)
			 (let ((k-url (action/hash ((code "code"))
					(handle-oauth2 provider code))))
			   (continue/js (oauth.make-uri self provider k-url))))
			((yahoo twitter)
			 (let ((+action-hash-override+
				 (format nil "act-~A" (make-unique-random-string 8))))
			   (multiple-value-bind (url request-token)
			       (oauth.make-uri self provider +action-hash-override+)
			     (let ((k-url (action/hash ((token "oauth_token")
							(verifier "oauth_verifier"))
					    (handle-oauth1 provider request-token
							   token verifier))))
			       (assert (equal k-url +action-hash-override+))
			       (continue/js url)))))
			(t
			 (send/anonymous "error" (format nil "Unknown provider ~A (2)"
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

