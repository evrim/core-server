;; -------------------------------------------------------------------------
;; OAuth Application Interface
;; -------------------------------------------------------------------------
(in-package :manager)

(defmethod oauth.make-answer-uri ((self manager-application) (provider symbol))
  (let ((url (manager.oauth-uri self)))
    (setf (uri.queries url) `(("action" . "answer")
			      ("provider" . ,(symbol->js provider))))
    url))

;; http://node1.coretal.net:8080/auth.core?mode=return&type=facebook&code=CODE#_=_
;; http://localhost:8080/auth.core?action=login&return-to=http%3A%2F%2Fnode1.coretal.net%3A8080%2Fauthentication%2Fauth.core
(defmethod oauth.make-uri ((self manager-application) (provider symbol) (state string))
  (let ((credentials (database.get self (make-keyword provider))))
    (when credentials
      (case provider
	(facebook
	 (with-slots (app-id) credentials
	   (<fb:oauth-uri :client-id app-id
			  :redirect-uri (oauth.make-answer-uri self provider)
			  :state state)))
	(google
	 (with-slots (client-id client-secret) credentials
	   (<google:oauth-uri :client-id client-id
			      :redirect-uri (oauth.make-answer-uri self provider)
			      :state state)))
	(twitter
	 (with-slots (consumer-key consumer-secret) credentials
	   (let ((callback (oauth.make-answer-uri self provider)))
	     (uri.add-query callback "state" state)
	     (let ((token (<twitter:get-request-token :callback callback
						      :consumer-key consumer-key
						      :consumer-secret consumer-secret)))
	       (values (<twitter:authorize-url :token token) token)))))))))

(defmethod oauth.get-user-v2 ((self manager-application) (provider (eql 'facebook))
			      (code string))
  (with-slots (app-id app-secret) (database.get self :facebook)
     (<fb:me :token
      (<fb:get-access-token :client-id app-id
			    :client-secret app-secret
			    :code code
			    :redirect-uri (oauth.make-answer-uri self 'facebook)))))

(defmethod oauth.get-user-v2 ((self manager-application) (provider (eql 'google))
			      (code string))
  (with-slots (client-id client-secret) (database.get self :google)
    (<google:userinfo :token
     (<google:get-access-token :client-id client-id
			       :client-secret client-secret
			       :code code
			       :redirect-uri (oauth.make-answer-uri self 'google)))))

(defmethod oauth.get-user-v1 ((self manager-application) (provider (eql 'twitter))
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


;; -------------------------------------------------------------------------
;; OAuth Handle
;; -------------------------------------------------------------------------
(defmethod oauth.handle ((self manager-application) (provider symbol)
			 (third-party jobject) &optional current-account)
  (let* ((account-id (get-attribute third-party
				    (case provider
				      (facebook :id)
				      (google :id)
				      (twitter :id_str))))
	 (account (external-account.find self :account-id account-id)))

    (cond
      ((and account current-account ;; Merge accounts
	    (not (eq (account.user account) (account.user current-account))))
       (let* ((old-user (account.user account))
	      (old-accounts (user.accounts old-user))
	      (new-user (account.user current-account)))
	 (mapcar (lambda (account) (account.update self account :user new-user))
		 old-accounts)
	 (user.delete self old-user)
	 account))
      (account account) ;; Already exists, return it
      ((and current-account (null account))
       ;; Previous accounts, adding a new one.
       (let ((new-account (funcall (case provider
				     (facebook #'facebook-account.add-from-jobject)
				     (google #'google-account.add-from-jobject)
				     (twitter #'twitter-account.add-from-jobject))
				   self third-party))
	     (user (account.user current-account)))
	 (user.update self user :accounts (cons new-account (user.accounts user)))
	 new-account))
      ((and (null current-account) (null account))
       ;; No previous accounts, new user, new account.
       (let ((new-account (funcall (case provider
				     (facebook #'facebook-account.add-from-jobject)
				     (google #'google-account.add-from-jobject)
				     (twitter #'twitter-account.add-from-jobject))
				   self third-party)))
	 (user.add self
		   :group (simple-group.find self :name "user")
		   :accounts (list new-account))
	 new-account)))))

(defmethod oauth.handle-v2 ((self manager-application) (provider symbol)
			    (code string) &optional current-account)
  (oauth.handle self provider (oauth.get-user-v2 self provider code)
		current-account))

(defmethod oauth.handle-v1 ((self manager-application)
			    (provider symbol) (request-token <oauth1:request-token)
			    (token string) (verifier string)
			    &optional current-account)
  (oauth.handle self provider
		(oauth.get-user-v1 self provider request-token token verifier)
		current-account))



;; (defmethod oauth.create-account-v1 ((self manager-application) (provider symbol)
;; 				    (code string))
;;   (let ((user (oauth.get-user-v2 self provider code)))
;;     (case provider
;;       (facebook
;;        (aif (facebook-account.find ))))))

;; (defmethod oauth.add-account-v1 ((self manager-application) (user user)
;; 				 (provider symbol) (code string))
;;   )

;; (defmethod oauth.find-account-v1 ((self manager-application) (provider symbol)
;; 				  (token string) (verifier string))
;;   (case provider
;;     (twitter
;;      (let ((credentials (database.get self (make-keyword provider))))
;;        (with-slots (consumer-key consumer-secret) credentials
;; 	 (let ((req-token (gethash token (manager.request-token-cache self))))
;; 	   (cond
;; 	     ((null req-token)
;; 	      (error "Twitter request token not found: ~A, verifier: ~A"
;; 		     token verifier))
;; 	     (t
;; 	      (let* ((access-token
;; 		       (<twitter:get-access-token :verifier verifier
;; 						  :consumer-key consumer-key
;; 						  :consumer-secret consumer-secret
;; 						  :request-token req-token))
;; 		     (twitter-id (<twitter:access-token.user-id access-token))
;; 		     (screen-name (<twitter:access-token.screen-name access-token))
;; 		     (twitter-user
;; 		       (<twitter:secure-get-user :token access-token
;; 						 :consumer-key consumer-key
;; 						 :consumer-secret consumer-secret
;; 						 :screen-name screen-name))
;; 		     (account
;; 		       (twitter-account.find self :account-id twitter-id)))

;; 		(remhash token (manager.request-token-cache self))
		
;; 		(if account
;; 		    account
;; 		    (multiple-value-bind (user account) (user.add-from-twitter self twitter-user)
;; 		      (declare (ignore user))
;; 		      account)))))))))))


;; (defmethod manager.create-oauth2-account ((self manager-application) (provider symbol)
;; 					  (code string))
;;   (let ((credentials (database.get self (make-keyword provider))))
;;     (case provider
;;       (facebook
;;        (with-slots (app-id app-secret) credentials
;; 	 (let* ((url (make-oauth-answer-uri self 'facebook))
;; 		(token (<fb:get-access-token :client-id app-id
;; 					     :client-secret app-secret
;; 					     :code code :redirect-uri url))
;; 		(fb-user (<fb:me :token token))
;; 		(facebook-id (get-attribute fb-user :id))
;; 		(account (facebook-account.find self :account-id facebook-id)))
   
;; 	   (aif account
;; 		account
;; 		(multiple-value-bind (user account) (user.add-from-facebook self fb-user)
;; 		  (declare (ignore user))
;; 		  account)))))
;;       (google
;;        (with-slots (client-id client-secret) credentials
;; 	 (let* ((url (make-oauth-answer-uri self 'google))
;; 		(token (<google:get-access-token :client-id client-id
;; 						 :client-secret client-secret
;; 						 :code code :redirect-uri url))
;; 		(google-user (<google:userinfo :token token))
;; 		(google-id (get-attribute google-user :id))
;; 		(account (google-account.find self :account-id google-id)))

;; 	   (aif (google-account.find self :account-id google-id)
;; 		account
;; 	        (multiple-value-bind (user account) (user.add-from-google self google-user)
;; 		  (declare (ignore user))
;; 		  account))))))))

;; (defmethod manager.add-oauth2-account ((self manager-application) (provider symbol)
;; 				       (code string))
;;   (let ((credentials (database.get self (make-keyword provider))))
;;     (case provider
;;       (facebook
;;        (with-slots (app-id app-secret) credentials
;; 	 (let* ((url (make-oauth-answer-uri self 'facebook))
;; 		(token (<fb:get-access-token :client-id app-id
;; 					     :client-secret app-secret
;; 					     :code code :redirect-uri url))
;; 		(fb-user (<fb:me :token token))
;; 		(facebook-id (get-attribute fb-user :id))
;; 		(account (facebook-account.find self :account-id facebook-id)))
   
;; 	   (aif account
;; 		account
;; 		(multiple-value-bind (user account) (user.add-from-facebook self fb-user)
;; 		  (declare (ignore user))
;; 		  account)))))
;;       (google
;;        (with-slots (client-id client-secret) credentials
;; 	 (let* ((url (make-oauth-answer-uri self 'google))
;; 		(token (<google:get-access-token :client-id client-id
;; 						 :client-secret client-secret
;; 						 :code code :redirect-uri url))
;; 		(google-user (<google:userinfo :token token))
;; 		(google-id (get-attribute google-user :id))
;; 		(account (google-account.find self :account-id google-id)))

;; 	   (aif (google-account.find self :account-id google-id)
;; 		account
;; 	        (multiple-value-bind (user account) (user.add-from-google self google-user)
;; 		  (declare (ignore user))
;; 		  account))))))))



;; (defmethod manager.find-account-v2 ((self manager-application) (provider symbol) (code string))
;;   (case provider
;;     (facebook
;;      (let ((fb-user (oauth.get-user-v2 self provider code)))
;;         (facebook-account.find self :account-id (get-attribute fb-user :id))))))

;; (defmethod manager.new-account-v2 ((self manager-application)
;; 				   (account account) (provider symbol) (code string))
;;   (case provider
;;     (facebook
;;      )))

