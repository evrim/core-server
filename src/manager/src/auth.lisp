(in-package :manager)

(defmethod manager.oauth-uri ((self manager-application))
  (let ((server (application.server self)))
    (make-uri :scheme "http"
	      :server (web-application.fqdn self)
	      :port (core-server::socket-server.port server)
	      :paths '(("oauth.html")))))

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
	(twitter nil)))))

;; -------------------------------------------------------------------------
;; Authentication Controller
;; -------------------------------------------------------------------------
(defmethod/cc make-auth-controller/anonymous ((self manager-application)
					      &key
					      (page "login")
					      (state (error "Provide :state"))
					      (error-message nil))
  (<core:simple-controller :default-page page 
   (<core:simple-page :name "login"
    (<core:simple-widget-map :selector "top-right"
			     :widget (<widget:simple-content
				      (<:p "Would you like to have a new account?"
				       (<:a :class "pad5"
					    :href "#page:register" "Sign Up"))))
    (<core:simple-widget-map :selector "middle"
			     :widget (<manager:login
				      :facebook-uri (make-oauth-uri self 'facebook state)
				      ;; :twitter-uri (make-twitter-uri self state)
				      :google-uri (make-oauth-uri self 'google state)))
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

;; (defmethod/cc make-auth-controller/registered ((self manager-application)
;; 					       &key
;; 					       (page "identity"))
;;   (<core:simple-controller :default-page page
;;    (<core:simple-page :name "identity"
;;     (<core:simple-widget-map :selector "middle"
;; 			     :widget (<manager:identity)))))

;; -------------------------------------------------------------------------
;; Authentication Controller
;; -------------------------------------------------------------------------
;; I personally love the code below. -evrim, 2012.
(defun foob (&rest args) (setf manager::*x args))
(defhandler "auth\.core" ((self manager-application))
  (labels ((handle-code (provider code return-to)
	     (let ((provider (find (string-upcase provider) '(facebook google twitter)
				   :test #'string=)))
	       (if (null provider)
		   (send/anonymous "error" return-to "Sorry, provider is unknown. (4)"))
	       
	       (let ((credentials (database.get self (make-keyword provider))))
		 (if (null credentials)
		     (send/anonymous "error" return-to "Sorry, internal error. (5)"))
		 
		 (case provider
		   (facebook
		    (with-slots (app-id app-secret) credentials
		      (let* ((url (make-oauth-answer-uri self "facebook"))
			     (token (<fb:get-access-token
				     :client-id app-id :client-secret app-secret
				     :code code :redirect-uri url))
			     (fb-user (<fb:me :token token))
			     (facebook-id (get-attribute fb-user :id)))
			;; (aif (fb-account.find self :id facebook-id)
			;;      (fb-account.update-from-jobject self fb-user)
			;;      (fb-account.add-from-jobject self fb-user))
			;; (foob token)
			(format nil "~A Got Token" token))))
		   (google
		    (with-slots (client-id client-secret) credentials
		      (let* ((url (make-oauth-answer-uri self "google"))
			     (token (<google:get-access-token
				     :client-id client-id
				     :client-secret client-secret
				     :code code :redirect-uri url))
			     ;; (fb-user (<fb:me :token token))
			     ;; (facebook-id (get-attribute fb-user :id))
			     )
			;; (aif (fb-account.find self :id facebook-id)
			;;      (fb-account.update-from-jobject self fb-user)
			;;      (fb-account.add-from-jobject self fb-user))
			(foob token)
			(format nil "~A Got Token" token))))
		   (t (break 'foo)
		    ;; (send/anonymous "error" return-to "Sorry, provider is missing. (2)")
		    )))))
	   (send/anonymous (page return-to error-message)
	     (javascript/suspend
	      (lambda (stream)
		(let* ((k-url (action/hash ((provider "provider") (code "code"))
				(handle-code provider code return-to)))
		       (controller (make-auth-controller/anonymous
				    self :page page :state k-url
				    :error-message error-message))
		       (controller (authorize self (make-anonymous-user)
					      controller)))
		  (rebinding-js/cc (controller) stream
		    (setf (slot-value window 'controller) (controller nil))))))))
    (let* ((request (context.request +context+))
	   (referer (or (http-request.header request 'referer) (make-uri)))
	   (action (or (uri.query referer "action") "login"))
	   (return-to (uri.query referer "return-to")))
      (cond
	((equal action "answer") ;; Redirect to exact continuation + avoid CSRF
	 (let ((state (uri.query referer "state"))
	       (code (uri.query referer "code"))
	       (provider (uri.query referer "provider"))
	       (url (http-request.uri request)))
	   (cond
	     ((null code)
	      (send/anonymous "error" nil "Sorry, code is missing. (2)"))
	     ((null provider)
	      (send/anonymous "error" nil "Sorry, provider is missing. (3)"))
	     (t (uri.add-query url +continuation-query-name+ state)
		(uri.add-query url "code" code)
		(uri.add-query url "provider" provider)
		(send/redirect (uri->string url))))))
	((null return-to) ;; Show Error Message
	 (send/anonymous "error" return-to "Sorry, return-to is missing. (1)"))
	(t (send/anonymous action return-to nil))))))

;; (defun foob (token) (setf manager::*token token))
;; (defun moob (code) (setf manager::*code code))
;; (defhandler "auth\.core" ((self manager-application) (action "action")
;; 			  (return-to "return-to") (provider "provider") (code "code"))
;;   (cond
;;     ((and (equal action "answer") provider)
;;      (cond
;;        ((equal "facebook" provider)
;; 	(let ((credentials (database.get self :facebook)))
;; 	  (cond
;; 	    (credentials
;; 	     (with-slots (app-id app-secret) credentials
;; 	       (let* ((token (<fb:get-access-token
;; 			      :client-id app-id :client-secret app-secret
;; 			      :code code
;; 			      :redirect-uri (make-oauth-answer-uri self "facebook"
;; 					     return-to)))
;; 		      (fb-user (<fb:me :token token))
;; 		      (facebook-id (get-attribute fb-user :id)))
;; 		 ;; (aif (fb-account.find self :id facebook-id)
;; 		 ;;      (fb-account.update-from-jobject self fb-user)
;; 		 ;;      (fb-account.add-from-jobject self fb-user))
;; 		 (foob token)
;; 		 (format nil "~A Got Token" token)))))))
;;        ((equal "google" provider)
;; 	(moob code)
;; 	(break code))
;;        (t nil)))
;;     ((equal action "login") (make-login-page self return-to))
;;     ((equal action "register") (make-login-page self return-to))
;;     (t (make-error-page self return-to "Sorry, an error occured. (1)"))))






















;; (defparameter +authentication-root+ "node1.coretal.net:8080")

;; (defmethod authentication-root ((self manager-application))
;;   +authentication-root+)

;; ;; -------------------------------------------------------------------------
;; ;; Authentication Page
;; ;; -------------------------------------------------------------------------
;; (defmethod make-auth-template ((self manager-application) (controller-uri string))
;;   (<:html
;;    (<:head
;;     (<:title "Core Server - http://labs.core.gen.tr/")
;;     (<:meta :http--equiv "Content-Type" :content "text/html; charset=utf-8")
;;     (<:link :rel "stylesheet" :href "/style/reset.css")
;;     (<:link :rel "stylesheet" :href "/style/common.css")
;;     (<:link :rel "stylesheet" :href "/style/core.css")
;;     (<:link :rel "stylesheet" :href "/style/auth.css")
;;     (<:link :rel "stylesheet" :href "/style/auth-controller.css")
;;     (<:script :type "text/javascript" :src "library.core")
;;     (<:script :type "text/javascript" :src controller-uri))
;;    (<:body
;;     (<:div :class "header max-width center text-left"
;; 	   (<:div :class "pad5 left"
;; 		  (<:img :src "style/images/logo-black.png"))
;; 	   (<:div :class "right font-12" :id "top-right"))
;;     (<:div :class "max-width center text-center clear" :id "middle")
;;     (<:div :class "center max-width clear"
;; 	   (<:div :class "left" :id "bottom")
;; 	   (<:div :class "core right"
;; 		  (<:input :type "button" :class "close"
;; 			   :onclick "window.close()"
;; 			   :value "Close"))))))
;; http://node1.coretal.net:8080/auth.core?action=answer&provider=facebook&return-to=http%3A%2F%2Flocalhost%3A8080%2Fauthentication%2Fauth.core&code=AQBbuPsNJLwAvLrd7VgD-zpPlthxpIf4is3VsgBd-VHC6dIGQ8J7Fw9o3-zjrYmTpI9XgKZO62oFkhrK_CYQQajN14mzrfaotncX_KLBhykz3kMe_VpnWbrDcou0c0c9Ew4MWN4E-pHyCBz9fT_ysspl032fsY8oR-UFYYym_1nE4gcIrcZQjF6bNss5UDI9_4A#_=_
;; https://www.facebook.com/dialog/oauth?client_id=162577700491917&response-type=token&display=popup&scope=email&redirect_uri=http%3A%2F%2Fnode1%2Ecoretal%2Enet%3A8080%2Fauth%2Ecore%3Faction%3Danswer%26provider%3Dfacebook%26return%2Dto%3Dhttp%253A%252F%252Flocalhost%253A8080%252Fauthentication%252Fauth%252Ecore
