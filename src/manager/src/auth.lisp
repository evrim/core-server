(in-package :manager)

(defmethod manager.oauth-uri ((self manager-application))
  (let ((server (application.server self)))
    (make-uri :scheme "http"
	      :server (web-application.fqdn self)
	      :port (core-server::socket-server.port server)
	      :paths '(("auth.core")))))

(defmethod make-oauth-answer-uri ((self manager-application) provider return-to)
  (let ((url (manager.oauth-uri self)))
    (setf (uri.queries url)
	  `(("action" . "answer")
	    ("provider" . ,provider)
	    ("return-to" . ,(typecase return-to
			      (uri (uri->string return-to))
			      (string return-to)))))
    url))

;; http://node1.coretal.net:8080/auth.core?mode=return&type=facebook&code=CODE#_=_
;; http://localhost:8080/auth.core?action=login&return-to=http%3A%2F%2Fnode1.coretal.net%3A8080%2Fauthentication%2Fauth.core
(defmethod make-facebook-uri ((self manager-application) (return-to string))
  (let ((credentials (database.get self :facebook)))
    (if credentials
	(with-slots (app-id) credentials
	  (<fb:oauth-uri :client-id app-id
			 :redirect-uri (make-oauth-answer-uri self
					"facebook" return-to))))))

;; (defmethod get-google-associate ((self manager-application))
;;   (flet ((make-new-associate ()
;; 	   (setf (database.get self :google-associate) (<google:associate)))
;; 	 (get-key (key associate)
;; 	   (cdr (assoc key associate :test #'string=))))
;;     (let ((associate (database.get self :google-associate)))
;;       (cond
;; 	((null associate) (make-new-associate))
;; 	((let ((timestamp (get-key "timestamp" associate))
;; 	       (expires (parse-integer (get-key "expires_in" associate))))
;; 	   (> (+ (get-universal-time) 120) (+ timestamp expires)))
;; 	 (make-new-associate))
;; 	(t associate)))))

;; (defmethod make-google-uri ((self manager-application))
;;   (let ((association (get-google-associate self)))
;;     (<google:request-association
;;      :association association
;;      :return-to )))

;; (defmethod make-twitter-uri ((self manager-application))
;;   )

;; -------------------------------------------------------------------------
;; Authentication Controller
;; -------------------------------------------------------------------------
(defmethod/cc make-auth-controller ((self manager-application)
				    (page string) (return-to string))
  (<core:simple-controller :default-page page
   (<core:simple-page :name "login"
    (<core:simple-widget-map :selector "top-right"
			     :widget (<widget:simple-content
				      (<:p "Would you like to have a new account?"
				       (<:a :class "pad5"
					    :href "#page:register" "Sign Up"))))
    (<core:simple-widget-map :selector "middle"
			     :widget (<manager:login
				      :facebook-uri (make-facebook-uri self return-to)
				      ;; :twitter-uri (make-twitter-uri self return-to)
				      ;; :google-uri (make-google-uri self return-to)
				      ))
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
			     :widget (<manager:authentication-error)))))

;; -------------------------------------------------------------------------
;; Authentication Controller
;; -------------------------------------------------------------------------
(defhandler "auth-controller\.core" ((self manager-application)
				     (page "page") (return-to "return-to"))
  (javascript/suspend
   (lambda (stream)
     (let* ((controller (make-auth-controller self (or page "login") return-to))
	    (controller (authorize self (make-anonymous-user) controller)))
       (rebinding-js/cc (controller) stream
	 (setf (slot-value window 'controller) (controller nil)))))))

;; -------------------------------------------------------------------------
;; Authentication Page
;; -------------------------------------------------------------------------
(defmethod make-auth-template ((self manager-application) (controller-uri string))
  (<:html
   (<:head
    (<:title "Core Server - http://labs.core.gen.tr/")
    (<:meta :http--equiv "Content-Type" :content "text/html; charset=utf-8")
    (<:link :rel "stylesheet" :href "/style/reset.css")
    (<:link :rel "stylesheet" :href "/style/common.css")
    (<:link :rel "stylesheet" :href "/style/core.css")
    (<:link :rel "stylesheet" :href "/style/auth.css")
    (<:link :rel "stylesheet" :href "/style/auth-controller.css")
    (<:script :type "text/javascript" :src "library.core")
    (<:script :type "text/javascript" :src controller-uri))
   (<:body
    (<:div :class "header max-width center text-left"
	   (<:div :class "pad5 left"
		  (<:img :src "style/images/logo-black.png"))
	   (<:div :class "right font-12" :id "top-right"))
    (<:div :class "max-width center text-center clear" :id "middle")
    (<:div :class "center max-width clear"
	   (<:div :class "left" :id "bottom")
	   (<:div :class "core right"
		  (<:input :type "button" :class "close"
			   :onclick "window.close()"
			   :value "Close"))))))

(defmethod make-login-page ((self manager-application) (return-to string))
  (make-auth-template self (href "auth-controller.core" :page "login"
							:return-to return-to)))

(defmethod make-register-page ((self manager-application) (return-to string))
  (make-auth-template self (href "auth-controller.core" :page "register"
							:return-to return-to)))

(defmethod make-error-page ((self manager-application)
			    (return-to string) (message string))
  (make-auth-template self (href "auth-controller.core" :page "error"
							:return-to return-to
							:message message)))

;; http://node1.coretal.net:8080/auth.core?action=answer&provider=facebook&return-to=http%3A%2F%2Flocalhost%3A8080%2Fauthentication%2Fauth.core&code=AQBbuPsNJLwAvLrd7VgD-zpPlthxpIf4is3VsgBd-VHC6dIGQ8J7Fw9o3-zjrYmTpI9XgKZO62oFkhrK_CYQQajN14mzrfaotncX_KLBhykz3kMe_VpnWbrDcou0c0c9Ew4MWN4E-pHyCBz9fT_ysspl032fsY8oR-UFYYym_1nE4gcIrcZQjF6bNss5UDI9_4A#_=_
;; https://www.facebook.com/dialog/oauth?client_id=162577700491917&response-type=token&display=popup&scope=email&redirect_uri=http%3A%2F%2Fnode1%2Ecoretal%2Enet%3A8080%2Fauth%2Ecore%3Faction%3Danswer%26provider%3Dfacebook%26return%2Dto%3Dhttp%253A%252F%252Flocalhost%253A8080%252Fauthentication%252Fauth%252Ecore
(defhandler "auth\.core" ((self manager-application) (action "action")
			  (return-to "return-to") (provider "provider") (code "code"))
  (cond
    ((and (equal action "answer") provider)
     (cond
       ((equal "facebook" provider)
	(let ((credentials (database.get self :facebook)))
	  (cond
	    (credentials
	     (with-slots (app-id app-secret) credentials
	       (format t "~A" (uri->string (make-oauth-answer-uri self "facebook" return-to)))
	       (let ((token (<fb:get-access-token
			     :client-id app-id
			     :client-secret app-secret
			     :code code
			     :redirect-uri (make-oauth-answer-uri self "facebook"
					    return-to))))
		 (format nil "~A Got Token" token)))))))
       (t nil)))
    ((equal action "login") (make-login-page self return-to))
    ((equal action "register") (make-login-page self return-to))
    (t (make-error-page self return-to "Sorry, an error occured. (1)"))))


;; (defparameter +authentication-root+ "node1.coretal.net:8080")

;; (defmethod authentication-root ((self manager-application))
;;   +authentication-root+)

