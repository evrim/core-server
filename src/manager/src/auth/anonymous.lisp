(in-package :manager)

;; -------------------------------------------------------------------------
;; Login Widget
;; -------------------------------------------------------------------------
(defcomponent <manager:login (<widget:simple <core:login)
  ((title :host remote :initform "Sign In")
   (subtitle :host remote :initform "Provide your credentials to log-in."))
  (:default-initargs
   :_username-input (<core:email-input :default-value "Email" :name "email"
				       :size "35")))

(defmethod/local external-login-url ((self <manager:login) provider)
  (let ((provider (find (string-upcase provider) '(facebook yahoo google twitter)
			:test #'string=)))
    (answer-component self (list self :login provider))))

(defmethod/remote do-external-login ((self <manager:login) provider)
  (setf window.location (external-login-url self provider)))

(defmethod/local core-server::login-with-credentials ((self <manager:login)
						      username password)
  (answer-component self (list self :login 'local username password)))

(defmethod/remote core-server::do-login-with-credentials ((self <manager:login)
							  username password)
  (let ((ctor (core-server::login-with-credentials self username password))
	(kontroller (controller (widget-map self))))
    (destroy kontroller)
    (set-parameter "page" nil)
    (call/cc ctor kontroller)))

(defmethod/remote core-server::buttons ((self <manager:login))
  (<:p
   (<:input :type "submit" :class "button"
	    :value "login" :disabled t)
   (<:a :href "#page:forgot" :class "pad10" "Forgot password?")))

(defmethod/remote template ((self <manager:login))
  (let ((form (call-next-method self)))
    (<:div
     (<:h1 (title self))
     (<:h2 (subtitle self))
     (<:div :class "left core" form)
     (<:div :class "right oauth-buttons"
      (<:p (<:a :class "btn-auth btn-facebook"
		:onclick (lifte (do-external-login self "facebook"))
	    (_"Sign In with ") (<:b "Facebook")))
      (<:p (<:a :class "btn-auth btn-twitter"
		:onclick (lifte (do-external-login self "twitter"))
	    (_"Sign In with ") (<:b "Twitter")))
      (<:p (<:a :class "btn-auth btn-google"
		:onclick (lifte (do-external-login self "google"))
	    (_"Sign In with ") (<:b "Google")))
      (<:p (<:a :class "btn-auth btn-yahoo"
		:onclick (lifte (do-external-login self "yahoo"))
	    (_"Sign In with ") (<:b "Yahoo")))))))

(defmethod/remote destroy ((self <manager:login))
  (remove-class self "core-auth-widget")
  (call-next-method self))

(defmethod/remote init ((self <manager:login))
  (call-next-method self)
  (add-class self "core-auth-widget"))

;; -------------------------------------------------------------------------
;; Forgot Password
;; -------------------------------------------------------------------------
(defcomponent <manager:forgot-password (<:div <widget:simple)
  ((title :host remote :initform "Recover Password")
   (subtitle :host remote
	     :initform "Please enter your email to recover your password")
   (_username-input :host remote
		    :initform(<core:email-input :default-value "Email"
						:name "email"
						:size "35"))))

(defmethod/local forgot-password ((self <manager:forgot-password) email)
  (answer-component self (list self :forgot email)))

(defmethod/remote do-forgot-password ((self <manager:forgot-password) email)
  (alert (_ (forgot-password self email)))
  (window.close))

(defmethod/remote template ((self <manager:forgot-password))
  (let ((_input (make-component (_username-input self)
				:validation-span-id "username-validation")))
    (<:div :class "core"
     (<:h1 (_ (title self)))
     (<:h2 (_ (subtitle self)))
     (<:form :onsubmit (lifte
			(do-forgot-password self (get-input-value _input)))
      (with-field _input
	  (<:span :class "validation"
	   :id "username-validation" "Enter your username"))
      (with-field ""
	  (<:input :type "submit" :class "button"
		   :value "Recover" :disabled t))))))

(defmethod/remote destroy ((self <manager:forgot-password))
  (remove-class self "core-auth-widget")
  (call-next-method self))

(defmethod/remote init ((self <manager:forgot-password))
  (call-next-method self)
  (add-class self "core-auth-widget")
  (append self (template self)))

;; -------------------------------------------------------------------------
;; Registration Widget
;; -------------------------------------------------------------------------
(defcomponent <manager:registration (<:div <widget:simple)
  ((_required-input :host remote :initform (<core:required-value-input))
   (_email-input :host remote :initform (<core:email-input))
   (_password-input :host remote :initform (<core:password-input))
   (_h1-text :host remote :initform "Registration")
   (_h2-text :host remote :initform "Sign-up to use Coretal.net Services"))
  
  (:default-initargs :id "registrationBox"))

(defmethod/remote make-elements ((self <manager:registration))
  (let ((_username (make-component (_required-input self)
				   :default-value (_ "Name")
				   :validation-span-id "XX-username-validation"
				   :size "35"))
	(_email (make-component (_email-input self)
				:default-value (_ "Email")
				:validation-span-id "XX-email-validation"
				:size "35"))
	(_password1 (make-component (_password-input self)
				    :default-value (_ "Enter Password")
				    :validation-span-id "XX-password-validation1"))
	(_password2 (make-component (_password-input self)
				    :default-value (_ "Re-enter Password")
				    :validation-span-id "XX-password-validation2"))
	(_agreement (call/cc (_required-input self)
			     (extend (jobject :validation-span-id
					      "XX-agreement-validation")
				     (<:input :type "checkbox")))))
    (add-class _agreement "pad5")
    (list _username _email _password1 _password2 _agreement)))

(defmethod/local register-with-credentials ((self <manager:registration) name
					    email password)
  (answer-component self (list self :register name email password)))

(defmethod/remote do-register-with-credentials ((self <manager:registration)
						name email password)
  (let ((ctor (register-with-credentials self name email password))
	(kontroller (controller (widget-map self))))
    (destroy kontroller)
    (set-parameter "page" nil)
    (call/cc ctor kontroller)))

(defmethod/remote template ((self <manager:registration))
  (destructuring-bind (_username _email _password1 _password2 _agreement) (make-elements self)
    (<:form :onsubmit (lifte
		       (do-register-with-credentials self (get-input-value _username)
			 (get-input-value _email) (get-input-value _password1)))
     (<:h1 (_ (_h1-text self)))
     (<:h2 (_ (_h2-text self)))
     (<:div :class "left width-50p"
      (with-field _username
	  (<:span :class "validation"
		  :id "XX-username-validation"
		  (_ "Enter your full name"))))
     (<:div (with-field _email
		(<:span :class "validation"
			:id "XX-email-validation"
			(_ "Enter your email address"))))
     (<:div :class "clear left width-50p"
      (with-field _password1
	  (<:span :class "validation" :id "XX-password-validation1"
	   (_ "Enter your password"))))
     (<:div (with-field _password2
		(<:span :class "validation" :id "XX-password2-validation2"
		 (_ "Re-enter your password"))))
     (<:div :class "left width-50p"
      (with-field
	  (<:p :class "agreement"
	   (<:label :for (slot-value _agreement 'id)
	    _agreement
	    (if (eq "tr" +default-language+)
		(<:span :class "pad5" " "
		 (<:a :href "/agreement.html"
		      :target "_blank"
		      (_ "agreement"))
		 " " (_ "I accept the terms in the"))
		(<:span :class "pad5" " "
		 (_ "I accept the terms in the") " "
		 (<:a :href "/agreement.html"
		      :target "_blank"
		      (_ "agreement") ".")))))
	  ""))
     (<:div  (with-field ""
		 (<:input :type "submit" :value (_ "Register") :disabled t))))))

(defmethod/remote destroy ((self <manager:registration))
  (remove-class self "core")
  (remove-class self "core-auth-widget")
  (call-next-method self))

(defmethod/remote init ((self <manager:registration))
  (call-next-method self)
  (add-class self "core-auth-widget")
  (add-class self "core")
  (append self (template self)))

;; -------------------------------------------------------------------------
;; Recover Password
;; -------------------------------------------------------------------------
(defcomponent <manager:recover-password (<:div <widget:simple)
  ((title :host remote :initform "Recover Password")
   (subtitle :host remote :initform "Please enter a new password")
   (_password-input :host remote :initform (<core:password-input))))

(defmethod/local recover-password ((self <manager:recover-password) password)
  (answer-component self (list self :recover password)))

(defmethod/remote do-recover-password ((self <manager:recover-password) password)
  (recover-password self password)
  (alert (+ (_ "Your password has been updated.") (_ "Thank you.")))
  (window.close))

(defmethod/remote template ((self <manager:recover-password))
  (let ((_input1 (make-component (_password-input self)
				 :validation-span-id "passwd1-validation"))
	(_input2 (make-component (_password-input self))))
    (<:div :class "core"
	   (<:h1 (_ (title self)))
	   (<:h2 (_ (subtitle self)))
	   (<:form :onsubmit (lifte (do-recover-password self
				      (get-input-value _input1)))
		   (with-field _input1
		     (<:span :id "passwd1-validation" :class "validation"))
		   (with-field _input2
		     (<:span :id "passwd2-validation" :class "validation"))
		   (<:p
		    (<:input :type "submit" :class "button"
			     :value (_"Reset Password")
			     :disabled t))))))

(defmethod/remote destroy ((self <manager:recover-password))
  (remove-class self "core-auth-widget")
  (call-next-method self))

(defmethod/remote init ((self <manager:recover-password))
  (call-next-method self)
  (add-class self "core-auth-widget")
  (append self (template self)))

;; -------------------------------------------------------------------------
;; Error Content
;; -------------------------------------------------------------------------
(defcomponent <manager:authentication-error (<widget:simple-content)
  ((message :host remote :initform "Sorry, an error occured.")))

(defmethod/remote destroy ((self <manager:authentication-error))
  (remove-class self "core-auth-widget")
  (call-next-method self))

(defmethod/remote init ((self <manager:authentication-error))
  (setf (core-server::content self)
	(list (<:h1 (_ "Authentication Error"))
	      (<:h2 (_ (or (get-parameter "message") (message self)
			   "Sorry, and error occured.")))))
  (add-class self "core-auth-widget")
  (call-next-method self))