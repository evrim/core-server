(in-package :manager)

;; -------------------------------------------------------------------------
;; Login Widget
;; -------------------------------------------------------------------------
(defcomponent <manager:login (<widget:simple <core:login)
  ((title :host remote :initform "Sign In")
   (subtitle :host remote :initform "Provide your credentials to log-in.")
   (facebook-uri :host remote)
   (twitter-uri :host remote)
   (google-uri :host remote))
  (:default-initargs
   :_username-input (<core:email-input :default-value "Email" :name "email"
				       :size "35")))

(defmethod/remote template ((self <manager:login))
  (let ((form (call-next-method self)))
    (<:div
     (<:h1 (title self))
     (<:h2 (subtitle self))
     (<:div :class "left core" form)
     (<:div :class "right buttons"
	    (<:p (<:a :class "btn-auth btn-facebook"
		      :href (facebook-uri self)
		      (_"Sign In with ") (<:b "Facebook")))
	    (<:p (<:a :class "btn-auth btn-twitter"
		      :href (twitter-uri self)
		      (_"Sign In with ") (<:b "Twitter")))
	    (<:p (<:a :class "btn-auth btn-google"
		      :href (google-uri self)
		      (_"Sign In with ") (<:b "Google")))))))

(defmethod/remote destroy ((self <manager:login))
  (call-next-method self))

(defmethod/remote init ((self <manager:login))
  (call-next-method self)
  (remove-class self "core")
  (add-class self "manager-login"))

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

(defmethod/remote template ((self <manager:registration))
  (destructuring-bind (_username _email _password1
				 _password2 _agreement) (make-elements self)
    (<:form :onsubmit
	    (lifte (answer-component self
				     (list "register"
					   (jobject
					    :username (get-input-value _username)
					    :email (get-input-value _email)
					    :password (get-input-value _password1)))))
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
  (remove-class self "manager-registration")
  (call-next-method self))

(defmethod/remote init ((self <manager:registration))
  (call-next-method self)
  (add-class self "manager-registration")
  (add-class self "core")
  (append self (template self)))


;; -------------------------------------------------------------------------
;; Error Content
;; -------------------------------------------------------------------------
(defcomponent <manager:authentication-error (<widget:simple-content)
  ((message :host remote :initform "Sorry, an error occured.")))

(defmethod/remote destroy ((self <manager:authentication-error))
  (remove-class self "manager-auth-error")
  (call-next-method self))

(defmethod/remote init ((self <manager:authentication-error))
  (setf (core-server::content self)
	(list (<:h1 (_ "Authentication Error"))
	      (<:h2 (or (get-parameter "message")
		     (message self)))))
  (add-class self "manager-auth-error")
  (call-next-method self))