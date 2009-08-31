(in-package :core-server)

;; +----------------------------------------------------------------------------
;; | Dialog Component
;; +----------------------------------------------------------------------------
(defcomponent dialog (<:div)
  ((dialog.overlay :host remote :initform nil)
   (dialog.message :host remote :initform "This is a message dialog.")
   (dialog.title :host remote :initform "Message from server"))
  (:default-initargs :class-name "login"))

(defmethod/remote call-component ((self dialog))
  (show self)
  (call-next-method self))

(defmethod/remote answer-component ((self dialog) arg)  
  (hide self)
  (call-next-method self arg))

(defmethod/remote show ((self dialog))
  (load-css "style/login/login.css")
  (add-class document.body "coretal")
  (append document.body self)
  (append document.body (overlay self))
  (setf document.body.style.overflow "hidden"))

(defmethod/remote hide ((self dialog))
  (.remove-child document.body self)
  (.remove-child document.body (overlay self)))

(defmethod/remote template ((self dialog))  
  (array
   (<:div :class "left" (<:a :href "http://www.coretal.net/" ""))
   (<:div :class "right"
	  (<:div :class "title" (dialog.title self))
	  (<:div :class "message" (dialog.message self))
	  (<:form :action "#"
		  (<:input :type "button" :class "button"
			   :value "OK"
			   :onclick (event (e)
				      (with-call/cc (answer-component self t))
				      false))))))

(defmethod/remote init ((self dialog))
  (add-class self "login")
  (mapcar (lambda (a) (.append-child self a)) (template self))
  (setf (overlay self) (<:div :class "overlay")))

;; +----------------------------------------------------------------------------
;; | Message Dialog
;; +----------------------------------------------------------------------------
(defcomponent yes-no-dialog (dialog)
  ()
  (:default-initargs :title "yes/no" :message "Do you want to answer Yes?"))

(defmethod/remote template ((self yes-no-dialog))
  (array
   (<:div :class "left" (<:a :href "http://www.coretal.net/" ""))
   (<:div :class "right"
	  (<:div :class "title" (dialog.title self))
	  (<:div :class "message" (dialog.message self))
	  (<:form :action "#"
		  (<:input :type "button" :class "button"
			   :value "Yes"
			   :onclick (event (e)
				      (with-call/cc (answer-component self t))
				      false))
		  (<:input :type "button" :class "button"
			   :value "No"
			   :onclick (event (e)
				      (with-call/cc (answer-component self nil))
				      false))))))

;; +----------------------------------------------------------------------------
;; | Login Dialog
;; +----------------------------------------------------------------------------
(defcomponent login-dialog (dialog)
  ()
  (:default-initargs :title "login"))

(defmethod/remote template ((self login-dialog))  
  (array
   (<:div :class "left" (<:a :href "http://www.coretal.net/" ""))
   (<:div :class "right"
	  (<:div :class "title" (dialog.title self))
	  (<:form :action "#"
		  :onsubmit (event (e)
			      (with-call/cc
				(let ((password this.password.value))
				  (setf this.password.value nil)
				  (answer-component self (cons this.email.value password))))
			      false)
		  (with-field
		      (<core:email-input :class-name "text" :type "text" :name "email"
					 :validation-span-id "email-validation"
					 :default-value "Email")
		    (<:span :class "validation" :id "email-validation" "Enter your email address"))
		  (with-field
		      (<core:password-input :class-name "text" :default-value "password"
					    :type "password" :name "password"
					    :validation-span-id "password-validation")
		    (<:span :class "validation" :id "password-validation" "Enter your password"))
		  (with-field ""
		    (<:input :type "submit" :class "button"
			     :value "login or register" :disabled t))))))

;; +----------------------------------------------------------------------------
;; | Registration Dialog
;; +----------------------------------------------------------------------------
(defcomponent registration-dialog (dialog)
  ()
  (:default-initargs :title "register"))

(defmethod/remote template ((self registration-dialog))  
  (array
   (<:div :class "left" (<:a :href "http://www.coretal.net/" ""))
   (<:div :class "right"
	  (<:div :class "title" (dialog.title self))
	  (<:form :action "#"
		  :onsubmit (event (e)
			      (with-call/cc (answer-component self this.email.value))
			      false)
		  (with-field
		      (<core:email-input :class-name "text" :type "text" :name "email"
					 :validation-span-id "email-validation"
					 :default-value "Email")
		    (<:span :class "validation" :id "email-validation" "Enter your email address"))
		  (with-field ""
		    (<:input :type "submit" :class "button"
			     :value "login or register" :disabled t))))))