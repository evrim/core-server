(in-package :core-server)

;; +-------------------------------------------------------------------------
;; | Dialog
;; +-------------------------------------------------------------------------
(defcomponent dialog (<:div)
  ((overlay :host remote :initform nil)
   (message :host remote :initform "This is a message dialog.")
   (title :host remote :initform "message"))
  (:default-initargs :class-name "login"))

(defmethod/remote call-component ((self dialog))
  (show-component self)
  (call-next-method self))

(defmethod/remote answer-component ((self dialog) arg)  
  (hide-component self)
  (call-next-method self arg))

(defmethod/cc call-component ((self dialog))
  (call-next-method self))

(defmethod/cc answer-componetn ((self dialog) arg)
  (call-next-method self arg))

(defmethod/remote show-component ((self dialog))
  (load-css "style/login/login.css")
  (add-class document.body "coretal")
  (append document.body self)
  (append document.body (overlay self))
  (setf document.body.style.overflow "hidden"))

(defmethod/remote hide-component ((self dialog))
  (.remove-child document.body self)
  (.remove-child document.body (overlay self)))

(defmethod/remote template ((self dialog))  
  (list
   (<:div :class "left" (<:a :href "http://www.coretal.net/" ""))
   (<:div :class "right"
	  (<:div :class "title" (title self))
	  (<:div :class "message" (message self))
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

;; +-------------------------------------------------------------------------
;; | Prompt Dialog
;; +-------------------------------------------------------------------------
(defcomponent prompt-dialog (dialog)
  ())

(defmethod/remote template ((self prompt-dialog))
  (list
   (<:div :class "left" (<:a :href "http://www.coretal.net/" ""))
   (<:div :class "right"
	  (<:div :class "title" (title self))
	  (<:div :class "message" (message self))
	  (<:form :action "#"		  
		  (<:input :type "text" :name "prompt")
		  (<:input :type "submit" :class "button" :value "OK"
			   :onclick (event (e)
				      (let ((button this))
					(with-call/cc
					  (answer-component self button.form.prompt.value)))
				      false))))))

;; +-------------------------------------------------------------------------
;; | Yes-No Dialog
;; +-------------------------------------------------------------------------
(defcomponent yes-no-dialog (dialog)
  ()
  (:default-initargs :title "yes/no" :message "Do you want to answer Yes?"))

(defmethod/remote template ((self yes-no-dialog))
  (list
   (<:div :class "left" (<:a :href "http://www.coretal.net/" ""))
   (<:div :class "right"
	  (<:div :class "title" (title self))
	  (<:div :class "message" (message self))
	  (<:form :action "#"
		  (<:input :type "button" :class "button"
			   :value "Yes"
			   :onclick (event (e)
				      (with-call/cc
					(answer-component self t))
				      false))
		  (<:input :type "button" :class "button"
			   :value "No"
			   :onclick (event (e)
				      (with-call/cc
					(answer-component self nil))
				      false))))))

;; +-------------------------------------------------------------------------
;; | Login Dialog
;; +-------------------------------------------------------------------------
(defcomponent login-dialog (dialog)
  ()
  (:default-initargs :title "login"))

(defmethod/remote template ((self login-dialog))  
  (list
   (<:div :class "left" (<:a :href "http://www.coretal.net/" ""))
   (<:div :class "right"
      (<:div :class "title" (title self))
      (<:form :action "#"
	      :onsubmit (event (e)			      
			  (let ((password this.password.value))
			    (with-call/cc
			      (setf this.password.value nil)
			      (answer-component self (cons this.email.value password))))
			  false)
	(with-field
	    (<core:email-input :class-name "text" :type "text" :name "email"
			       :validation-span-id "email-validation"
			       :default-value "Email")
	  (<:span :class "validation"
		  :id "email-validation" "Enter your email address"))
	(with-field
	    (<core:password-input :class-name "text"
				  :default-value "password"
				  :type "password" :name "password"
				  :validation-span-id "password-validation")
	  (<:span :class "validation"
		  :id "password-validation" "Enter your password"))
	(with-field ""
	  (<:input :type "submit" :class "button"
		   :value "login or register" :disabled t))))))

;; +-------------------------------------------------------------------------
;; | Registration Dialog
;; +-------------------------------------------------------------------------
(defcomponent registration-dialog (dialog)
  ()
  (:default-initargs :title "register"))

(defmethod/remote template ((self registration-dialog))  
  (list
   (<:div :class "left" (<:a :href "http://www.coretal.net/" ""))
   (<:div :class "right"
	  (<:div :class "title" (title self))
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