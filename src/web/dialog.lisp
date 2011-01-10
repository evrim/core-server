(in-package :core-server)

;; +-------------------------------------------------------------------------
;; | Dialog
;; +-------------------------------------------------------------------------
(defcomponent dialog (<:div)
  ((overlay :host remote :initform nil)
   (message :host remote :initform "This is a message dialog.")
   (title :host remote :initform "message")
   (css-url :host remote :initform "http://www.coretal.net/style/dialog/dialog.css"))
  (:default-initargs :class "coretal-dialog"))

(defmethod/remote call-component ((self dialog))
  (show-component self)
  (call-next-method self))

(defmethod/remote answer-component ((self dialog) arg)  
  (hide-component self)
  (call-next-method self arg))

(defmethod/cc call-component ((self dialog))
  (call-next-method self))

(defmethod/cc answer-component ((self dialog) arg)
  (call-next-method self arg))

(defmethod/remote show-component ((self dialog))
  (load-css (css-url self))
  (window.scroll 0 0)
  (prepend document.body (overlay self))
  (prepend document.body self)
  (setf document.body.style.overflow "hidden"))

(defmethod/remote hide-component ((self dialog))
  (remove-css (css-url self))
  (setf document.body.style.overflow "visible")
  (.remove-child document.body self)
  (.remove-child document.body (overlay self)))

(defmethod/remote template ((self dialog))    
  (<:div :class "center text-center"
	 (<:div :class "left left-bg" (<:a :href "http://www.coretal.net/" ""))
	 (<:div :class "right right-bg"
		(<:div :class "title" (title self))
		(<:div :class "message" (message self))
		(<:form :action "#"
			(<:input :type "button" :class "button"
				 :value "OK"
				 :onclick (event (e)
					    (with-call/cc
					      (answer-component self t))
					    false))))))

(defmethod/remote init ((self dialog))
  (.append-child self (template self))
  (setf (overlay self) (<:div :class "coretal-dialog-overlay")))

;; +-------------------------------------------------------------------------
;; | Prompt Dialog
;; +-------------------------------------------------------------------------
(defcomponent prompt-dialog (dialog)
  ())

(defmethod/remote template ((self prompt-dialog))  
  (<:div :class "center text-center"
    (<:div :class "left left-bg" (<:a :href "http://www.coretal.net/" ""))
    (<:div :class "right right-bg"
	   (<:div :class "title" (title self))
	   (<:div :class "message" (message self))
	   (<:form :action "#"
		   (<:input :type "text" :name "prompt" :class "text")
		   (<:div
		    (<:input :type "submit" :class "button" :value "OK"
			     :onclick
			     (event (e)
			       (let ((button this))
				 (with-call/cc
				   (answer-component self button.form.prompt.value)))
				    false))
		    (<:input :type "button" :class "button"
			     :value "Cancel"
			     :onclick (lambda (e)
					(hide-component self))))))))

;; +-------------------------------------------------------------------------
;; | Yes-No Dialog
;; +-------------------------------------------------------------------------
(defcomponent yes-no-dialog (dialog)
  ()
  (:default-initargs :title "yes/no" :message "Do you want to answer Yes?"))

(defmethod/remote template ((self yes-no-dialog))  
  (<:div :class "center text-center"
    (<:div :class "left left-bg" (<:a :href "http://www.coretal.net/" ""))
    (<:div :class "right right-bg"
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
  ((email-input :host remote :initform (<core:email-input))
   (password-input :host remote :initform (<core:password-input)))
  (:default-initargs :title "login"))

(defmethod/remote template ((self login-dialog))    
  (<:div :class "center text-center"
    (<:div :class "left left-bg" (<:a :href "http://www.coretal.net/" ""))
    (<:div :class "right right-bg"
	   (<:div :class "title" (title self))
	   (<:form :action "#"
		   :onsubmit (event (e)			      
				    (let ((password this.password.value))
				      (with-call/cc
					(setf this.password.value nil)
					(answer-component self (cons this.email.value password))))
				    false)
		   (with-field
		       (<:span :class "validation"
			       :id "email-validation"
			       "Enter your email address")
		     (call/cc (email-input self)
			      (jobject :class-name "text" :type "text"
				       :name "email"
				       :validation-span-id "email-validation"
				       :default-value "Email")))
		   (with-field
		       (<:span :class "validation"
			       :id "password-validation"
			       "Enter your password")
		       (call/cc (password-input self)
				(jobject :class-name "text"
					 :default-value "password"
					 :type "password" :name "password"
					 :validation-span-id "password-validation")))
		   (with-field ""
		     (<:div (<:input :type "submit" :class "button"
				     :value "login or register" :disabled t)
			    (<:input :type "button" :class "button"
				     :value "cancel"
				     :onclick (lambda (e) (hide-component self)))))))))

;; +-------------------------------------------------------------------------
;; | Registration Dialog
;; +-------------------------------------------------------------------------
(defcomponent registration-dialog (dialog)
  ((email-input :host remote :initform (<core:email-input)))
  (:default-initargs :title "register"))

(defmethod/remote template ((self registration-dialog))    
  (<:div :class "center text-center"
    (<:div :class "left left-bg" (<:a :href "http://www.coretal.net/" ""))
    (<:div :class "right right-bg"
	   (<:div :class "title" (title self))
	   (<:form :action "#"
		   :onsubmit (event (e)
				    (with-call/cc
				      (answer-component self this.email.value))
				    false)
		   (with-field
		       (<:span :class "validation" :id "email-validation"
			       "Enter your email address")
		       (call/cc (email-input self)
				(jobject :class-name "text" :type "text"
					 :name "email"
					 :validation-span-id "email-validation"
					 :default-value "Email")))
		   (with-field ""
		     (<:input :type "submit" :class "button"
			      :value "login or register" :disabled t))))))

;; -------------------------------------------------------------------------
;; Forgot Password
;; -------------------------------------------------------------------------
(defcomponent forgot-password-dialog (dialog)
  ((email-input :host remote :initform (<core:email-input)))
  (:default-initargs :title "password"))

(defmethod/remote template ((self forgot-password-dialog))    
  (<:div :class "center text-center"
    (<:div :class "left left-bg" (<:a :href "http://www.coretal.net/" ""))
    (<:div :class "right right-bg"
	   (<:div :class "title" (title self))
	   (<:form :action "#"
		   :onsubmit (event (e)
			       (with-call/cc
				 (answer-component self this.email.value))
				    false)
		   (with-field
		       (<:span :class "validation" :id "email-validation"
			       "Enter your email address")
		     (call/cc (email-input self)
			      (jobject :class-name "text" :type "text"
				       :name "email"
				       :validation-span-id "email-validation"
				       :default-value "Email")))
		   (with-field ""
		     (<:input :type "submit" :class "button"
			      :value "send my password" :disabled t))))))

;; -------------------------------------------------------------------------
;; Big Dialog
;; -------------------------------------------------------------------------
(defcomponent big-dialog (dialog)
  ()
  (:default-initargs
    :class "coretal-big-dialog coretal-dialog"
    :title "Dialog"))

(defmethod/remote dialog-buttons ((self big-dialog))
  (<:div :class "buttons right pad10"
	 (<:input :type "button" :value "Close"
		  :onclick (lifte self.hide-component))))

(defmethod/remote template ((self big-dialog))
  (<:div :class "center text-center"
	 (<:div :class "left left-bg" (<:a :href "http://www.coretal.net/" ""))
	 (<:div :class "right right-bg2"
		(<:div :class "title" (title self))
		(<:div :class "bg-pad-top center text-center")
		(<:div :class "center content bg-white pad10" (message self))
		(<:div :class "clear bg-pad-bottom center text-center")
		(dialog-buttons self))))
