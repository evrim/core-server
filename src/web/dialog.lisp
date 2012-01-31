(in-package :core-server)

;; +-------------------------------------------------------------------------
;; | Dialog
;; +-------------------------------------------------------------------------
(defcomponent dialog (<:div callable-component cached-component)
  ((overlay :host remote :initform nil)
   (message :host remote :initform "This is a message dialog.")
   (title :host remote :initform "message")
   (css-url :host remote :initform "/style/dialog/dialog.css")
   (_scroll :host remote :initform (list 0 0)))
  (:default-initargs :class "coretal coretal-dialog"))

(defmacro/js message (self) `(get-message ,self))
(defmethod/remote get-message ((self dialog))
  (slot-value self 'message))

(defmacro/js title (self) `(get-title ,self))
(defmethod/remote get-title ((self dialog))
  (slot-value self 'title))

(defmethod/remote destroy ((self dialog))
  (hide-component self)
  (delete-slots self 'overlay 'message 'title 'css-url '_scroll)
  (call-next-method self))

(defmethod/remote call-component ((self dialog))
  (show-component self)
  (call-next-method self))

(defmethod/remote answer-component ((self dialog) arg)
  (destroy self)
  (call-next-method self arg))

(defmethod/cc call-component ((self dialog))
  (call-next-method self))

(defmethod/cc answer-component ((self dialog) arg)
  (call-next-method self arg))

(defmethod/remote show-component ((self dialog))
  (load-css (css-url self))
  (setf (_scroll self)
	(list (or document.document-element.scroll-left window.page-x-offset)
	      (or document.document-element.scroll-top window.page-y-offset)))
  (window.scroll 0 0)
  (prepend document.body (overlay self))
  (prepend document.body self)
  (setf document.body.style.overflow "hidden"))

(defmethod/remote hide-component ((self dialog))
  (remove-css (css-url self))
  (setf document.body.style.overflow "visible")  
  (.remove-child document.body self)
  (.remove-child document.body (overlay self))
  (let ((scroll (_scroll self)))
    (window.scroll (car scroll) (car (cdr scroll)))))

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

;; -------------------------------------------------------------------------
;; Supply Dialog Mixin
;; -------------------------------------------------------------------------
(defcomponent supply-dialog ()
  ())

(defmethod/local make-dialog ((self supply-dialog))
  (dialog))

;; +-------------------------------------------------------------------------
;; | Prompt Dialog
;; +-------------------------------------------------------------------------
(defcomponent prompt-dialog (dialog)
  ())

(defmethod/remote template ((self prompt-dialog))
  (let ((_prompt (<:input :type "text" :name "prompt" :class "text")))
    (<:div :class "center text-center"
	   (<:div :class "left left-bg" (<:a :href "http://www.coretal.net/" ""))
	   (<:div :class "right right-bg"
		  (<:div :class "title" (title self))
		  (<:div :class "message" (message self))
		  (<:form :action "#"
			  _prompt
			  (<:div :class "buttons"
			   (<:input :type "submit" :class "button"
				    :value (_"OK")
				    :onclick (lifte
					      (answer-component self
						  (slot-value _prompt 'value))))
			   (<:input :type "button" :class "button"
				    :value (_"Cancel")
				    :onclick (lifte (hide-component self)))))))))

;; -------------------------------------------------------------------------
;; Supply Prompt Dialog Mixin
;; -------------------------------------------------------------------------
(defcomponent supply-prompt-dialog ()
  ())

(defmethod/local make-prompt-dialog ((self supply-prompt-dialog))
  (prompt-dialog))

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
	    (<:div :class "buttons"
		   (<:input :type "button" :class "button"
			    :value (_"Yes")
			    :onclick (lifte (answer-component self t)))
		   (<:input :type "button" :class "button"
			    :value (_"No")
			    :onclick (lifte (answer-component self nil))))))))

;; -------------------------------------------------------------------------
;; Supply Yes-no-dialog Mixin
;; -------------------------------------------------------------------------
(defcomponent supply-yes-no-dialog ()
  ())

(defmethod/local make-yes-no-dialog ((self supply-yes-no-dialog))
  (yes-no-dialog))

;; +-------------------------------------------------------------------------
;; | Login Dialog
;; +-------------------------------------------------------------------------
(defcomponent login-dialog (dialog)
  ((default-email :host remote :initform "Email")
   (email-input :host remote :initform (<core:email-input))
   (password-input :host remote :initform (<core:password-input)))
  (:default-initargs :title "login"))

(defmethod/remote template ((self login-dialog))  
  (let ((_email (call/cc (email-input self)
			 (jobject :class-name "text" :type "text"
				  :value (default-email self) 
				  :name "email"
				  :validation-span-id "email-validation"
				  :default-value "Email")))
	(_password (call/cc (password-input self)
			    (jobject :class-name "text" :default-value "password"
				     :type "password" :name "password"
				     :validation-span-id "password-validation"))))
    (<:div :class "center text-center"
	   (<:div :class "left left-bg" (<:a :href "http://www.coretal.net/" ""))
	   (<:div :class "right right-bg"
		  (<:div :class "title" (title self))
		  (<:form :action "#"
			  :onsubmit (event (e)			      
				     (let ((password (slot-value _password 'value)))
				       (with-call/cc
					 (setf (slot-value _password 'value) nil)
					 (answer-component self
				          (cons (slot-value _email 'value) password))))
				     false)
			  (with-field (<:span :class "validation" :id "email-validation"
					      "Enter your email address")
			    _email)
			  (with-field (<:span :class "validation" :id "password-validation"
					      "Enter your password")
			    _password)
			  (with-field ""
			    (<:div (<:input :type "submit" :class "button"
					    :value "login" :disabled t)
				   (<:input :type "button" :class "button"
					    :value "cancel"
					    :onclick (lifte (hide-component self))))))))))

;; +-------------------------------------------------------------------------
;; | Registration Dialog
;; +-------------------------------------------------------------------------
(defcomponent registration-dialog (dialog)
  ((email-input :host remote :initform (<core:email-input)))
  (:default-initargs :title "register"))

(defmethod/remote template ((self registration-dialog))
  (let ((_email (call/cc (email-input self)
			 (jobject :class-name "text" :type "text"
				  :name "email"
				  :validation-span-id "email-validation"
				  :default-value "Email"))))
    (<:div :class "center text-center"
	   (<:div :class "left left-bg" (<:a :href "http://www.coretal.net/" ""))
	   (<:div :class "right right-bg"
		  (<:div :class "title" (title self))
		  (<:form :action "#"
			  :onsubmit (lifte
				     (answer-component self
						       (slot-value _email 'value)))
			  (with-field (<:span :class "validation"
					      :id "email-validation"
					      "Enter your email address")
			    _email)
			  (with-field ""
			    (<:input :type "submit" :class "button"
				     :value "login or register" :disabled t)))))))

;; -------------------------------------------------------------------------
;; Forgot Password
;; -------------------------------------------------------------------------
(defcomponent forgot-password-dialog (dialog)
  ((email-input :host remote :initform (<core:email-input)))
  (:default-initargs :title "password"))

(defmethod/remote template ((self forgot-password-dialog))
  (let ((_email (call/cc (email-input self)
			 (jobject :class-name "text" :type "text"
				  :name "email"
				  :validation-span-id "email-validation"
				  :default-value "Email"))))
    (<:div :class "center text-center"
	   (<:div :class "left left-bg" (<:a :href "http://www.coretal.net/" ""))
 	   (<:div :class "right right-bg"
		  (<:div :class "title" (title self))
		  (<:form :action "#"
			  :onsubmit (lifte
				     (answer-component self
						       (slot-value _email 'value)))
			  (with-field (<:span :class "validation"
					      :id "email-validation"
					      "Enter your email address")
			    _email)
			  (with-field ""
			    (<:input :type "submit" :class "button"
				     :value "send my password" :disabled t)))))))

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
		  :onclick (lifte (hide-component self)))))

(defmethod/remote template ((self big-dialog))
  (<:div :class "center text-center"
	 (<:div :class "left left-bg" (<:a :href "http://www.coretal.net/" ""))
	 (<:div :class "right right-bg2"
		(<:div :class "title" (title self))
		(<:div :class "bg-pad-top center text-center")
		(<:div :class "center content bg-white pad10" (message self))
		(<:div :class "clear bg-pad-bottom center text-center")
		(dialog-buttons self))))

;; -------------------------------------------------------------------------
;; Full Screen Dialog
;; -------------------------------------------------------------------------
(defcomponent fullscreen-dialog (dialog)
  ()
  (:default-initargs :class "coretal-fullscreen-dialog" :title ""))

(defmethod/remote template ((self fullscreen-dialog))
  (<:div :class "center text-center"	 
	 (<:h1 "I am a fullscreen dialog")
	 (<:p "Lorem ipsum ..")))

(defmethod/remote init ((self fullscreen-dialog))
  (call-next-method self)
  (append self (<:a :onclick (lifte (hide-component self))
		    :title (_"Close")
		    :class "close-button"
		    (<:img :src (+ "http://www.coretal.net/"
				   "style/images/close.jpg")))))