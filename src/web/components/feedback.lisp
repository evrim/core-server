(in-package :core-server)

(defcomponent feedback-component (toaster-component)
  ((feedback-id :accessor feedback-id :host remote :initform "feedback")
   (greeting-text :accessor greeting-text :host remote
		  :initform "Please give us feedback to improve our site. Click here to enter.")
   (thank-text :accessor thank-text :host remote
	       :initform "Thank you for giving us feedback.")))

(defmethod/remote get-div ((self feedback-component))
  (return ($ (this.get-feedback-id))))

(defmethod/local feedback-form ((self feedback-component))
  (<:form
   (<:input :type "text" :id "feedback-text" :name "feedback-text")))

(defmethod/local send-feedback ((self feedback-component) feedback url)
  (sendmail (application.server (application self)) (web-application.admin-email (application self)) "Feedback"
	    (format nil "Date:~A~%Url:~A~%Feedback:~A~%"
		    (time->string (get-universal-time) :long)
		    url
		    feedback)))

(defmethod/remote setup ((self feedback-component))
  (if (= "undefined" (typeof (this.get-div)))
      (this.toast "Feedback div not found, aborting feedback component."))
    
  (setf (slot-value (this.get-div) 'inner-h-t-m-l) "")
  (let ((form (this.feedback-form)))
    (.append-child (this.get-div) form)
    (let ((input ($ "feedback-text")))      
      (setf form.onsubmit (dojo.hitch this
				      (lambda ()
					(this.toast "Sending...")
					(this.send-feedback input.value (+ "" window.location))
					(this.setup)
					(this.toast (this.get-thank-text))
					(return false))))
      (setf input.value (this.get-greeting-text)
	    input.onfocus (dojo.hitch this
				      (lambda ()					
					(if (= (this.get-greeting-text) input.value)
					    (setf input.value ""))))))))
