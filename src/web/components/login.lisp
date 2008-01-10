(in-package :core-server)

(defcomponent login-component ()
  ())

(defmethod/remote authenticate ((self login-component) username password)
  (throw (new (*error "Please implement authenticate method."))))

(defmethod/local render-login-form ((self login-component))
  (<:div :id "login-dialog"
	 (<:form :action "#" :id "login-form"
;;; 	  ;;	    :onsubmit "return false;"
;;; 	  ;;	    :onsubmit "return trg.authenticate(this.username.value, this.password.value);"
		 (<:div :class "field-name" "Username:")
		 (<:div :class "field-value" (<:input :type "text" :name "username" :id "username-field"))
		 (<:div :class "field-name" "Password:")
		 (<:div :class "field-value" (<:input :type "password" :name "password" :id "password-field"))
		 (<:div :class "field-name")
		 (<:div :class "field-value" (<:input :type "submit" :value "Enter")))))

(defmethod/remote do-login ((self login-component))
  (let ((d (this.render-login-form)))
    (setf d.first-child.onsubmit
	  (dojo.hitch this (lambda ()
			     (let ((result
				    (this.authenticate
				     (slot-value (dojo.by-id "username-field") 'value)
				     (slot-value (dojo.by-id "password-field") 'value))))
			       (if result
				   (.hide (dijit.by-id "login-dialog"))
				   (alert "Sorry, username or password is wrong, please try again.")))
			     (return false))))
    (dojo.require "dijit.Dialog")
    (if (dijit.by-id "login-dialog")
	(.destroy (dijit.by-id "login-dialog")))
    
    (let ((dialog (new (dijit.*dialog (create :title "Coretal v2") d))))
      (dialog.show))
    (return false)))
