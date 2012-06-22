;; +-------------------------------------------------------------------------
;; | Authentication Components
;; +-------------------------------------------------------------------------
(in-package :core-server)

;; --------------------------------------------------------------------------
;; Login Box
;; --------------------------------------------------------------------------
(defcomponent login-box (<:div callable-component)
  ((_password-input :host remote
		    :initform (<core:password-input  :min-length 5))
   (_username-input :host remote :initform (<core:required-value-input)))
  (:default-initargs :id "loginBox"))

(defmethod/local answer-component ((self login-box) arg)
  (answer arg))

(defmethod/remote template ((self login-box))
  (let ((_username (make-component (_username-input self)
				   :class-name "text"
				   :type "text" :name "username"
				   :validation-span-id "username-validation"
				   :default-value "Username"))
	(_password (make-component (_password-input self)
				    :class-name "text"
				    :default-value "password"
				    :type "password" :name "password"
				    :validation-span-id "password-validation")))
    (list
     (<:form :onsubmit
	     (lifte (answer-component self
				      (cons (get-input-value _username)
					    (get-input-value _password))))
	     (with-field _username
	       (<:span :class "validation"
		       :id "username-validation" "Enter your username"))
	     (with-field _password
	       (<:span :class "validation"
		       :id "password-validation" "Enter your password"))
	     (with-field ""
	       (<:input :type "submit" :class "button"
			:value "login" :disabled t))))))

(defmethod/remote destroy ((self login-box))
  (remove-class self "core"))

(defmethod/remote init ((self login-box))
  (add-class self "core")
  (mapcar (lambda (a) (.append-child self a)) (template self)))

;; -------------------------------------------------------------------------
;; Registration Box
;; -------------------------------------------------------------------------
(defcomponent registration-box (<:div)
  ()
  (:default-initargs :id "registrationBox"))

(defmethod/remote template ((self registration-box))
  (list
   (<:form :action "#"
	   :onsubmit (event (e)
		       (with-call/cc
			 (answer-component self this.email.value))
		       false)
      (with-field
	  (<core:email-input :class-name "text" :type "text" :name "email"
			     :validation-span-id "email-validation"
			     :default-value "Email")
	(<:span :class "validation" :id "email-validation" "Enter your email address"))
      (with-field ""
	(<:input :type "submit" :class "button"
		 :value "login or register" :disabled t)))))

(defmethod/remote init ((self registration-box))
  (mapcar (lambda (a) (.append-child self a)) (template self)))

;;(defcomponent-ctor login)
;; (defhtmlcomponent-ctor login)

;; --------------------------------------------------------------------------
;; Authentication Component
;; --------------------------------------------------------------------------
;; (defcomponent <core:auth ()
;;   ((user :host local)))

;; (defmethod/local get-name ((self <core:auth))
;;   (slot-value (s-v 'user) 'name))

;; (defmethod/local is-authenticated ((self <core:auth))
;;   (and (s-v 'user) t))

;; --------------------------------------------------------------------------
;; Login Dialog
;; --------------------------------------------------------------------------
;; (defcomponent login-box (<:a)
;;   ())

;; (defcomponent login-component ()
;;   ())

;; (defmethod/remote authenticate ((self login-component) username password)
;;   (throw (new (*error "Please implement authenticate method."))))

;; (defmethod/local render-login-form ((self login-component))
;;   (<:div :id "login-dialog"
;; 	 (<:form :action "#" :id "login-form"
;; ;;; 	  ;;	    :onsubmit "return false;"
;; ;;; 	  ;;	    :onsubmit "return trg.authenticate(this.username.value, this.password.value);"
;; 		 (<:div :class "field-name" "Username:")
;; 		 (<:div :class "field-value" (<:input :type "text" :name "username" :id "username-field"))
;; 		 (<:div :class "field-name" "Password:")
;; 		 (<:div :class "field-value" (<:input :type "password" :name "password" :id "password-field"))
;; 		 (<:div :class "field-name")
;; 		 (<:div :class "field-value" (<:input :type "submit" :value "Enter")))))

;; (defmethod/remote do-login ((self login-component))
;;   (let ((d (this.render-login-form)))
;;     (setf d.first-child.onsubmit
;; 	  (dojo.hitch this (lambda ()
;; 			     (let ((result
;; 				    (this.authenticate
;; 				     (slot-value (dojo.by-id "username-field") 'value)
;; 				     (slot-value (dojo.by-id "password-field") 'value))))
;; 			       (if result
;; 				   (.hide (dijit.by-id "login-dialog"))
;; 				   (alert "Sorry, username or password is wrong, please try again.")))
;; 			     (return false))))
;;     (dojo.require "dijit.Dialog")
;;     (if (dijit.by-id "login-dialog")
;; 	(.destroy (dijit.by-id "login-dialog")))
    
;;     (let ((dialog (new (dijit.*dialog (create :title "Coretal v2") d))))
;;       (dialog.show))
;;     (return false)))

;; Core Server: Web Application Server

;; Copyright (C) 2006-2008  Metin Evrim Ulu, Aycan iRiCAN

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
