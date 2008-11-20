;; +----------------------------------------------------------------------------
;; | Authentication Components
;; +----------------------------------------------------------------------------
(in-package :core-server)

;; ----------------------------------------------------------------------------
;; Login Box
;; ----------------------------------------------------------------------------
(defcomponent login (<:div)
  ())

(defmethod/local auth ((self login) username password)
  (answer/dispatch 'login username password))

(defmethod/remote template ((self login))
  (<:form :action "#"
	  :id "login-form"
	  :onsubmit (lambda (e)
		      (let ((form e.target))
			(self.auth form.username.value form.password.value)
			false))
	  (<:div :class "field-name" "Username:")
	  (<:div :class "field-value" (<:input :type "text" :name "username" :id "username-field"))
	  (<:div :class "field-name" "Password:")
	  (<:div :class "field-value" (<:input :type "password" :name "password" :id "password-field"))
	  (<:div :class "field-name")
	  (<:div :class "field-value" (<:input :type "submit" :value "Enter"))))

(defmethod/remote init ((self login))
  (setf this.inner-h-t-m-l "")
  (self.append-child (template self)))

;;(defcomponent-ctor login)
;; (defhtmlcomponent-ctor login)

;; ----------------------------------------------------------------------------
;; Authentication Component
;; ----------------------------------------------------------------------------
(defcomponent <core:auth ()
  ((user :host local)))

(defmethod/local get-name ((self <core:auth))
  (slot-value (s-v 'user) 'name))

(defmethod/local is-authenticated ((self <core:auth))
  (and (s-v 'user) t))

;; ----------------------------------------------------------------------------
;; Login Dialog
;; ----------------------------------------------------------------------------
(defcomponent login-box (<:a)
  ())

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
