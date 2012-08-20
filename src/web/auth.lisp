;; +-------------------------------------------------------------------------
;; | Authentication Components
;; +-------------------------------------------------------------------------
(in-package :core-server)

;; --------------------------------------------------------------------------
;; Login Box
;; --------------------------------------------------------------------------
(defcomponent <core:login (<:div callable-component)
  ((_password-input :host remote
		    :initform
		    (<core:password-input :min-length 5 :name "password"
					  :default-value "Password"))
   (_username-input :host remote
		    :initform
		    (<core:required-value-input :name "username"
						:default-value "Username")))
  (:default-initargs :id "loginBox"))

(defmethod/local login-with-credentials ((self <core:login) username password)
  (answer (list username password)))

(defmethod/remote do-login-with-credentials ((self <core:login) username password)
  (login-with-credentials self username password))

(defmethod/remote buttons ((self <core:login))
  (with-field ""
      (<:input :type "submit" :class "button"
	       :value "login" :disabled t)))

(defmethod/remote template ((self <core:login))
  (let ((_username (make-component (_username-input self)
				   ;; :class-name "text"
				   :validation-span-id "username-validation"))
	(_password (make-component (_password-input self)
				   ;; :class-name "text"
				   :validation-span-id "password-validation")))
    (<:form :onsubmit (lifte
		       (do-login-with-credentials self (get-input-value _username)
			 (get-input-value _password)))
     (with-field _username
	 (<:span :class "validation"
		 :id "username-validation" "Enter your username"))
     (with-field _password
	 (<:span :class "validation"
		 :id "password-validation" "Enter your password"))
     (buttons self))))

(defmethod/remote destroy ((self <core:login))
  (remove-class self "core")
  (call-next-method self))

(defmethod/remote init ((self <core:login))
  (add-class self "core")
  (append self (template self)))


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
