(in-package :manager)

;; -------------------------------------------------------------------------
;; <manager:login-link - Simple Anchor that invokes Manager App Login Process
;; -------------------------------------------------------------------------
(defcomponent <manager:login-link (<:a secure-object)
  ((auth-uri :host remote :initform (error "Provide :auth-uri"))
   (return-uri :host remote :initform (error "Provide :return-uri"))
   (login-text :host remote :initform "Login")
   (logout-text :host remote :initform "Logout"))
  (:default-initargs
   :levels '(<manager:login-link/anonymous <manager:login-link/registered)
   :permissions '((OWNER . 1) (GROUP . 1) (OTHER . 1)
		  (ANONYMOUS . 0) (UNAUTHORIZED . -1))
   :owner (make-simple-user :name "Admin")
   :group (make-simple-group :name "users")))

(defcomponent <manager:login-link/anonymous (<:a secure-object/authorized)
  ((secure-object :host lift :type <manager:login-link)
   (auth-uri :host remote :lift t)
   (return-uri :host remote :lift t)
   (login-text :host remote :lift t)))

(defmethod/remote init ((self <manager:login-link/anonymous))
  (with-slots (protocol host pathname) (slot-value window 'location)
    (let* ((paths (reverse (cdr (.split pathname "/"))))
	   (paths (if (eq "" (car paths))
		      (.join (reverse (cons (return-uri self) (cdr paths))) "/")
		      (.join (reverse (cons (return-uri self) paths) "/"))))
	   (return-uri (+ protocol "//" host "/" paths))
	   (url (+ (auth-uri self) "?action=login&return-to="
		   (encode-u-r-i-component return-uri))))
      (_debug (list "url" url))
      (setf (slot-value self 'inner-h-t-m-l) (_ (login-text self))
	    (slot-value self 'onclick)
	    (lifte (pop-up url "Login" 600 350))))))

(defcomponent <manager:login-link/registered (<:a <widget:simple secure-object/authorized)
  ((secure-object :host lift :type <manager:login-link)
   (logout-text :host remote :lift t)))

(defmethod/remote init ((self <manager:login-link/registered))
  (setf (slot-value self 'inner-h-t-m-l) (_ (logout-text self))
	(slot-value self 'onclick) (lifte (do-logout (controller (widget-map self))))))

;; Core Server: Web Application Server

;; Copyright (C) 2006-2012  Metin Evrim Ulu

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
