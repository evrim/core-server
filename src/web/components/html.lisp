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

(in-package :core-server)

(defcomponent html-element (dom-element)
  ())

(defmethod/local render ((self html-element))
  nil)

;;; (with-yaclml-output-to-string
;;;     (<:div :id "56" :style "background-color:#FFF; color:#000"
;;; 	   (<:p "This is render of html-element")
;;; 	   (<:p "Aytek maraba!")))

(defmethod/cc send/ctor ((self html-element) remote-slots local-methods remote-methods)
  (<:js
   `(defun ,(class-name (class-of self)) ()
	(let ((o (create ,@remote-slots ,@local-methods ,@remote-methods))
	      (p (document.create-element o.tag)))
	  (doeach (property o)
	    (setf (slot-value p property) (slot-value o property)))
	  
	  (if (= "function" (typeof o.render))		
	      (setf p.inner-h-t-m-l (o.render)))

	  (setf this.prototype p)
	  (return p)))))

(defcomponent div-element (html-element)
  ()
  (:default-initargs :tag "div"))

(defmethod/local render ((self div-element))
  (with-html-output (http-response.stream (response +context+))
    (<:div "hobaaa")))