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

(defcomponent dojo-widget (html-element)
  ((dojo-type :initarg :dojo-type :initform (error "Please specify :dojo-type"))
   (dojo-args :initarg :dojo-args :initform '())))

(defmethod/local render ((self dojo-widget))
  (with-html-output (http-response.stream (response +context+))
    (<:p "This is a dojo widget.")))

(defmethod/cc send/ctor ((self dojo-widget) remote-slots local-methods remote-methods)
  (<:js
   `(defun ,(class-name (class-of self)) ()
	(let ((o (create ,@remote-slots ,@local-methods ,@remote-methods))
	      (p (document.create-element o.tag)))
	  (doeach (property o)
	    (setf (slot-value p property) (slot-value o property)))
	  
	  (if (= "function" (typeof o.render))		
	      (setf p.inner-h-t-m-l (o.render)))
	  
;;	  (dojo.require "dojo.xml.Parse")
	  (dojo.require ,(format nil "~A" (js::symbol-to-js (slot-value self 'dojo-type))))
;;; 	  (let ((parser (new (dojo.xml.*parse)))
;;; 		(frag (parser.parse-element p nil t)))
;;; 	    (.create-components (dojo.widget.get-parser) frag))
	  (setf this.prototype
		(new (,(slot-value self 'dojo-type) (create ,@(slot-value self 'dojo-args)) p)))
	  (return this.prototype)))))

