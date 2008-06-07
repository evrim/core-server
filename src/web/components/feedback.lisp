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

(defcomponent feedback-component (toaster-component)
  ((feedback-id :accessor feedback-id :host remote :initform "feedback")
   (greeting-text :accessor greeting-text :host remote :initarg :greeting-text
		  :initform "Please give us feedback to improve our site. Click here to enter.")
   (thank-text :accessor thank-text :host remote
	       :initform "Thank you for giving us feedback."
	       :initarg :thank-text)
   (feedback-from :accessor feedback-from :host local :initform "nospam@core.gen.tr")))

(defmethod/remote get-div ((self feedback-component))
  (return ($ (this.get-feedback-id))))

(defmethod/local feedback-form ((self feedback-component))
  (<:form
   (<:input :type "text" :id "feedback-text" :name "feedback-text")))

(defmethod/local send-feedback ((self feedback-component) feedback url)
  (prog1 'true
    (sendmail (application.server (application self))
	      (feedback-from self)
	      (web-application.admin-email (application self))
	      "Feedback"
	      (with-core-stream/cc (s "")
		(with-html-output s
		  (<:html
		   (<:head (<:title "Feedback"))
		   (<:body
		    (<:p (format nil "We have got a feedback for ~A." (web-application.fqdn (application self))))
		    (<:table
		     (<:tr
		      (<:td "Date:")
		      (<:td (time->string (get-universal-time) :long)))
		     (<:tr
		      (<:td "Url:")
		      (<:td url))
		     (<:tr
		      (<:td "Text:")
		      (<:td feedback))))))
		(return-stream s)))))

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
					(this.send-feedback (escape input.value) (+ "" window.location))
					(this.setup)
					(this.toast (this.get-thank-text))
					(return false))))
      (setf input.value (this.get-greeting-text)
	    input.onfocus (dojo.hitch this
				      (lambda ()					
					(if (= (this.get-greeting-text) input.value)
					    (setf input.value ""))))))))
