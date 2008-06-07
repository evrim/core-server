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

;; form component
(defcomponent web-form-component (toaster-component)
  ((form-id :host remote :initarg :form-id :initform "feedback"
	    :documentation "id of the form element")))

;; return form element
(defmethod/remote form ((self web-form-component))
  (return ($ this.form-id)))

;; send form as html
(defmethod/local sendform ((self web-form-component) form)
  (let ((f (format nil "<html><head><meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\"></head><body><div id=\"form\">~A</div></body></html>" (cl-ppcre::regex-replace-all "\\\\n" form ""))))
    (sendmail
     (application.server (application self)) ;; mail-sender
     (format nil "noreply@~A" (web-application.fqdn (application self))) ;; from
     (web-application.admin-email (application self)) ;; to
     "A form has been submitted." ;; subject
     f ;; html-message
     )))

;; add value attributes with current input values
(defmethod/remote setformvals ((self web-form-component))
  (.for-each (dojo.query "*" (this.get-form-id))
	     (lambda (i)
	       (case (i.tag-name.to-lower-case)
		 ("textarea"
		  (i.append-child (document.create-text-node i.value)))
		 ("input"
		  (case i.node-type
		    ("checkbox"
		     false)
		    ("radio"
		     (when i.checked
		       (i.set-attribute "checked" "true")))
		    (t
		     (i.set-attribute "value" i.value))))))))

;; initialize component, hook form's onsubmit
(defmethod/remote initialize ((self web-form-component) obj)
  (if (= null (this.form))
      (this.toast "Feedback div not found, aborting feedback component.")
      (let ((form (this.form)))
	(setf form.onsubmit
	      (lambda ()
		(let ((orig (.clone-node (obj.form) "deep")))
		  (.for-each (.filter (dojo.query "[type=radio]" (obj.get-form-id))
				      (lambda (e)
					(return e.checked))) 
			     (lambda (e)
			       (e.parent-node.replace-child (document.create-text-node "X") e)))
		  (obj.setformvals)
		  (obj.sendform form.inner-h-t-m-l)
		  (.parent-node.replace-child (obj.form) orig (obj.form))
		  (obj.toast "Form successfuly sent. Thank you.")
		  (return false)))))))

;; (defurl *test* "forms.can" ()
;;   (javascript/suspend
;;    (lambda ()
;;      (dojo "forms.can")
;;      (mapcar #'send/component (list (make-instance 'web-form-component)))
;;      (<:js
;;        `(progn
;;      	  (setf form (new (web-form-component)))
;;      	  (dojo.add-on-load (lambda ()
;;      			      (form.initialize form))))))))

