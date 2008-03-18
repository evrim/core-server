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
  (let ((f (format nil "<div id=\"form\">~A</div>" (cl-ppcre::regex-replace-all "\\\\n" form ""))))
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
	       (case i.tag-name
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
		  (this.toast "Form successfuly sent. Thank you.")
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

