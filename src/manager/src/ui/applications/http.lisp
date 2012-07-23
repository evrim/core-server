(in-package :manager)

(deftable http-application-handlers-table ()
  ((url :label "URL Regular Expression")
   (method :label "CLOS Method")))

(deftable http-application-security-handlers-table ()
  ((url :label "URL Regular Expression")
   (method :label "CLOS Method")
   (authentication-type :label "Auth Type")))

;; -------------------------------------------------------------------------
;; HTTP Application / View
;; -------------------------------------------------------------------------
(defcomponent http-application/view (<core:tab)
  ((%http-application :host lift :type http-application
		      :reader %%http-application :initarg :http-application)
   (handlers :host remote)
   (security-handlers :host remote)
   (_table :host remote :initform (http-application-handlers-table))
   (_security-table :host remote
		    :initform (http-application-security-handlers-table)))
  (:ctor %make-http-application/view)
  (:default-initargs :nav-alignment "right" :tab-title "HTTP Application"))

(defun make-http-application/view (&key application)
  (flet ((to-symbols1 (handlers)
	   (mapcar (lambda (a)
		     (jobject
		      :method (format nil "~A:~A"
				      (package-name
				       (symbol-package (car a)))
				      (symbol-name (car a)))
		      :url (cadr a)))
		   handlers))
	 (to-symbols2 (handlers)
	   (mapcar (lambda (a)
		     (jobject
		      :method (format nil "~A:~A"
				      (package-name
				       (symbol-package (car a)))
				      (symbol-name (car a)))
		      :url (cadr a)
		      :authentication-type (symbol->js (car (reverse a)))))
		   handlers)))
    (let ((handlers (http-application+.handlers (class-of application)))
	  (s-handlers (core-server::http-application+.security-handlers
		       (class-of application))))
      (%make-http-application/view :http-application application 
				   :security-handlers (to-symbols2 s-handlers)
				   :handlers (to-symbols1 handlers)))))

(defmethod/remote init ((self http-application/view))
  (setf (core-server::tabs self)
	(list (cons "handlers"
		    (make-component (_table self) :instances (handlers self)))
	      (cons "securityHandlers"
		    (make-component (_security-table self)
				    :instances (security-handlers self)))))
  (call-next-method self))