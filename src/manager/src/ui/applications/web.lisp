(in-package :manager)

;; -------------------------------------------------------------------------
;; Web Application CRUD
;; -------------------------------------------------------------------------
(defcomponent web-application-crud-mixin ()
  ((tab :host remote :initform (<core:tab))))

(defwebcrud <manager:web-application/crud (web-application-crud-mixin)
  ((fqdn :label "Domain Name")
   (application-class :label "Application Class" :read-only t) 
   (is-running :label "Is running?" :remote-type checkbox :read-only t)
   (is-registered :label "Is registered?" :remote-type checkbox :read-only t))
  (:default-initargs :title "Application" :editable-p nil :deletable-p nil))

(defmethod/remote register-button ((self <manager:web-application/crud))
  (with-slots (instance) self
    (with-slots (is-registered) instance
      (<:input :type "button"
	       :value (if is-registered (_"Unregister") (_"Register"))
	       :title (if is-registered
			  (_"Unregister this application from server")
			  (_"Register this application to the server"))
	       :onclick (lifte
			 (answer-component self
					   (list "register/unregister"
						 instance)))))))

(defmethod/remote start-button ((self <manager:web-application/crud))
  (with-slots (instance) self
    (with-slots (is-running) instance
      (<:input :type "button"
	       :value (if is-running (_"Stop") (_"Start"))
	       :title (if is-running
			  (_"Stop this application")
			  (_"Start this application"))
	       :onclick (lifte
			 (answer-component self
					   (list "start/stop" instance)))))))

(defmethod/remote view-buttons ((self <manager:web-application/crud))
  (append (call-next-method self)
	  (list (start-button self) (register-button self))))

(defmethod/remote destroy ((self <manager:web-application/crud))
  (if (typep (slot-value (tab self) 'destroy) 'function)
      (destroy (tab self)))
  (call-next-method self))

(defmethod/remote init ((self <manager:web-application/crud))
  (call-next-method self)
  (with-slots (instance) self
    (with-slots (tabs) instance
      (when tabs
	(let ((tabs (mapcar-cc (lambda (tab)
				 (destructuring-bind (name c) tab
				   (cons name (make-component c))))
			       tabs)))
	  (append self (setf (tab self)
			     (make-component (tab self) :tabs tabs
					     :tab-title "Capabilities"))))))))

;; -------------------------------------------------------------------------
;; Web Application View
;; -------------------------------------------------------------------------
(defcomponent web-application/view (remote-reference)
  ((%application :host lift :type web-application :reader %application
		 :initarg :application)
   (fqdn :lift t :host remote)
   (application-class :host remote)
   (is-running :host remote)
   (is-registered :host remote)
   (tabs :host remote))
  (:ctor %make-web-application/view))

(defun make-application-tab (application superclass)
  (case superclass
    (http-application
     (list "HTTP Application"
	   (make-http-application/view :application application)))
    (database-server
     (list "Database Server"
	   (<manager:database-server/crud
	    :instance (<manager:database-server :server application))))))

(defun make-web-application/view (&key application)
  (let ((class (symbol->js (class-name (class-of application))))
	(registered-p (if (application.server application)
			  t))
	(supers (mapcar #'class-name
			(class+.superclasses (class-of application)))))
    (%make-web-application/view
     :application application
     :application-class class
     :is-running (status application)
     :is-registered registered-p
     :tabs (reduce0 (lambda (acc super)
		      (aif (make-application-tab application super)
			   (cons it acc)
			   acc))
		    supers))))
