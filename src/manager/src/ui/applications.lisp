(in-package :manager)

;; -------------------------------------------------------------------------
;; Application Table
;; -------------------------------------------------------------------------
(deftable <manager:applications-table ()
  ((fqdn :label "FQDN")
   (application-class :label "Application Class")))

;; -------------------------------------------------------------------------
;; Web Application CRUD
;; -------------------------------------------------------------------------
(defwebcrud <manager:web-application-crud ()
  ((fqdn :label "Domain Name")
   (application-class :label "Application Class" :read-only t) 
   (is-running :label "Is running?" :remote-type checkbox :read-only t)
   (is-registered :label "Is registered?" :remote-type checkbox :read-only t))
  (:default-initargs :title "Application"
    :editable-p t :deletable-p t))

(defmethod/remote register-button ((self <manager:web-application-crud))
  (with-slots (instance) self
    (with-slots (is-registered) instance
      (<:input :type "button" :value (if is-registered
					 (_"Unregister")
					 (_"Register"))
	       :title (if is-registered
			  (_"Unregister this application from server")
			  (_"Register this application to the server"))
	       :onclick (lifte
			 (answer-component self
					   (list "register/unregister"
						 instance)))))))

(defmethod/remote start-button ((self <manager:web-application-crud))
  (with-slots (instance) self
    (with-slots (is-running) instance
      (<:input :type "button" :value (if is-running (_"Stop") (_"Start"))
	       :title (if is-running
			  (_"Stop this application")
			  (_"Start this application"))
	       :onclick (lifte
			 (answer-component self
					   (list "start/stop" instance)))))))

(defmethod/remote view-buttons ((self <manager:web-application-crud))
  (append (call-next-method self)
	  (list (start-button self) (register-button self))))

;; -------------------------------------------------------------------------
;; Manager Created Application Crud
;; -------------------------------------------------------------------------
(defparameter +superclasses+ '(http-application database-server))
(defwebcrud <manager:dynamic-application-crud (<manager:web-application-crud)
  ((fqdn :label "Domain Name")
   (application-class :label "Application Class" :read-only t)
   (application-superclasses :label "Superclasses"
			     :remote-type core-server::multiple-checkbox
			     :options (mapcar #'core-server::symbol-to-js
					      +superclasses+))
   (is-running :label "Is running?" :remote-type checkbox :read-only t)
   (is-registered :label "Is registered?" :remote-type checkbox :read-only t))
  (:default-initargs :title "Application"
    :editable-p t :deletable-p t))

;; -------------------------------------------------------------------------
;; Web Application View
;; -------------------------------------------------------------------------
(defcomponent web-application/view (remote-reference)
  ((%web-application :host lift :type web-application
		     :reader %%web-application :initarg :web-application)
   (fqdn :lift t :host remote)
   (application-class :host remote)
   (is-running :host remote)
   (is-registered :host remote))
  (:ctor %make-web-application/view))

(defun make-web-application/view (&key web-application)
  (let ((class (symbol->js (class-name (class-of web-application))))
	(registered-p (if (application.server web-application)
			  t)))
    (%make-web-application/view :web-application web-application
				:application-class class
				:is-running (status web-application)
				:is-registered registered-p)))

;; -------------------------------------------------------------------------
;; Manager Created Application View
;; -------------------------------------------------------------------------
(defcomponent dynamic-application/view (web-application/view)
  ((%web-application :host lift :type dynamic-application
		     :reader %%web-application :initarg :web-application)
   (application-superclasses :host remote) 
   (type-name :host remote :initform "dynamic-application"))
  (:ctor %make-dynamic-application/view))

(defcrud/lift dynamic-application/view dynamic-application)
(defun make-dynamic-application/view (&key web-application)
  (let ((class (symbol->js (class-name (class-of web-application))))
	(registered-p (if (application.server web-application)
			  t))
	(supers (reduce0 (lambda (acc atom)
			   (if (member (class-name atom) +superclasses+)
			       (cons (symbol->js (class-name atom)) acc)
			       acc))
			 (reverse
			  (class+.superclasses (class-of web-application))))))
    (%make-dynamic-application/view
     :web-application web-application
     :application-class class
     :application-superclasses supers
     :is-running (status web-application)
     :is-registered registered-p)))

;; -------------------------------------------------------------------------
;; Applications Component
;; -------------------------------------------------------------------------
(defcomponent <manager:applications (<core:table-with-crud
				     <widget:simple-widget)
  ()
  (:default-initargs :table-title "Applications"
    :table (<manager:applications-table)
    :crud (list (<manager:web-application-crud)
		(<manager:dynamic-application-crud))
    :input-element (<core:fqdn-input)))

(defmethod/local start-stop-app ((self <manager:applications)
				 (app web-application/view))
  (break (list 'start app)))

(defmethod/local register-unregister-app ((self <manager:applications)
					  (app web-application/view))
  (break (list 'register app)))

(defmethod/remote handle-crud ((self <manager:applications)
			       instance action args)
  (if (eq action "start/stop")
      (start-stop-app self instance)
      (if (eq action "register/unregister")
	  (register-unregister-app self instance)
	  (call-next-method self instance action args))))

(defmethod/cc _make-view ((self <manager:applications) (app web-application)
			  (class t))
  (make-web-application/view :web-application app))

(defmethod/cc _make-view ((self <manager:applications) (app web-application)
			  (class core-server::persistent-http-application+))
  (make-dynamic-application/view :web-application app))

(defmethod/cc make-view ((self <manager:applications) (app web-application))
  (_make-view self app (class-of app)))

(defmethod/remote core-server::_make-crud-component ((self <manager:applications)
						     instance)
  (if (slot-value instance 'type-name)
      (make-component (car (cdr (core-server::crud self))) :instance instance)
      (make-component (car (core-server::crud self)) :instance instance)))

(defmethod/local get-instances ((self <manager:applications))
  (mapcar (lambda (app) (make-view self app))
	  (union (database.get (application.server application) :applications)
		 (server.applications (application.server application))
		 :key #'web-application.fqdn :test #'equal)))

(defmethod/local add-instance ((self <manager:applications) fqdn)
  (let ((new-application (make-instance 'dynamic-application
					:fqdn fqdn
					:admin-email "root@localhost")))
    (register (application.server application) new-application)
    (make-view self new-application)))

(defmethod/local delete-instance ((self <manager:applications)
				  (instance web-application/view))
  (prog1 t (unregister (application.server application)
		       (slot-value instance '%web-application))))

(defmethod/local update-instance ((self <manager:applications)
				  (instance web-application/view) args)
  (let* ((attributes (core-server::plist-to-alist (jobject.attributes args)))
	 (attributes (remove 'application-superclasses attributes :key #'car))
	 (object (apply #'dynamic-application.update
			(application.server application)
			(cons instance (core-server::alist-to-plist attributes)))))
    (make-view self object)))


