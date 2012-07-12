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
(defwebcrud <manager:manager-created-application-crud (<manager:web-application-crud)
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
  ((http-application :host lift :type http-application
		     :reader %%http-application)
   (fqdn :lift t :host remote)
   (application-class :host remote)
   (is-running :host remote)
   (is-registered :host remote))
  (:ctor %make-web-application/view))

(defun make-web-application/view (&key http-application)
  (let ((class (core-server::symbol-to-js
		(class-name (class-of http-application))))
	(registered-p (if (application.server http-application)
			  t)))
    (%make-web-application/view :http-application http-application
				:application-class class
				:is-running (status http-application)
				:is-registered registered-p)))

;; -------------------------------------------------------------------------
;; Manager Created Application View
;; -------------------------------------------------------------------------
(defcomponent manager-created-application/view (web-application/view)
  ((application-superclasses :host remote))
  (:ctor %make-manager-created-application/view))

(defun make-manager-created-application/view (&key http-application)
  (let ((class (core-server::symbol-to-js
		(class-name (class-of http-application))))
	(registered-p (if (application.server http-application)
			  t))
	(supers (reduce0 (lambda (acc atom)
			   (if (member (class-name atom) +superclasses+)
			       (cons (core-server::symbol-to-js
				      (class-name atom)) acc)
			       acc))
			 (reverse
			  (class+.superclasses (class-of http-application))))))
    (%make-manager-created-application/view
     :http-application http-application
     :application-class class
     :application-superclasses supers
     :is-running (status http-application)
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
		(<manager:manager-created-application-crud))
    :input-element (<core:fqdn-input)))

(defmethod/local start-stop-app ((self <manager:applications)
				 (app web-application/view))
  (break (list 'start app)))

(defmethod/local register-unregister-app ((self <manager:applications)
					  (app web-application/view))
  (break (list 'register app)))

(defmethod/remote handle-crud ((self <manager:applications)
			       instance action args)
  (cond
    ((eq action "start/stop") (start-stop-app self instance))
    ((eq action "register/unregister") (register-unregister-app self instance))
    (t (call-next-method self instance action args))))

(defmethod/cc _make-view ((self <manager:applications) (app web-application)
			  (class t))
  (make-web-application/view :http-application app))

(defmethod/cc _make-view ((self <manager:applications) (app web-application)
			  (class core-server::persistent-http-application+))
  (make-manager-created-application/view :http-application app))

(defmethod/cc make-view ((self <manager:applications) (app web-application))
  (_make-view self app (class-of app)))

(defmethod/remote core-server::_make-crud-component ((self <manager:applications)
						     instance)
  (if (slot-value instance 'application-superclasses)
      (progn
	(_debug (list "foo" (core-server::_crud self)))
	(make-component (car (cdr (core-server::crud self)))
			:instance instance))
      (progn
	(_debug (list "moo" (core-server::_crud self)))
	(make-component (car (core-server::crud self)) :instance instance))))

(defmethod/local get-instances ((self <manager:applications))
  (mapcar (lambda (app) (make-view self app))
	  (server.applications (application.server application))))

(defmethod/local add-instance ((self <manager:applications) fqdn)
  (let ((application (make-instance 'manager-created-application
				    :fqdn fqdn
				    :admin-email "root@localhost")))
    (register (application.server application) application)
    (make-view self application)))

