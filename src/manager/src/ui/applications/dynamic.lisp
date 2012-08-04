(in-package :manager)

;; -------------------------------------------------------------------------
;; Manager Created Application Crud
;; -------------------------------------------------------------------------
(defwebcrud <manager::%dynamic-application-crud (<manager:web-application/crud)
  ((fqdn :label "Domain Name")
   (application-class :label "Application Class" :read-only t)
   (application-superclasses :label "Superclasses"
			     :remote-type core-server::multiple-checkbox
			     :options (mapcar #'core-server::symbol-to-js
					      +superclasses+))
   (is-running :label "Is running?" :remote-type checkbox :read-only t)
   (is-registered :label "Is registered?" :remote-type checkbox :read-only t))
  (:default-initargs :title "Application" :editable-p t :deletable-p t))

(defcomponent <manager:dynamic-application-crud (singleton-component-mixin
						 <manager::%dynamic-application-crud)
  ())


;; -------------------------------------------------------------------------
;; Manager Created Application View
;; -------------------------------------------------------------------------
(defcomponent dynamic-application/view (web-application/view)
  ((%application :host lift :type dynamic-application :reader %application
		 :initarg :application)
   (application-superclasses :host remote)
   (type-name :host remote :initform "dynamic-application"))
  (:ctor %make-dynamic-application/view))

(defcrud/lift dynamic-application/view dynamic-application)
(defmethod/lift dynamic-application.change-class ((self persistent-server)
						  (instance dynamic-application/view)
						  new-superclasses))
(defmethod/lift dynamic-application.superclasses ((self dynamic-application/view)))

(defun make-dynamic-application/view (&key application)
  (let ((class (symbol->js (class-name (class-of application))))
	(registered-p (if (application.server application)
			  t))
	(supers (mapcar #'class-name
			(class+.superclasses (class-of application)))))
    (%make-dynamic-application/view
     :application application
     :application-class class
     :application-superclasses (dynamic-application.superclasses application)
     :is-running (status application)
     :is-registered registered-p
     :tabs (reduce0 (lambda (acc super)
		      (aif (make-application-tab application super)
			   (cons it acc)
			   acc))
		    supers))))