(in-package :manager)

;; -------------------------------------------------------------------------
;; Application Table
;; -------------------------------------------------------------------------
(deftable <manager:applications-table ()
  ((fqdn :label "FQDN")
   (application-class :label "Application Class")))

;; -------------------------------------------------------------------------
;; Application CRUD
;; -------------------------------------------------------------------------
(defwebcrud <manager:application-crud ()
  ((fqdn :label "Domain Name")
   (application-class :label "Application Class" :read-only t)
   (is-running :label "Is running?" :remote-type checkbox :read-only t)
   (is-registered :label "Is registered?" :remote-type checkbox :read-only t))
  (:default-initargs :title "Application"
    :editable-p t :deletable-p t))

(defmethod/remote register-button ((self <manager:application-crud))
  (with-slots (instance) self
    (with-slots (is-registered) instance
      (<:input :type "button" :value (if is-registered (_"Unregister") (_"Register"))
	       :title (if is-registered
			  (_"Unregister this application from server")
			  (_"Register this application to the server"))
	       :onclick (lifte (answer-component self
						 (list "register/unregister"
						       instance)))))))

(defmethod/remote start-button ((self <manager:application-crud))
  (with-slots (instance) self
    (with-slots (is-running) instance
      (<:input :type "button" :value (if is-running (_"Stop") (_"Start"))
	       :title (if is-running
			  (_"Stop this application")
			  (_"Start this application"))
	       :onclick (lifte (answer-component self
						 (list "start/stop" instance)))))))

(defmethod/remote view-buttons ((self <manager:application-crud))
  (append (call-next-method self) (list (start-button self) (register-button self))))

(defcomponent http-application/view (remote-reference)
  ((http-application :host lift :type http-application :reader %%http-application)
   (fqdn :lift t :host remote)
   (application-class :host remote)
   (is-running :host remote)
   (is-registered :host remote))
  (:ctor %make-http-application/view))

(defun make-http-application/view (&key http-application)
  (let ((class (core-server::symbol-to-js (class-name (class-of http-application))))
	(registered-p (if (application.server http-application)
			  t)))
    (%make-http-application/view :http-application http-application
				 :application-class class
				 :is-running (status http-application)
				 :is-registered registered-p)))

;; -------------------------------------------------------------------------
;; Applications Component
;; -------------------------------------------------------------------------
(defcomponent <manager:applications (<core:table-with-crud <widget:simple-widget)
  ()
  (:default-initargs :table-title "Applications"
    :table (<manager:applications-table)
    :crud (<manager:application-crud)
    :input-element (<core:fqdn-input)))

(defmethod/local start-stop-app ((self <manager:applications)
				 (app http-application/view))
  (break (list 'start app)))

(defmethod/local register-unregister-app ((self <manager:applications)
					  (app http-application/view))
  (break (list 'register app)))

(defmethod/remote handle-crud ((self <manager:applications) instance action args)
  (cond
    ((eq action "start/stop") (start-stop-app self instance))
    ((eq action "register/unregister") (register-unregister-app self instance))
    (t (call-next-method self instance action args))))

(defmethod/local get-instances ((self <manager:applications))
  (mapcar (lambda (app) (make-http-application/view :http-application app))
	  (server.applications (application.server application))))
