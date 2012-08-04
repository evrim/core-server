(in-package :manager)

;; -------------------------------------------------------------------------
;; Application Table
;; -------------------------------------------------------------------------
(deftable <manager:application/table ()
  ((fqdn :label "FQDN")
   (application-class :label "Application Class")))

;; -------------------------------------------------------------------------
;; Applications Component
;; -------------------------------------------------------------------------
(defcomponent <manager:applications (<core:table-with-crud <widget:simple)
  ()
  (:default-initargs
    :table-title "Applications"
    :table (<manager:application/table)
    :crud (list (<manager:web-application/crud) (<manager:dynamic-application-crud))
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

(defmethod/cc make-view ((self <manager:applications) (app dynamic-application))
  (make-dynamic-application/view :application app))

(defmethod/cc make-view ((self <manager:applications) (app web-application))
  (make-web-application/view :application app))

(defmethod/remote core-server::_make-crud-component ((self <manager:applications)
						     instance)
  (make-component (if (slot-value instance 'type-name)
		      (car (cdr (core-server::crud self)))
		      (car (core-server::crud self)))
		  :instance instance))

(defmethod/local get-instances ((self <manager:applications))
  (mapcar (lambda (app) (make-view self app))
	  (server.applications (application.server application))))

(defmethod/local add-instance ((self <manager:applications) fqdn)
  (aif (find-application (application.server application) fqdn)
       (make-web-error 'error "FQDN %1 already exists." fqdn)
       (let ((new-application (make-instance 'dynamic-application
					     :fqdn fqdn
					     :admin-email "root@localhost")))
	 (register (application.server application) new-application)
	 (make-view self new-application))))

(defmethod/local delete-instance ((self <manager:applications)
				  (instance web-application/view))
  (prog1 t (unregister (application.server application)
		       (slot-value instance '%web-application))))

(defmethod/local update-instance ((self <manager:applications)
				  (instance web-application/view) args)
  (jobject))

(defmethod/local update-instance ((self <manager:applications)
				  (instance dynamic-application/view) args)
  (let* ((attributes (core-server::plist-to-alist (jobject.attributes args)))
	 (supers (cdr (find 'application-superclasses attributes :key #'car
			    :test #'string=)))
	 (attributes (core-server::alist-to-plist
		      (remove 'application-superclasses attributes :key #'car
			      :test #'string=))))
    (dynamic-application.change-class (application.server application)
				      instance supers)
    (make-view self (apply #'dynamic-application.update
			   (application.server application)
			   (cons instance attributes)))))

;; (defmethod/local get-tabs ((self <manager:applications)
;; 			   (instance web-application/view))
;;   nil)

;; (defmethod/local get-tabs ((self <manager:applications)
;; 			   (instance dynamic-application/view))
;;   (let ((app (%web-application instance)))
;;     (reduce0 (lambda (acc tab)
;; 	       (cond
;; 		 ((equal tab "httpApplication")
;; 		  (cons (cons tab
;; 			      (make-http-application/view
;; 			       :http-application (%%web-application instance)))
;; 			acc))
;; 		 ((equal tab "databaseServer")
;; 		  (cons (cons tab
;; 			      (make-database-server/view
;; 			       :database-server (%%web-application instance)))
;; 			acc))
;; 		 (t acc)))
;; 	     (dynamic-application.superclasses instance))))

;; (defmethod/remote core-server::_make-crud-component ((self <manager:applications)
;; 						     instance)
;;   (let* ((_crud (if (slot-value instance 'type-name)
;; 		    (car (cdr (core-server::crud self)))
;; 		    (car (core-server::crud self))))
;; 	 (_crud (make-component _crud :instance instance))
;; 	 (_tabs (get-tabs self instance)))
;;     (when _tabs
;;       (let ((_tab (make-component (_tab self)
;; 				  :tabs (mapcar-cc
;; 					 (lambda (tab)
;; 					   (destructuring-bind (name c) tab
;; 					     (cons name (make-component c))))
;; 					 _tabs)
;; 				  :tab-title "Configuration")))
;; 	(add-class _tab "pad5")
;; 	(append _crud _tab)))
;;     _crud))
