(in-package :Core-server)

(defpackage :core-server.examples.auth
  (:nicknames :auth)
  (:use :cl :core-server :arnesi :manager))

(in-package :auth)

;; -------------------------------------------------------------------------
;; Authentication Example
;; -------------------------------------------------------------------------
(defapplication auth-example (manager-web-application-mixin http-application)
  ()
  (:default-initargs
   :fqdn "authentication"
   :admin-email "evrim@core.gen.tr"
   :htdocs-pathname (merge-pathnames
   		     (make-pathname :directory
   				    '(:relative "src" "manager" "wwwroot"))
   		     (bootstrap:home))
   :oauth-key "root"
   :oauth-secret "core-server"
   :default-handler 'index.html))

(defcomponent widget1 (secure-object <widget:simple)
  ()
  (:default-initargs
   :levels '(widget1/anonymous widget1/registered)
   :permissions '((OWNER . 1) (GROUP . 1) (OTHER . 1) (ANONYMOUS . 0) (UNAUTHORIZED . -1))
   :owner (make-simple-user :name "Admin")
   :group (make-simple-group :name "users")))

(defcomponent widget1/anonymous (secure-object/authorized widget1)
  ((secure-object :host lift :type widget1)))

(defmethod/remote init ((self widget1/anonymous))
  (call-next-method self)
  (append self (<:p "Widget1: Anonymous user.")))

(defcomponent widget1/registered (secure-object/authorized widget1)
  ((secure-object :host lift :type widget1)
   (username :host remote)))

(defmethod shared-initialize :after ((self widget1/registered) slots &rest args)
  (declare (ignore args))
  (setf (username self) (user.name (secure.user self))))

(defmethod/remote init ((self widget1/registered))
  (call-next-method self)
  (append self (<:p "Widget1: Registered user"))
  (append self (<:p "Welcome, " (username self) ".")))

(defmethod/cc make-controller ((self auth-example))
  (<core:simple-controller :default-page "index"
			   :plugins (list (<manager:authentication-plugin))
   (<core:simple-page :name "index"
    (<core:simple-widget-map :selector "login"
			     :widget (<manager:login-link
				      :auth-uri (web-application.oauth-uri self)
				      :return-uri (web-application.oauth-handler-uri self)))
    (<core:simple-widget-map :selector "content" :widget (widget1)))))

(defhandler "index.core" ((self auth-example))
  (labels ((handle-action (component action &rest args)
	     (cond
	       ((eq :login action)
	     	(let ((user (car args)))
	     	  (with-slots (id name) user
	     	    (let* ((user (setf (query-session :user)
	     			       (make-simple-user
	     				:name name
	     				:group (make-simple-group :name "users"))))
	     		   (controller (authorize self user (make-controller self))))
	     	      (apply #'handle-action (continue/js controller))))))
	       ((eq :logout action)
	     	(setf (query-session :user) nil)
	     	(apply #'handle-action
	     	       (continue/js
	     		(authorize self (make-anonymous-user) (make-controller self)))))
	       (t (error "Unhandler action (~A ~A ~{~A~})" action component args)))))
    (apply #'handle-action
	   (javascript/suspend
	    (lambda (stream)
	      (let ((controller1 (authorize self (or (query-session :user)
				 		  (make-anonymous-user))
					    (make-controller self))))
		(rebinding-js/cc (controller1) stream
		  (call/cc controller1 nil
			   (lambda (c) (setf (slot-value window 'controller) c))))))))))

(defhandler "index.html" ((self auth-example))
  (<:html
   (<:head
    (<:title "Core Server - http://labs.core.gen.tr/")
    (<:meta :http--equiv "Content-Type" :content "text/html; charset=utf-8;")
    (<:script :type "text/javascript" :src "library.core")
    (<:script :type "text/javascript" :src "index.core"))
   (<:body
    (<:h1 "[Core Server]")
    (<:h2 "Server Authentication Example")
    (<:a :id "login" :href "" "Click to Login")
    (<:div :id "content"))))

(defparameter *app* (make-instance 'auth-example))
(register *server* *app*)
