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
  (:default-initargs :fqdn "authentication"
    :admin-email "evrim@core.gen.tr"
    :htdocs-pathname (merge-pathnames #P"examples/auth/" (bootstrap:home))))

(defparameter *app* (make-instance 'auth-example))
(register *server* *app*)

(defcomponent widget1 (secure-object <widget:simple)
  ()
  (:default-initargs :levels '(widget1/anonymous widget1/registered)
    :owner (make-simple-user :name "Admin")
    :group (make-simple-group :name "users")))

(defcomponent widget1/anonymous (secure-object/authorized widget1)
  ((secure-object :host lift :type widget1)))

(defmethod/remote init ((self widget1/anonymous))
  (call-next-method self)
  (append self (<:p "Widget1: Anonymous user.")))

(defcomponent widget1/registered (secure-object/authorized widget1)
  ((secure-object :host lift :type widget1)))

(defmethod/remote init ((self widget1/registered))
  (call-next-method self)
  (append self (<:p "Widget1: Registered user")))

(defmethod/cc make-controller ((self auth-example))
  (<core:simple-controller :default-page "index"
   (<core:simple-page :name "index"
    (<core:simple-widget-map :selector "login"
			     :widget (<manager:login-link
				      :auth-uri (web-application.oauth-uri self)
				      :return-uri (web-application.oauth-handler-uri self)))
    (<core:simple-widget-map :selector "content" :widget (widget1)))))

(defhandler "index\.core" ((self auth-example))
  (javascript/suspend
   (lambda (stream)
     (let ((controller (authorize self (make-anonymous-user)
				  (make-controller self))))
       (rebinding-js/cc (controller) stream
	 (call/cc controller nil
		  (lambda (c)
		    (setf (slot-value window 'controller) c))))))))
