(in-package :manager)

(defvar *wwwroot* (make-project-path "manager" "wwwroot"))

(defvar *db-location*
  (merge-pathnames
   (make-pathname :directory '(:relative "var" "localhost" "db"))
   (tr.gen.core.server.bootstrap:home)))

(defapplication manager-application (root-http-application-mixin
				     http-application database-server
				     logger-server
				     serializable-web-application)
  ()
  (:default-initargs
      :database-directory *db-location*
    :db-auto-start t
    :fqdn "localhost"
    :admin-email "root@localhost"
    :project-name "manager"
    :project-pathname #p"/home/aycan/core-server/projects/manager/"
    :htdocs-pathname *wwwroot*
    :sources '(src/packages src/model src/tx src/interfaces src/application
	       src/security src/ui/main)
    :directories '(#p"src/" #p"src/ui/" #p"t/" #p"doc/" #p"wwwroot/"
		   #p"wwwroot/style/" #p"wwwroot/images/" #p"templates/"
		   #p"db/")
    :use '(:common-lisp :core-server :cl-prevalence :arnesi)
    :depends-on '(:arnesi :core-server)))

(defvar *app* (make-instance 'manager-application))

(deftransaction init-database ((self manager-application))
  (assert (null (database.get self 'initialized)))
  (setf (database.get self 'api-secret) (random-string))
  (manager-user.add self :name "Root User" :username "root" :password "core-server")
  (setf (database.get self 'initialized) t)
  t)

(defmethod start ((self manager-application))
  (if (null (database.get self 'initialized))
      (prog1 t (init-database self))
      nil))

(defun register-me (&optional (server *server*))
  (if (null (status *app*)) (start *app*))
  (register server *app*))

(defun unregister-me (&optional (server *server*))
  (unregister server *app*))

;; -------------------------------------------------------------------------
;; Index Loop
;; -------------------------------------------------------------------------
(defhandler "index\.core" ((self manager-application))
  (destructuring-bind (username password)
      (javascript/suspend
       (lambda (stream)
	 (let ((box (core-server::login-box))
	       (clock (<core:simple-clock)))
	   (with-js (box clock) stream
	     (let ((ctor box)
		   (cl clock))
	       (add-on-load
		(lambda ()
		  (ctor (document.get-element-by-id "login")
			(lambda (result)
			  (cl (document.get-element-by-id "clock" window.k)))))))))))
    (continue/js
     (let ((admin (manager-user.find self :username username)))
       (cond
	 ((and admin (equal (manager-user.password admin) password))
	  (prog1 (lambda (self k) (k (setf window.location "manager.html")))
	    (update-session :user admin)))
	 (t nil))))))

;; -------------------------------------------------------------------------
;; Main Manager Loop
;; -------------------------------------------------------------------------
(defhandler "manager\.core" ((self manager-application))
  (javascript/suspend
   (lambda (stream)
     (aif (query-session :user)
	  (let ((manager (or (query-session :manager)
			     (update-session :manager (make-controller self it)))))
	    (with-js (manager) stream
	      (let ((ctor manager))
		(add-on-load (lambda () (ctor null window.k))))))
	  (with-js () stream
	    (setf window.location "index.html"))))))


;; -------------------------------------------------------------------------
;; Interface
;; -------------------------------------------------------------------------
(deftransaction make-api-key ((self manager-application) fqdn)
  (let ((secret (ironclad::ascii-string-to-byte-array (database.get self 'api-secret))))
    (core-server::hmac secret (format nil "~A-api-key" fqdn))))

(deftransaction make-api-password ((self manager-application) fqdn)
  (let ((secret (ironclad::ascii-string-to-byte-array (database.get self 'api-secret))))
    (core-server::hmac secret (format nil "~A-api-password" fqdn))))

(deftransaction site.add ((self manager-application) &key
			  (fqdn (error "Provide :fqdn"))
			  (api-key (make-api-key self fqdn))
			  (api-password (make-api-password self fqdn))
			  (owner (manager-user.find self :username "root"))
			  (timestamp (get-universal-time)))
  (assert (not (null owner)))
  (call-next-method self :fqdn fqdn :api-key api-key
		    :api-password api-password :owner owner
		    :timestamp timestamp))


(defhandler "auth\.core" ((self manager-application) (reply-to "reply-to")
			(action "action") (mode "mode"))
  (<:html
   (<:head
    (<:title "Core Server - http://labs.core.gen.tr/")
    (<:meta :http--equiv "Content-Type" :content "text/html; charset=utf-8")
    (<:link :rel "stylesheet" :href "/style/reset.css")
    (<:link :rel "stylesheet" :href "/style/common.css")
    (<:style :type "text/css"
	     (css "body"
		  :background "url('/style/dialog/stripe.png')"))
    (<:script :type "text/javascript" :src "library.core"))
   (<:body :class "stripe-bg"
	   (<:div :class "max-width center text-center"
		  (core-server::login-box)
		  "foo"))))
