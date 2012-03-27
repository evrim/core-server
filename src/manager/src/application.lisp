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

(defun register-me (&optional (server *server*)) (register server *app*))
(defun unregister-me (&optional (server *server*)) (unregister server *app*))

(defparameter +admin+ (list "admin" "admin"))

;; -------------------------------------------------------------------------
;; Index Loop
;; -------------------------------------------------------------------------
(defhandler "index\.core" ((self manager-application))
  (destructuring-bind (name password)
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
     (if (and (equal (car +admin+) name) (equal (cadr +admin+) password))
	 (prog1 (lambda (self k) (k (setf window.location "manager.html")))
	   (update-session :user +admin+))
	 nil))))

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

