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
  (:default-initargs :database-directory *db-location*
    :db-auto-start t
    :model-class 'manager-model
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
    :depends-on '(:arnesi :core-server) :urls nil))

(defvar *app* (make-instance 'manager-application))

(defun register-me (&optional (server *server*)) (register server *app*))
(defun unregister-me (&optional (server *server*)) (unregister server *app*))

