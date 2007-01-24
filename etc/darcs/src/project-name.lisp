(in-package :project-name)

(defparameter *db-location* #P"project-path/db/")
(defparameter *wwwroot* #P"project-path/wwwroot/")

(defclass project-name-application (ucw-web-application
				    cookie-session-application-module
				    ajax-application-module)
  ()
  (:default-initargs 
   :tal-generator (make-instance 'yaclml:file-system-generator
				 :cachep t
				 :root-directories (list *wwwroot* *ucw-tal-root*))
    :www-roots (list *wwwroot* *ucw-tal-root*)))

(defvar *app* (make-instance 'project-name-application
			     :fqdn project-fqdn
			     :admin-email project-admin-email
			     :url-prefix "/project-name/"
			     :debug-on-error t))

(defvar *db*
  (make-instance 'database-server
		 :db-auto-start t
		 :directory *db-location*
		 :model-class 'project-name-model))

(defun register-me (server)
  (register-application server *app*))

(defun unregister-me (server)
  (unregister-application server *app*))
