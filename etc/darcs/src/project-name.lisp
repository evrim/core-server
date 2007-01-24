(in-package :project-name)

(defparameter *db-location* #P"project-path/db/")
(defparameter *wwwroot* #P"project-path/wwwroot/")

(defclass project-name-application (ucw-web-application
				    database-server
				    cookie-session-application-module
				    ajax-application-module)
  ()
  (:default-initargs
   :tal-generator (make-instance 'yaclml:file-system-generator
				 :cachep t
				 :root-directories (list *wwwroot* *ucw-tal-root*))
    :www-roots (list *wwwroot* *ucw-tal-root*)
    :directory *db-location*
    :db-auto-start t
    :model-class 'project-name-model))

(defvar *app* (make-instance 'project-name-application
			     :fqdn project-fqdn
			     :admin-email project-admin-email
			     :url-prefix "/project-name/"
			     :debug-on-error t))

(defun register-me ()
  (core-server::register core-server::*server* *app*))

(defun unregister-me (server)
  (core-server::unregister core-server::*server* *app*))
