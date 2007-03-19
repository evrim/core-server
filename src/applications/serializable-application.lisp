(in-package :core-server)

(defgeneric serialize-source (app symbol)
  (:documentation "Serialize a new source for application")
  (:method ((self null) symbol) t))
(defgeneric serialize-asd (app)
  (:documentation "Serialize system definition"))
(defgeneric source-to-pathname (app symbol)
  (:documentation "todo"))
(defgeneric serialize (app)
  (:documentation "Serialize application"))

(defgeneric package-keyword (app &optional long)
  (:documentation "Package name for our new application"))

(defgeneric src/packages (app)
  (:documentation "Serialize a source directory"))
(defgeneric src/model (app)
  (:documentation "Serialize a source directory"))
(defgeneric src/tx (app)
  (:documentation "Serialize a source directory"))
(defgeneric src/interfaces (app)
  (:documentation "Serialize a source directory"))
(defgeneric src/application (app)
  (:documentation "Serialize a source directory"))
(defgeneric src/security (app)
  (:documentation "Serialize a source directory"))
(defgeneric src/ui/main (app)
  (:documentation "Serialize a source directory"))

(defmethod package-keyword ((self serializable-web-application) &optional long)
  (or (and long (make-keyword (strcat "tr.gen.core." (web-application.project-name self))))
      (make-keyword (web-application.project-name self))))

(defmethod package-test-keyword ((self serializable-web-application))
  (make-keyword (format nil "~A.test" (package-keyword self))))

(defmethod model-class ((self serializable-web-application))  
  (intern (format nil "~A" (string-upcase (strcat (web-application.project-name self) "-model")))))

(defmethod application-class ((self serializable-web-application))
  (intern (format nil "~A" (string-upcase (strcat (web-application.project-name self) "-application")))))

(defmethod serialize-source ((self serializable-web-application) symbol)
  (with-package :core-server
    (with-open-file (out (source-to-pathname self symbol) :direction :output :if-does-not-exist :create :if-exists :supersede)
      (let ((*print-escape* nil)
	    (*print-pretty* t))
	(mapcar #'(lambda (line)
		    (format out "~A" (string-downcase (format nil "~S~%~%" line))))
		(cdr (apply symbol (list self))))))))

(defmethod serialize-asd ((self serializable-web-application))
  (with-open-file (out (merge-pathnames (make-pathname :name (web-application.project-name self) :type "asd")
					(web-application.project-pathname self))
		       :direction :output :if-does-not-exist :create :if-exists :supersede)
    (mapcar #'(lambda (line)
		(format out "~A" (string-downcase (format nil "~S~%~%" line))))
	    (cdr (apply 'asd (list self))))))

(defmethod source-to-pathname ((self serializable-web-application) symbol)
  (let* ((result (cl-ppcre:split "(/)" (string-downcase (string symbol))))
	 (directories (butlast result))
	 (file (last1 result)))
    (merge-pathnames (make-pathname :directory (cons :relative directories) :name file :type "lisp")
		     (web-application.project-pathname self))))

(defmethod serialize ((self serializable-web-application))
  ;; Create template folders
  (mapcar #'(lambda (dir)
	      (ensure-directories-exist (merge-pathnames dir (web-application.project-pathname self))))
	  (serializable-web-application.directories self))

  (serialize-asd self)

  (mapcar (curry #'serialize-source self) (serializable-web-application.sources self)))

(defmethod evaluate ((self serializable-web-application))
  (pushnew (web-application.project-pathname self)
	   asdf:*central-registry*)
  (asdf:oos 'asdf:load-op (package-keyword self)))

(defmethod asd ((self serializable-web-application))
  `(progn
     (in-package :asdf)
     
     (defsystem ,(package-keyword self)
       :description "Core Template Application"
       :version ".1"
       :author ,(web-application.admin-email self)
       :maintainer ,(web-application.admin-email self)
       :licence "LGPL v2"
       :components ((:static-file ,(strcat (web-application.project-name self) ".asd"))
		    (:module :src
			     :serial t
			     :components
			     ((:file "packages")
			      (:file "model" :depends-on ("packages"))
			      (:file "tx" :depends-on ("model"))
			      (:file "interfaces" :depends-on ("tx"))
			      (:file "application" :depends-on ("interfaces"))
			      (:module :ui
				       :serial t
				       :components
				       ((:file "main"))))))
       :depends-on ,(serializable-web-application.depends-on self)
       :serial t)
     
     (defsystem ,(package-test-keyword self)
       :components ((:module :t
			     :components
			     ((:file "packages"))))
       :depends-on (,(package-keyword self) :core-server :FiveAM))

     (defmethod perform ((op asdf:test-op) (system (eql (find-system ,(package-keyword self)))))
       (asdf:oos 'asdf:load-op ,(package-test-keyword self))
       (funcall (intern (string :run!) (string :it.bese.FiveAM)) ,(package-keyword self)))))

(defmethod src/packages ((self serializable-web-application))
  `(progn
     (in-package :cl-user)
     (defpackage ,(package-keyword self t)       
       (:nicknames ,(package-keyword self))
       (:use ,@(serializable-web-application.use self))
       (:import-from #:yaclml #:file-system-generator)
       (:shadowing-import-from #:cl-prevalence #:name))))

(defmethod src/model ((self serializable-web-application))  
  `(progn
     (in-package ,(package-keyword self))
     (defclass ,(model-class self) ()
       ())
     (defmethod print-object ((self ,(model-class self)) stream)
       (print-unreadable-object (self stream :type t :identity t)))))

(defmethod src/tx ((self serializable-web-application))
  `(progn
     (in-package ,(package-keyword self))))

(defmethod src/interfaces ((self serializable-web-application))
  `(progn
     (in-package ,(package-keyword self))))

(defmethod src/application ((self serializable-web-application))
  `(progn
     (in-package ,(package-keyword self))
     (defvar *wwwroot* (make-project-path ,(symbol-name (package-keyword self)) "wwwroot"))
     (defvar *talroot* (make-project-path ,(symbol-name (package-keyword self)) "templates"))
     (defvar *db-location* (make-project-path ,(symbol-name (package-keyword self)) "db"))
     (defclass ,(application-class self) (ucw-web-application
					  apache-web-application
					  database-server
					  cookie-session-application-module
					  ajax-application-module
					  serializable-web-application)
       ()
       (:default-initargs
	:tal-generator (make-instance 'yaclml::file-system-generator
				      :cachep t
				      :root-directories (list *wwwroot* *ucw-tal-root*))
	 :www-roots (list *wwwroot* *ucw-tal-root*)
	 :directory *db-location*
	 :db-auto-start t
	 :model-class ',(model-class self)
	 :fqdn ,(web-application.fqdn self)
	 :admin-email ,(web-application.admin-email self)
	 :project-name ,(web-application.project-name self)
	 :project-pathname ,(web-application.project-pathname self)
	 :sources ',(serializable-web-application.sources self)
	 :directories ',(serializable-web-application.directories self)
	 :use ',(serializable-web-application.use self)
	 :depends-on ',(serializable-web-application.depends-on self)
	 :dispatchers (cons (make-instance 'regexp-dispatcher :url-string "^index.*$"
					   :handler (lambda ()
						      (arnesi::with-call/cc
							(let ((self nil))
							  (with-request-params nil (context.request *context*)
							    (call 'main-window))))))
			    (ucw::standard-dispatchers))))
     (defvar *app* (make-instance ',(application-class self)))
     (defun register-me (server)
       (core-server::register server *app*))
     (defun unregister-me (server)
       (core-server::unregister server *app*))))

(defmethod src/security ((self serializable-web-application))
  `(progn
     (in-package ,(package-keyword self))))

(defmethod src/ui/main ((self serializable-web-application))
  `(progn
     (in-package ,(package-keyword self))
     (defun header ()
       (<:h1 "Header"))
     (defun footer ()
       (<:h1 "Footer"))
     (defcomponent main-window (ajax-window)
       ())
     (defmethod render ((self main-window))
       (<:div :id "header" (header))
       (<:div :id "body" (<:h1 "Hi, i'm a template *core* application."))
       (<:div :id "footer" (footer)))))