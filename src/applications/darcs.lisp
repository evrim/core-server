(in-package :core-server)

(defun make-darcs-application (fqdn project-name admin-email project-pathname &optional use depends-on)
  (let ((params (list :fqdn fqdn
		      :project-name project-name
		      :admin-email admin-email
		      :project-pathname project-pathname)))
    (and use (nconc params (list :use use)))
    (and depends-on (nconc params (list :depends-on depends-on)))
    (apply #'make-instance
	   'darcs-application
	   params)))

(defun darcs (&rest args)
  (unwind-protect
       (sb-ext::process-exit-code
	(sb-ext:run-program +darcs+ args :output *standard-output*))))

(defun make-keyword (str)
  (intern (string-upcase str) (find-package :keyword)))

(defmethod package-keyword ((self darcs-application) &optional long)
  (or (and long (make-keyword (strcat "tr.gen.core." (web-application.project-name self))))
      (make-keyword (web-application.project-name self))))

(defmethod package-test-keyword ((self darcs-application))
  (make-keyword (format nil "~A.test" (package-keyword self))))

(defmethod model-class ((self darcs-application))  
  (intern (format nil "~A" (string-upcase (strcat (web-application.project-name self) "-model")))))

(defmethod application-class ((self darcs-application))
  (intern (format nil "~A" (string-upcase (strcat (web-application.project-name self) "-application")))))

(defmethod asd ((self darcs-application))
  `(progn
     (in-package :asdf)
     
     (defsystem ,(package-keyword self)
       :description "Core Template Application"
       :version ".1"
       :author (web-application.admin-email self)
       :maintainer "bilgi@core.gen.tr"
       :licence "LGPL v2"
       :components ((:static-file ,(strcat (web-application.project-name self) ".asd"))
		    (:module :src
			     :serial t
			     :components
			     ((:file "packages")
			      (:file "model" :depends-on ("packages"))
			      (:file "application" :depends-on ("packages" "model"))
			      (:file "tx" :depends-on ("packages" "model"))
			      (:file "interfaces" :depends-on ("tx"))
			      (:module :ui
				       :serial t
				       :components
				       ((:file "main"))))))
       :depends-on ,(darcs-application.depends-on self)
       :serial t)
     
     (defsystem ,(package-test-keyword self)
       :components ((:module :t
			     :components
			     ((:file "packages"))))
       :depends-on (,(package-keyword self) :core-server :FiveAM))

     (defmethod perform ((op asdf:test-op) (system (eql (find-system ,(package-keyword self)))))
       (asdf:oos 'asdf:load-op ,(package-test-keyword self))
       (funcall (intern (string :run!) (string :it.bese.FiveAM)) ,(package-keyword self)))))

(defmethod src/packages ((self darcs-application))
  `(progn
     (in-package :cl-user)
     (defpackage ,(package-keyword self t)       
       (:nicknames ,(package-keyword self))
       (:use :common-lisp :ucw :core-server :cl-prevalence)
       (:import-from #:yaclml #:file-system-generator))))

(defmethod src/model ((self darcs-application))  
  `(progn
     (in-package ,(package-keyword self))
     (defclass ,(model-class self) ()
       ())
     (defmethod print-object ((self ,(model-class self)) stream)
       (print-unreadable-object (self stream :type t :identity t)))))

(defmethod src/tx ((self darcs-application))
  `(progn
     (in-package ,(package-keyword self))))

(defmethod src/interfaces ((self darcs-application))
  `(progn
     (in-package ,(package-keyword self))))


(defmethod src/application ((self darcs-application))
  `(progn
     (in-package ,(package-keyword self))
     (defvar *wwwroot* (make-project-path ,(symbol-name (package-keyword self)) "wwwroot"))
     (defvar *talroot* (make-project-path ,(symbol-name (package-keyword self)) "templates"))
     (defvar *db-location* (make-project-path ,(symbol-name (package-keyword self)) "db"))
     (defclass ,(application-class self) (ucw-web-application
					  database-server
					  cookie-session-application-module
					  ajax-application-module
					  darcs-application)
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
	 :sources ',(darcs-application.sources self)
	 :directories ',(darcs-application.directories self)
	 :use ',(darcs-application.use self)
	 :depends-on ',(darcs-application.depends-on self)))
     (defvar *app* (make-instance ',(application-class self)))
     (defun register-me ()
       (core-server::register core-server::*server* *app*))     
     (defun unregister-me ()
       (core-server::unregister core-server::*server* *app*))
     (defentry-point "^index.*$" (:application *app* :class regexp-dispatcher)
	 ()
       (call 'main-window))))

(defmethod src/security ((self darcs-application))
  `(progn
     (in-package ,(package-keyword self))))

(defmethod src/ui/main ((self darcs-application))
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

(defmethod source-to-pathname ((self darcs-application) symbol)
  (let* ((result (cl-ppcre:split "(/)" (string-downcase (string symbol))))
	 (directories (butlast result))
	 (file (last1 result)))
    (merge-pathnames (make-pathname :directory (cons :relative directories) :name file :type "lisp")
		     (web-application.project-pathname self))))

(defmethod serialize-source ((self darcs-application) symbol)
  (with-package :core-server
    (with-open-file (out (source-to-pathname self symbol) :direction :output :if-does-not-exist :create :if-exists :supersede)
      (let ((*print-escape* nil)
	    (*print-pretty* t))
	(mapcar #'(lambda (line)
		    (format out "~A" (string-downcase (format nil "~S~%~%" line))))
		(cdr (apply symbol (list self))))))))

(defmethod serialize-asd ((self darcs-application))
  (with-open-file (out (merge-pathnames (make-pathname :name (web-application.project-name self) :type "asd")
					(web-application.project-pathname self))
		       :direction :output :if-does-not-exist :create :if-exists :supersede)
    (mapcar #'(lambda (line)
		(format out "~A" (string-downcase (format nil "~S~%~%" line))))
	    (cdr (apply 'asd (list self))))))

(defmethod darcs-directory ((self darcs-application))
  (merge-pathnames (make-pathname :directory '(:relative "_darcs"))
		   (web-application.project-pathname self)))

(defmethod darcs-author-file ((self darcs-application))
  (merge-pathnames (make-pathname :directory '(:relative "prefs") :name "author") (darcs-directory self)))
  
(defmethod serialize ((self darcs-application))  
  ;; Error if exists
  (when (probe-file (darcs-directory self))
    (error "Project already has _darcs folder."))
    
  ;; Create template folders
  (mapcar #'(lambda (dir)
	      (ensure-directories-exist (merge-pathnames dir (web-application.project-pathname self))))
	  (darcs-application.directories self))

  (serialize-asd self)

  (mapcar (curry #'serialize-source self) (darcs-application.sources self))    

  (init self)
  (record self)
  (put self))

(defmethod init ((self darcs-application))
  (if (zerop (with-current-directory (web-application.project-pathname self)
	       (darcs "init")))
      (progn
	(with-open-file (out (darcs-author-file self) :direction :output :if-does-not-exist :create)
	  (format out " <~A>" "bilgi@core.gen.tr")))
      (error "darcs init failed.")))

(defmethod record ((self darcs-application) &optional patch-name)
  (with-current-directory (web-application.project-pathname self)
    (darcs "record" "-m" (or patch-name "core-server checkpoint")
	   "--all" "--author" "bilgi@core.gen.tr" "--skip-long-comment" "--look-for-adds")))

(defmethod put ((self darcs-application) &optional (remote-repo (format nil "~A@node2:/home/projects/~A" 
									+remote-user+ (web-application.project-name self))))
  (with-current-directory (web-application.project-pathname self)
    (darcs "put" remote-repo)))

(defmethod push-all ((self darcs-application))
  (with-current-directory (web-application.project-pathname self)    
    (darcs "push" "--all")))

