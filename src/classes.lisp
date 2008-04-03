(in-package :tr.gen.core.server)

;;; Applications
(defclass application ()
  ((server :accessor application.server :initform nil
	   :documentation "On which server this application is running, setf'ed after (register)")
   (initargs :accessor application.initargs :initarg :initargs :initform nil
	     :documentation "Init-args when this instance is created.")))

(defclass web-application (application)
  ((fqdn :reader web-application.fqdn :initarg :fqdn :initform (error "Fqdn must be supplied."))
   (admin-email :accessor web-application.admin-email :initarg :admin-email
		:initform (error "Admin email must be supplied."))
   (project-name :accessor web-application.project-name
		 :initarg :project-name
		 :initform nil)
   (project-pathname :accessor web-application.project-pathname
		     :initarg :project-pathname
		     :initform nil)
   (htdocs-pathname :accessor web-application.htdocs-pathname
		    :initarg :htdocs-pathname
		    :initform nil)))

(defclass apache-web-application (web-application)
  ((vhost-template-pathname :accessor apache-web-application.vhost-template-pathname
			    :initarg :vhost-template-pathname
			    :initform (merge-pathnames (make-pathname :directory '(:relative "etc")
								      :name "vhost" :type "conf")
						       (asdf:component-pathname
							(asdf:find-system :core-server)))
			    :documentation "Apache Vhost Configuration Template Pathname") 
   (default-entry-point :accessor apache-web-application.default-entry-point
                        :initarg :default-entry-point
			:initform "index.core"
			:documentation "Default Entry Point for redirector creation, setq nil not to.")
   (skel-pathname :accessor apache-web-application.skel-pathname :initarg :skel-pathname
		  :initform (merge-pathnames (make-pathname :directory '(:relative "etc" "skel"))
					     (asdf:component-pathname (asdf:find-system :core-server)))
		  :documentation "Skeleton Pathname which is copied to htdoc directory. setq nil no to.")))

(defclass serializable-web-application (web-application)
  ((sources :accessor serializable-web-application.sources
	    :initarg :sources
	    :initform '(src/packages src/model src/tx src/interfaces
			src/application src/security src/ui/main))
   (directories :accessor serializable-web-application.directories
		:initarg :directories
		:initform (list (make-pathname :directory '(:relative "src"))
				(make-pathname :directory '(:relative "src" "ui"))
				(make-pathname :directory '(:relative "t"))
				(make-pathname :directory '(:relative "doc"))
				(make-pathname :directory '(:relative "wwwroot"))
				(make-pathname :directory '(:relative "wwwroot" "style"))
				(make-pathname :directory '(:relative "wwwroot" "images"))
				(make-pathname :directory '(:relative "templates"))
				(make-pathname :directory '(:relative "db"))))
   (use :accessor serializable-web-application.use :initarg :use
	:initform (list :common-lisp :core-server :cl-prevalence :arnesi :js)) ;; :ucw ;;yaclml
   (depends-on :accessor serializable-web-application.depends-on :initarg :depends-on
	       :initform (list :arnesi :core-server :parenscript)))) ;; :ucw

(defclass darcs-application (serializable-web-application)
  ())

(defclass git-application (serializable-web-application)
  ())

;;; Servers
(defclass server ()
  ((name :accessor server.name :initarg :name :initform "Dummy Server")
   (mutex :accessor server.mutex :initarg :mutex :initform (sb-thread:make-mutex :name "Server mutex"))
   (auto-start :accessor server.auto-start :initarg :auto-start :initform nil)))

(defmethod print-object ((self server) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "\"~A\" is~A running." (server.name self) (if (status self) "" " *not*"))))

(defclass web-server (server)
  ())

(defclass apache-server (web-server)
  ((apachectl-pathname :accessor apache-server.apachectl-pathname :initarg :apachectl-pathname
		       :initform
		       #+pardus (make-pathname :directory '(:absolute "usr" "sbin")
					       :name "apache2ctl")
		       #+debian (make-pathname :directory '(:absolute "usr" "sbin")
					       :name "apache2ctl")
		       #+(not (or pardus debian)) (make-pathname :directory '(:absolute "etc" "init.d") :name "apache2"))
   (htpasswd-pathname :accessor apache-server.htpasswd-pathname :initarg :htpasswd-pathname
		      :initform
		      #+debian (make-pathname :directory '(:absolute "usr" "bin") :name "htpasswd")
		      #-debian (make-pathname :directory '(:absolute "usr" "sbin") :name "htpasswd2"))
   (vhosts.d-pathname :accessor apache-server.vhosts.d-pathname :initarg :vhosts.d-pathname
		      :initform
		      #+debian (make-pathname :directory '(:absolute "etc" "apache2" "sites-enabled"))
		      #-debian (make-pathname :directory '(:absolute "etc" "apache2" "vhosts.d")))
   (htdocs-pathname :accessor apache-server.htdocs-pathname :initarg :htdocs-pathname
		    :initform (make-pathname :directory '(:absolute "var" "www"))))
  (:default-initargs :name "Apache2 Web Server"))

(defclass database-server (server guarded-prevalence-system)
  ((model-class :accessor database-server.model-class :initarg :model-class
		:initform nil :documentation "model class for initial creation")
   (db-auto-start :accessor database-server.db-auto-start :initarg :db-auto-start
		  :initform nil :documentation "when t, db is autostarted."))
  (:default-initargs :file-extension "sexp"
    :serializer #'cl-prevalence::serialize-sexp
    :deserializer #'cl-prevalence::deserialize-sexp
    :directory nil
    :name "Guarded Prevalence Database Server"))

(defmethod print-object ((self database-server) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "Prevalence Database is~A running on: \"~A\"."
	    (if (status self) "" " *not*")
	    (get-directory self))))

(defclass standard-model-class ()
  ((creation-date :accessor standard-model-class.creation-date
		  :initarg :creation-date :initform nil)))

(defclass tinydns-server (server)
  ((svc-pathname :accessor tinydns-server.svc-pathname
		 :initarg :svc-pathame :initform (whereis "svc"))
   (svstat-pathname :accessor tinydns-server.svstat-pathname
		    :initarg :svscan-pathname :initform (whereis "svstat"))
   (root-pathname :accessor tinydns-server.root-pathname :initarg :root-pathname
		  :initform (make-pathname :directory '(:absolute "service" "tinydns")))
   (compiler-pathname :accessor tinydns-server.compiler-pathname :initarg :compiler-pathname
		      :initform (whereis "tinydns-data"))
   (domains :accessor tinydns-server.domains :initform nil)
   (%timestamp :initform (get-universal-time)))
  (:default-initargs :name "TinyDNS Server"))


;; this is a thread that logs messages to a stream
(defclass logger-server (local-unit)
  ((log-stream :accessor log-stream :initarg :log-stream :initform nil) ;;*core-output*
   (log-path :accessor log-path :initarg :log-path
	     :initform (merge-pathnames (make-pathname :directory '(:relative "var" "log"))
					(sb-posix:getenv "CORESERVER_HOME")))))

(defclass email-server (server)
  ())

(defclass postfix-server (email-server)
  ((postfix-script-pathname :accessor postfix-server.postfix-script-pathname
			    :initarg postfix-script-pathname
			    :initform (make-pathname :directory '(:absolute "etc" "init.d")
						     :name "postfix"))
   (virtual-mailbox-maps :accessor postfix-server.virtual-mailbox-maps
			 :initarg :virtual-mailbox-maps
			 :initform (make-pathname :directory '(:absloute "etc" "postfix")
						  :name "vmailbox")))
  (:default-initargs :name "Postfix mail Server"))

(defclass ticket-model ()
  ((tickets :accessor ticket-model.tickets :initarg :tickets
	    :initform (make-hash-table :test #'equal))))

(defclass ticket ()
  ((hash :accessor ticket.hash :initarg :hash :initform (error "No hash given."))
   (type :accessor ticket.type :initarg :type :initform nil)
   (used :accessor ticket.used :initarg :used :initform nil)))

(defun create-unique-hash (table)
  (let ((hash (arnesi::random-string 10)))
    (cond
      ((null (cadr (multiple-value-list (gethash hash table)))) hash)
      (t (create-unique-hash table)))))

(defclass ticket-server (server)
  ((db :accessor ticket-server.db :initarg :db
       :initform (error "Ticket database not found! Please use :db argument."))
   (hash-fun :accessor ticket-server.hash-fun :initarg :hash-fun
	     :initform #'(lambda (oldhashlist) 
			   (create-unique-hash oldhashlist))))
  (:default-initargs :name "Ticket Server"))