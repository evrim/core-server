(in-package :tr.gen.core.server)

;;; Applications
(defclass application ()
  ((server :accessor application.server :initform nil
	   :documentation "On which server this application is running, setf'ed after (register)")
   (initargs :accessor application.initargs :initarg :initargs :initform nil
	     :documentation "Init-args when this instance is created.")))

(defclass web-application (application)
  ((fqdn :reader web-application.fqdn :initarg :fqdn :initform (error "Fqdn must be supplied."))
   (admin-email :accessor web-application.admin-email :initarg :admin-email :initform (error "Admin email must be supplied."))
   (project-name :accessor web-application.project-name
		 :initarg :project-name
		 :initform nil)
   (project-pathname :accessor web-application.project-pathname
		     :initarg :project-pathname
		     :initform nil)))

(defclass apache-web-application (web-application)
  ((vhost-template-pathname :accessor apache-web-application.vhost-template-pathname :initarg :vhost-template-pathname
			    :initform (merge-pathnames (make-pathname :directory '(:relative "etc") :name "vhost" :type "conf")
						       (asdf:component-pathname (asdf:find-system :core-server)))
			    :documentation "Apache Vhost Configuration Template Pathname") 
   (default-entry-point :accessor apache-web-application.default-entry-point :initarg :default-entry-point
			:initform "index.core" :documentation "Default Entry Point for redirector creation, setq nil not to.")
   (skel-pathname :accessor apache-web-application.skel-pathname :initarg :skel-pathname
		  :initform (merge-pathnames (make-pathname :directory '(:relative "etc" "skel"))
					     (asdf:component-pathname (asdf:find-system :core-server)))
		  :documentation "Skeleton Pathname which is copied to htdoc directory. setq nil no to.")))

(defclass ucw-web-application (web-application) ;; ucw:modular-application
  ((persistent :accessor ucw-web-application.persistent :initarg :persistent :initform nil))
  ;; (:default-initargs :debug-on-error t)
  )

(defclass serializable-web-application (web-application)
  ((sources :accessor serializable-web-application.sources
	    :initarg :sources
	    :initform '(src/packages src/model src/tx src/interfaces src/application src/security src/ui/main))
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
		       #+pardus (make-pathname :directory '(:absolute "usr" "sbin") :name "apache2ctl")
		       #+debian (make-pathname :directory '(:absolute "usr" "sbin") :name "apache2ctl")
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
  ((creation-date :accessor standard-model-class.creation-date :initarg :creation-date :initform nil)))

(defclass name-server (server)
  ((ns-script-pathname :accessor name-server.ns-script-pathname :initarg :ns-script-pathname
		       :initform
		       #-debian (make-pathname :directory '(:absolute "etc" "init.d") :name "svscan")
		       #+debian (make-pathname :directory '(:absolute "etc" "init.d") :name "djbdns"))
   (ns-db-pathname :accessor name-server.ns-db-pathname :initarg :ns-db-pathname
		   :initform (if (sb-posix:getenv "CORESERVER_HOME")
				 (merge-pathnames (make-pathname :directory '(:relative "var" "tinydns"))
						  (sb-posix:getenv "CORESERVER_HOME"))
				 (merge-pathnames (make-pathname :directory '(:relative "db" "ns"))
						  (asdf:component-pathname (asdf:find-system :core-server)))))
   (ns-root-pathname :accessor name-server.ns-root-pathname :initarg :ns-root-pathname
		     :initform (make-pathname :directory '(:absolute "service" "tinydns" "root")))
   (ns-compiler-pathname :accessor name-server.ns-compiler-pathname :initarg ns-compiler
			 :initform (make-pathname :directory '(:absolute "usr" "bin") :name "tinydns-data"))
   (ns-db :accessor name-server.ns-db :initarg ns-db
	  :initform (make-instance 'database-server
				   :directory (if (sb-posix:getenv "CORESERVER_HOME")
						  (merge-pathnames (make-pathname :directory '(:relative "var" "tinydns"))
								   (sb-posix:getenv "CORESERVER_HOME"))
						(merge-pathnames (make-pathname :directory '(:relative "db" "ns"))
								 (asdf:component-pathname (asdf:find-system :core-server)))) 
				   :model-class 'ns-model)))
  (:default-initargs :name "TinyDNS Server"))

(defclass ns-model ()
  ((domains :accessor ns-model.domains :initarg :domains :initform (make-hash-table :test #'equal))))

(defclass ns-record ()
  ((source :accessor ns-record.source :initarg :source :initform nil) 
   (target :accessor ns-record.target :initarg :target :initform nil)))

(defmethod print-object ((self ns-record) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "Source: ~A Target: ~A" (ns-record.source self) (ns-record.target self))))

(defclass ns-mx (ns-record)
  ())

(defclass ns-alias (ns-record)
  ())

(defclass ns-ns (ns-record)
  ())

(defclass ns-host (ns-record)
  ())

(defclass ucw-model ()
  ((applications :accessor ucw-model.applications :initarg :applications :initform (make-hash-table :test #'equal))))

(defclass ucw-server (server) ;; ucw::standard-server
  ((ucw-db :accessor ucw-server.ucw-db :initarg ucw-db
	   :initform (make-instance 'database-server
				    :directory
				    (if (sb-posix:getenv "CORESERVER_HOME")
					(merge-pathnames (make-pathname :directory '(:relative "var" "ucw"))
							 (pathname (sb-posix:getenv "CORESERVER_HOME")))
				      (merge-pathnames (make-pathname :directory '(:relative "db" "ns"))
						       (asdf:component-pathname (asdf:find-system :core-server)))) 
				    :model-class 'ucw-model)))
  (:default-initargs :name "Ucw Web Server"
    ;; :backend (ucw::make-backend :mod-lisp
;; 				:host "127.0.0.1"
;; 				:port 3001)
    ))

(defclass email-server (server)
  ())

(defclass postfix-server (email-server)
  ((postfix-script-pathname :accessor postfix-server.postfix-script-pathname :initarg postfix-script-pathname
			    :initform (make-pathname :directory '(:absolute "etc" "init.d") :name "postfix"))
   (virtual-mailbox-maps :accessor postfix-server.virtual-mailbox-maps :initarg :virtual-mailbox-maps
			 :initform (make-pathname :directory '(:absloute "etc" "postfix") :name "vmailbox")))
  (:default-initargs :name "Postfix mail Server"))

(defclass ticket-model ()
  ((tickets :accessor ticket-model.tickets :initarg :tickets :initform (make-hash-table :test #'equal))))

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