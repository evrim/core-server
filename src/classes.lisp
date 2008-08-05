;; Core Server: Web Application Server

;; Copyright (C) 2006-2008  Metin Evrim Ulu, Aycan iRiCAN

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :tr.gen.core.server)

;;+----------------------------------------------------------------------------
;;| [Core-serveR] Application Server Base Classes
;;+----------------------------------------------------------------------------
;;
;; This file contains base classes to use/extend/inherit while building your
;; application or server. These are kept together for developers to easily
;; identify their needs while building their project.
;;

;;-----------------------------------------------------------------------------
;; Application Classes
;;-----------------------------------------------------------------------------
(defclass application ()
  ((server :accessor application.server :initform nil
	   :documentation "On which server this application is running, setf'ed after (register)")
   (initargs :accessor application.initargs :initarg :initargs :initform nil
	     :documentation "Init-args when this instance is created."))
  (:documentation "Base Application Class"))

(defclass web-application (application)
  ((fqdn :reader web-application.fqdn :initarg :fqdn :initform (error "Fqdn must be supplied.")
	 :documentation "Fully qualified domain name of this application")
   (admin-email :accessor web-application.admin-email :initarg :admin-email
		:initform (error "Admin email must be supplied.")
		:documentation "Administrators' email address")
   (project-name :accessor web-application.project-name
		 :initarg :project-name :initform nil
		 :documentation "Name/Symbol of the project")
   (project-pathname :accessor web-application.project-pathname
		     :initarg :project-pathname :initform nil
		     :documentation "Pathname of the project")
   (htdocs-pathname :accessor web-application.htdocs-pathname
		    :initarg :htdocs-pathname :initform nil
		    :documentation "Htdocs pathname of the project,
		    used for serving static files"))
  (:documentation "Base Web Application Class"))

(defclass http-application (web-application)
  ((urls :accessor application.urls :initarg :urls :initform '()
	 :documentation "A list that contains URLs that this application handles")
   (sessions :accessor application.sessions :initform (make-hash-table :test #'equal)
	     :documentation "A hash-table that holds sessions")
   (debug :accessor application.debug :initform t
	  :documentation "Debugging flag for this application"))
  (:documentation "HTTP Application Class"))

(defmethod print-object ((self http-application) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (if (typep self 'server)
	(format stream "FQDN:\"~A\" is ~A running" (web-application.fqdn self)
		(if (status self) "" "*not*"))
	(format stream "FQDN:\"~A\"" (web-application.fqdn self)))))

(defclass http-session ()
  ((id :reader session.id :initform (random-string 8))
   (continuations :reader session.continuations :initform (make-hash-table :test #'equal)) 
   (timestamp :accessor session.timestamp :initform (get-universal-time))
   (data :accessor session.data :initform (make-hash-table :test #'equal))))

(defclass http-context ();;    (core-cps-stream)
  ((request :accessor context.request :initarg :request :initform nil)
   (response :accessor context.response :initarg :response :initform nil)
   (session :accessor context.session :initarg :session :initform nil)
   (application :accessor context.application :initarg :application :initform nil)
   (continuation :accessor context.continuation :initform nil)
   (returns :accessor context.returns :initform nil)))

(defclass apache-web-application (web-application)
  ((vhost-template-pathname
    :accessor apache-web-application.vhost-template-pathname :initarg :vhost-template-pathname
    :initform (merge-pathnames
	       (make-pathname :directory '(:relative "etc") :name "vhost" :type "conf")
	       (asdf:component-pathname (asdf:find-system :core-server)))
    :documentation "Apache Virtual Host Template Configuration Pathname - see etc/vhost.conf") 
   (default-entry-point
     :accessor apache-web-application.default-entry-point :initarg :default-entry-point
     :initform "index.core"
     :documentation "Default Entry Point for redirector creation, setq nil not to.")
   (skel-pathname
    :accessor apache-web-application.skel-pathname :initarg :skel-pathname
    :initform (merge-pathnames (make-pathname :directory '(:relative "etc" "skel"))
			       (asdf:component-pathname (asdf:find-system :core-server)))
    :documentation "Skeleton Pathname which is copied to htdoc directory. setq nil no to."))
  (:documentation "Apache Web Application Class - This class is used
this to manage vhost configuration for this application. It generates
a new vhost configuration from 'vhost-template-pathname' and writes it
to apache vhost configuration directory.  See src/servers/apache.lisp
for implementation."))

(defclass serializable-web-application (web-application)
  ((sources :accessor serializable-web-application.sources :initarg :sources
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
	:initform (list :common-lisp :core-server :cl-prevalence :arnesi))
   (depends-on :accessor serializable-web-application.depends-on :initarg :depends-on
	       :initform (list :arnesi :core-server)))
  (:documentation "Base class for template application - This class is
used to create a new application. See
src/applications/serializable-application.lisp for implementation"))

(defclass darcs-application (serializable-web-application)
  ()
  (:documentation "Darcs Application Class - A
serializable-application that uses Darcs (http://darcs.net) as SCM"))

(defclass git-application (serializable-web-application)
  ()
  (:documentation "Git Application Class - A serializable-application
that uses GIT (http://git.or.cz) as SCM"))

;;-----------------------------------------------------------------------------
;; Server Classes
;;-----------------------------------------------------------------------------
(defclass server ()
  ((name :accessor server.name :initarg :name :initform "Dummy Server"
	 :documentation "Name of the server")
   (mutex :accessor server.mutex :initarg :mutex :initform (sb-thread:make-mutex :name "Server mutex")
	  :documentation "Lock used to synchronize some operations on server")
   (auto-start :accessor server.auto-start :initarg :auto-start :initform nil
	       :documentation "If t, the server would be started when created")
   (debug  :accessor server.debug :initarg :debug :initform t
	  :documentation "Debugging flag of generic server"))
  (:documentation "Server Base Class"))

(defmethod print-object ((self server) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "\"~A\" is~A running." (server.name self)
	    (if (status self) "" " *not*"))))

(defclass socket-server (server)
  ((host :initarg :host :initform "0.0.0.0"
	 :documentation "IP address that this server binds to")
   (port :initarg :port :initform 3009
	 :documentation "Port that this server binds to")
   (protocol :initarg :protocol :initform :tcp
	     :documentation "Network Protocol, can be :udp, :tcp")
   (reuse-address :initarg :reuse-address :initform t
		  :documentation "TCP reuse address option")
   (backlog :initarg :backlog :initform 1
	    :documentation "TCP backlog option")
   (peers-max :initarg :peers-max :initform 6
	      :documentation "Number of peers that this server manages")
   (element-type :initarg :element-type :initform '(unsigned-byte 8)
		 :documentation "Data type for socket stream")
   (external-format :initarg :external-format :initform :utf-8
		    :documentation "External format for stream")
   (peer-class :initarg :peer-class :initform 'stream-peer
	       :documentation "Class to instantiate as peer thread.")
   ;;   (request-timeout-length :initarg :request-timeout-length :initform 90)
   (%socket :initform nil) (%peers :initform nil) (%socket-thread :initform nil)
   (%debug-unit :initform nil))
  (:documentation "Socket Server Class"))

(defclass web-server (server)
  ()
  (:documentation "Web Server Base Class"))

(defclass custom-http-peer (http-peer)
  ())

(defclass http-server (web-server socket-server logger-server)
  ((applications :accessor server.applications :initform '()))
  (:default-initargs :port 3001 :peer-class '(custom-http-peer)))

(defclass apache-server (web-server)
  ((apachectl-pathname
    :accessor apache-server.apachectl-pathname :initarg :apachectl-pathname
    :initform
    #+pardus (make-pathname :directory '(:absolute "usr" "sbin") :name "apache2ctl")
    #+debian (make-pathname :directory '(:absolute "usr" "sbin") :name "apache2ctl")
    #+(not (or pardus debian)) (make-pathname :directory '(:absolute "etc" "init.d") :name "apache2"))
   (htpasswd-pathname
    :accessor apache-server.htpasswd-pathname :initarg :htpasswd-pathname
    :initform
    #+debian (make-pathname :directory '(:absolute "usr" "bin") :name "htpasswd")
    #-debian (make-pathname :directory '(:absolute "usr" "sbin") :name "htpasswd2"))
   (vhosts.d-pathname
    :accessor apache-server.vhosts.d-pathname :initarg :vhosts.d-pathname
    :initform
    #+debian (make-pathname :directory '(:absolute "etc" "apache2" "sites-enabled"))
    #-debian (make-pathname :directory '(:absolute "etc" "apache2" "vhosts.d")))
   (htdocs-pathname :accessor apache-server.htdocs-pathname :initarg :htdocs-pathname
		    :initform (make-pathname :directory '(:absolute "var" "www"))))
  (:default-initargs :name "Apache2 Web Server Class - Mix this class
with your server to manage Apache2 Web Server. See src/servers/apache
for implementation"))

(defclass database-server (server guarded-prevalence-system)
  ((model-class :accessor database-server.model-class :initarg :model-class
		:initform nil :documentation "model class for initial creation")
   (db-auto-start :accessor database-server.db-auto-start :initarg :db-auto-start
		  :initform nil :documentation "If t, db is autostarted."))
  (:default-initargs :file-extension "sexp"
    :serializer #'cl-prevalence::serialize-sexp
    :deserializer #'cl-prevalence::deserialize-sexp
    :directory nil
    :name "Guarded Prevalence Database Server"))

(defmethod print-object ((self database-server) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "Prevalence Database is~A running on: \"~A\"."
	    (if (status self) "" " *not*") (get-directory self))))

(defclass standard-model-class ()
  ((creation-date :accessor standard-model-class.creation-date
		  :initarg :creation-date :initform nil))
  (:documentation "A base class for database model"))

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
  (:default-initargs :name "TinyDNS Server - Mix this class with your
server to manage TinyDNS server. See src/servers/tinydns.lisp for
implementation"))

;; this is a thread that logs messages to a stream
(defclass logger-server (local-unit)
  ((log-stream :accessor log-stream :initarg :log-stream :initform nil) ;;*core-output*
   (log-path :accessor log-path :initarg :log-path
	     :initform (merge-pathnames (make-pathname :directory '(:relative "var" "log"))
					(bootstrap:home))))
  (:documentation "Log Server mixin class - Mix this class with your
server to enable logging features. See src/servers/logger.lisp for
implementation"))

(defclass email-server (server)
  ()
  (:documentation "Base class for an email server"))

(defclass postfix-server (email-server)
  ((postfix-script-pathname :accessor postfix-server.postfix-script-pathname
			    :initarg postfix-script-pathname
			    :initform (make-pathname :directory '(:absolute "etc" "init.d")
						     :name "postfix"))
   (virtual-mailbox-maps :accessor postfix-server.virtual-mailbox-maps
			 :initarg :virtual-mailbox-maps
			 :initform (make-pathname :directory '(:absloute "etc" "postfix")
						  :name "vmailbox")))
  (:default-initargs :name "Postfix Mail Server - Mix this class with
your server to manage Postfix server. See src/servers/postfix.lisp for
implementation"))

(defclass ticket-model ()
  ((tickets :accessor ticket-model.tickets :initarg :tickets
	    :initform (make-hash-table :test #'equal)
	    :documentation "A list that holds tickets"))  
  (:documentation "Model class for Ticket server"))

(defclass ticket ()
  ((hash :accessor ticket.hash :initarg :hash :initform (error "No hash given.")
	 :documentation "A random string")
   (type :accessor ticket.type :initarg :type :initform nil
	 :documentation "Type of this ticket")
   (used :accessor ticket.used :initarg :used :initform nil
	 :documentation "t if this ticket is already used"))
  (:documentation "A Ticket that can be sent to people over the net to give them
temporary or permanent access to a resource"))

(defun create-unique-hash (table)
  "Creates a unique random hash string to be used with ticket server"
  (let ((hash (arnesi::random-string 10)))
    (cond
      ((null (cadr (multiple-value-list (gethash hash table)))) hash)
      (t (create-unique-hash table)))))

(defclass ticket-server (server)
  ((db :accessor ticket-server.db :initarg :db
       :initform (error "Ticket database not found! Please use :db argument.")
       :documentation "Database server of this ticker server")
   (hash-fun :accessor ticket-server.hash-fun :initarg :hash-fun
	     :initform #'(lambda (oldhashlist) 
			   (create-unique-hash oldhashlist))
	     :documentation "Customizable hash function for ticket.hash"))
  (:default-initargs :name "Ticket Server"))