(in-package :cl-user)

(defpackage :tr.gen.core.manager
  (:nicknames :manager)
  (:import-from #:core-server #:host #:port #:peers-max #:peer-class
		#:protocol #:username #:password #:mail-port #:ssl
		#:server)
  (:use :common-lisp :core-server :arnesi))


(defpackage :tr.gen.core.manager.widget
  (:nicknames :<manager)
  (:export #:controller

	   ;; Info
	   #:server-info/crud
	   #:server-info

	   ;; Server
	   #:server
	   #:server/crud

	   ;; Socket
	   #:socket-server/crud
	   #:socket-server

	   ;; Database
	   #:database-server/crud
	   #:database-server

	   ;; Mail Sender
	   #:mail-sender/crud
	   #:mail-sender

	   ;; Applications
	   #:application/table
	   #:web-application/crud

	   #:applications
	   #:application-crud
	   #:dynamic-application-crud

	   ;; Sites
	   #:sites
	   #:site/table
	   #:site/crud

	   ;; Administrators
	   #:admin/table
	   #:admin/crud
	   #:administrators

	   ;; Settings
	   #:settings
	   ))