(in-package :cl-user)

(defpackage :tr.gen.core.manager
  (:nicknames :manager)
  (:import-from #:core-server #:host #:port #:peers-max #:peer-class
		#:protocol #:username #:password #:mail-port #:ssl
		#:server)
  (:use :common-lisp :core-server :arnesi)
  (:export #:manager-web-application-mixin #:web-application.oauth-uri
	   #:web-application.oauth-handler-uri #:web-application.api-uri))


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

	   ;; Auth
	   #:login
	   #:registration
	   #:authentication-error
	   
	   ;; Login Link
	   #:login-link
	   #:login-link/anonymous
	   #:login-link/registered

	   ;; Account
	   #:accounts
	   #:accounts/anonymous
	   #:accounts/registered

	   ;; Plugin
	   #:authentication-plugin
	   #:authentication-plugin/anonymous
	   #:authentication-plugin/user

	   ;; API Client
	   #:realm.list
	   #:realm.add
	   #:realm.update
	   #:realm.delete
	   
	   ))


;; -------------------------------------------------------------------------
;; A Namespace for API Calls
;; -------------------------------------------------------------------------
(defpackage :<core-server
  (:nicknames :tr.gen.core.server.markup :core-server.markup)
  (:use :core-server)
  (:export #:markup #:markup+ #:response #:error
	   #:login #:logout #:authentication #:user ))