(in-package :cl-user)

(defpackage :tr.gen.core.manager
  (:nicknames :manager)
  (:import-from #:core-server #:host #:port #:peers-max #:peer-class
		#:protocol #:username #:password #:mail-port #:ssl
		#:server)
  (:use :common-lisp :core-server :arnesi))


(defpackage :tr.gen.core.manager.widget
  (:nicknames :<manager)
  (:export #:settings #:applications #:applications-table #:application-crud
	   #:dynamic-application-crud
	   #:web-application-crud #:server #:server-info
	   #:controller #:server/crud #:socket-server/crud
	   #:socket-server-info
	   #:database-server/crud
	   #:database-server-info #:mail-sender/crud
	   #:mail-sender-info))