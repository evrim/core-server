(in-package :cl-user)
(defpackage :tr.gen.core.server
  (:nicknames :core-server)
  (:use :common-lisp :iterate :cl-prevalence :yaclml :arnesi :ucw)
  (:shadow #:ucw #:start #:arnesi #:name #:body #:self)
  (:export
   ;; [Protocol]
   ;; classes
   #:application
   #:web-application
   #:server
   #:web-server
   ;; accessors
   #:server.name
   #:server.mutex
   #:web-application.fqdn
   #:web-application.admin-email   
   ;; API
   #:start
   #:stop
   #:status
   #:register
   #:unregister
   #:with-server-mutex
   ;; [Apache]
   ;; classes
   #:apache-server
   #:apache-web-application
   ;; accessors
   #:apache-web-application.vhost-template-pathname
   #:apache-web-application.redirector-pathname
   #:apache-web-application.default-entry-point
   #:apache-web-application.skel-pathname
   #:apache-web-application.config-pathname
   #:apache-web-application.docroot-pathname
   #:apache-server.apachectl-pathname
   #:apache-server.htpasswd-pathname
   #:apache-server.vhosts.d-pathname
   #:apache-server.htdocs-pathname
   ;; API
   #:graceful
   #:create-docroot
   #:create-vhost-config
   #:create-redirector
   #:validate-configuration
   #:config-pathname
   #:apache-server.refresh
   #:apache-server.destroy
   ;; [UnCommonWeb]
   ;; classes
   #:ucw-server
   #:ucw-web-application
   ;; API
   #:ucw-server-fqdns
   
   ;; [Database]
   ;; classes
   #:database-server
   ;; Accessors
   #:database-server.model-class
   ;; API
   #:database-server.model-class
   #:create-guard-with-mutex
   #:model
   #:make-database

   ;; [Nameserver]
   ;; classes
   #:name-server
   #:ns-model
   #:ns-mx
   #:ns-alias
   #:ns-ns
   #:ns-host
   ;; accessors
   #:name-server.ns-script-pathname
   #:name-server.ns-db-pathname
   #:name-server.ns-root-pathname
   #:name-server.ns-compiler-pathname
   #:name-server.ns-db
   #:ns-model.domains
   #:ns-record.source
   #:ns-record.target
   ;; API   
   #:with-nameserver-refresh
   #:name-server.refresh
   #:host-part
   #:domain-part
   #:find-host
   #:add-mx
   #:add-ns
   #:add-host
   #:add-alias
   #:find-domain-records
   ;; [Postfix]
   ;; classes
   #:email-server
   #:postfix-server
   ;; Helpers
   #:with-current-directory
   ))

(defpackage :tr.gen.core.server.test
  (:nicknames :core-server.test)
  (:use :common-lisp :iterate :cl-prevalence :core-server
	;; :cl-store
	))
