(in-package :cl-user)
(defpackage :tr.gen.core.server
  (:nicknames :core-server)
  (:use :common-lisp :iterate :cl-prevalence :yaclml :arnesi :ucw :sb-bsd-sockets)
  (:shadowing-import-from #:ucw #:start)
  (:shadowing-import-from #:swank #:send #:receive #:accept-connection)
  (:shadowing-import-from #:arnesi #:name #:body #:self)
  (:export   
   ;; [Streams]
   #:core-stream
   #:peek-stream
   #:read-stream
   #:write-stream
   #:rewind-stream
   #:checkpoint-stream
   #:commit-stream
   ;; [Stream Types]
   #:core-vector-io-stream
   #:core-string-io-stream
   #:core-fd-io-stream
   #:core-file-io-stream
   #:make-core-stream
   ;; rfc 2109
   #:cookie
   #:cookie.name
   #:cookie.value
   #:cookie.version
   #:cookie.comment
   #:cookie.domain
   #:cookie.max-age
   #:cookie.path
   #:cookie.secure
   #:make-cookie
   #:cookiep
   #:cookie!
   #:rfc2109-cookie-header?
   #:rfc2109-cookie-value?
   #:rfc2109-quoted-value?
   #:cookie?
   ;; rfc 2388
   #:mime-part
   #:mime-part.name
   #:mime-part.filename
   #:mime-part.content-type
   #:mime-part.charset
   #:mime-part.encoding
   #:mime-part.mixed-boundary
   #:mime-part.data
   #:make-mime-part
   #:rfc2388-boundary-char?
   #:rfc2388-boundary?
   #:rfc2388-content-disposition?
   #:rfc2388-content-type?
   #:rfc2388-content-transfer-encoding?
   #:rfc2388-mime-part?
   #:mime-parts?
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
   #:current-application
   #:current-window
   #:current-session
   #:find-ucw-application
   
   ;; [Database]
   ;; classes
   #:database-server
   #:standard-model-class
   ;; Accessors
   #:database-server.model-class
   #:standard-model-class.creation-date
   ;; API
   #:database-server.model-class
   #:create-guard-with-mutex
   #:model
   #:make-database
   #:update-slots
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
   ;; API
   #:add-email
   #:del-email

   ;; [Ticket]
   ;; classes
   #:ticket-model
   #:ticket-server
   ;; API
   #:make-ticket-server
   #:generate-tickets
   #:find-ticket
   #:add-ticket
   #:ticket-model.tickets
   #:ticket-server.db

   ;; [Core]
   ;; classes
   #:core-server
   #:core-web-server
   #:*core-server*
   #:*core-web-server*
   ;; [Whois]
   ;; API
   #:whois
   ;; Helpers
   #:with-current-directory
   #:make-project-path
   #:with-current-directory
   #:time->string
   #:+day-names+
   #:+month-names+

   ;; [Serializable Application]
   #:serializable-web-application
   ;; [Darcs Web Application]
   ;; classes
   #:make-darcs-application
   #:darcs-application
   #:src/model
   #:src/packages
   #:src/interfaces
   #:src/security
   #:src/tx
   #:src/ui/main
   #:src/application
   #:darcs-application.sources
   #:serialize-source
   #:serialize-asd
   #:serialize
   #:share
   #:evaluate
   #:record
   #:put
   #:push-all
   ;; [Helpers]
   #:make-keyword
   ;; [Search]
   #:core-search
   #:string-search
   #:integer-search
   ))
