(in-package :cl-user)
(defpackage :tr.gen.core.server
  (:nicknames :core-server)
  (:use :common-lisp :iterate :cl-prevalence :yaclml :arnesi :ucw :sb-bsd-sockets :bordeaux-threads)
  (:shadowing-import-from #:ucw #:start)
  (:shadowing-import-from #:swank #:send #:receive #:accept-connection)
  (:shadowing-import-from #:arnesi #:name #:body #:self)
  (:export 
   ;; Threads
   #:thread-mailbox
   #:thread-send
   #:thread-receive
   #:cleanup-mailbox
   #:thread-spawn
   #:thread-kill
   ;; [Streams]
   #:core-stream
   #:read-stream
   #:peek-stream
   #:checkpoint-stream
   #:commit-stream
   #:rewind-stream
   #:write-stream
   #:close-stream
   #:core-streamp
   ;; [Stream Types]
   #:core-vector-io-stream
   #:core-string-io-stream
   #:core-fd-io-stream
   #:core-file-io-stream
   #:make-core-stream
   ;; Stream Helpers
   #:with-core-stream
   ;; sockets
   #:resolve-hostname
   #:make-server
   #:close-server
   #:accept
   #:connect
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
   ;; rfc 2045
   #:quoted-printable?
   #:quoted-printable!
   #:base64?
   #:base64!
   ;;;; header symbol
   #:quoted-printable
   ;; rfc 2046
   ;;;; classes and methods
   #:mime
   #:mime.headers
   #:top-level-media
   #:mime.data
   #:composite-level-media
   #:mime.children
   ;;;; utilities
   #:mimes?
   #:make-top-level-media
   #:make-composite-level-media
   #:mime-search
   #:mime.header
   ;;;; header symbols
   #:content-type
   ;; rfc 2388
   #:rfc2388-mimes?
   ;; rfc 2396
   #:uri
   #:uri.scheme
   #:uri.username
   #:uri.password
   #:uri.server
   #:uri.port
   #:uri.paths
   #:uri.queries
   #:uri.fragments
   #:urip
   #:make-uri
   #:uri?
   #:query!
   #:uri!
   ;; rfc 2616
   ;; classes
   #:http-request
   #:http-response
   ;; accessors
   #:http-message.version
   #:http-message.general-headers
   #:http-message.unknown-headers
   #:http-message.entities
   #:http-request.method
   #:http-request.uri
   #:http-request.headers
   #:http-request.entity-headers
   #:http-response.headers
   #:http-response.status-code
   ;; helpers
   ;;; request
   #:http-accept?
   #:http-accept-charset?
   #:http-accept-encoding?
   #:http-accept-language?
   #:http-request-headers?
   #:http-expect?
   #:http-from?
   #:http-host?
   #:http-if-match?
   #:http-if-modified-since?
   #:http-if-none-match?
   #:http-if-range?
   #:http-if-unmodified-since?
   #:http-max-forwards?
   #:http-range?
   #:http-referer?
   #:http-te?
   #:http-user-agent?
   #:http-response!
   ;;; response
   #:http-accept-ranges!
   #:http-age!
   #:http-etag!
   #:http-location!
   #:http-proxy-authenticate!
   #:http-retry-after!
   #:http-server!
   #:http-vary!
   #:http-wwww-authenticate!
   ;; header symbols
   ;; request methods
   #:OPTIONS
   #:GET
   #:HEAD
   #:POST
   #:PUT
   #:DELETE
   #:TRACE
   #:CONNECT
   ;; cache request directives
   #:NO-CACHE
   #:NO-STORE
   #:MAX-AGE
   #:MAX-STALE
   #:MIN-FRESH
   #:NO-TRANSFORM
   #:ONLY-IF-CACHED
   ;; cache response directives
   #:PUBLIC
   #:PRIVATE
   #:NO-CACHE
   #:NO-STORE
   #:NO-TRANSFORM
   #:MUST-REVALIDATE
   #:PROXY-REVALIDATE
   #:MAX-AGE
   #:S-MAXAGE
   ;; general headers
   #:CACHE-CONTROL
   #:CONNECTION
   #:DATE
   #:PRAGMA
   #:TRAILER
   #:TRANSFER-ENCODING
   #:UPGRADE
   #:VIA
   #:WARNING
   ;; request headers
   #:ACCEPT
   #:ACCEPT-CHARSET
   #:ACCEPT-ENCODING
   #:ACCEPT-LANGUAGE
   #:AUTHORIZATION
   #:EXPECT
   #:FROM
   #:HOST
   #:IF-MATCH
   #:IF-MODIFIED-SINCE
   #:IF-RANGE
   #:IF-UNMODIFIED-SINCE
   #:MAX-FORWARDS
   #:PROXY-AUTHORIZATION
   #:RANGE
   #:REFERER
   #:TE
   #:USER-AGENT
   ;; response headers
   #:ACCEPT-RANGES
   #:AGE
   #:ETAG
   #:LOCATION
   #:PROXY-AUTHENTICATE
   #:RETRY-AFTER
   #:SERVER
   #:VARY
   #:WWW-AUTHENTICATE
   ;; entity headers
   #:ALLOW
   #:CONTENT-ENCODING
   #:CONTENT-LANGUAGE
   #:CONTENT-LENGTH
   #:CONTENT-LOCATION
   #:CONTENT-MD5
   #:CONTENT-RANGE
   #:CONTENT-TYPE
   #:EXPIRES
   #:LAST-MODIFIED
   ;; browser symbols
   #:BROWSER
   #:VERSION
   #:OPERA
   #:MOZ-VER
   #:OS
   #:REVISION
   #:IE
   #:SEAMONKEY
   ;; 
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
