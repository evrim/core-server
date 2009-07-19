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

;;+----------------------------------------------------------------------------
;;|
;;| [Core-serveR] Project - http://labs.core.gen.tr
;;|
;;+----------------------------------------------------------------------------
;;| Author: Evrim Ulu <evrim@core.gen.tr>
;;| Co-Author: Aycan Irican <aycan@core.gen.tr>
;;|
;;| Project launch date: Dec 2006
;;+----------------------------------------------------------------------------

(in-package :cl-user)
(defpackage :tr.gen.core.install)

(defpackage :tr.gen.core.ffi
  (:nicknames :core-ffi)
  (:use :cl :cffi)
  (:export
   #:gethostbyname
;;;    #:epollin
;;;    #:epollout
;;;    #:epollerr
;;;    #:make-epoll-device
;;;    #:wait
;;;    #:epoll-event.events
;;;    #:epoll-event.fd
   ;; socket
   #:%recv
   #:%send
   #:%close
   ;; libev
   #:ev-loop
   #:ev-unloop
   #:ev-default-loop
   #:bzero
   #:set-nonblock
   #:ev-watcher
   #:ev-io
   #:size-of-ev-io
   #:ev-io-init
   #:ev-io-start
   #:ev-io-stop
   #:ev-read
   #:evunloop-all
   #:uuid-generate))

(defpackage :tr.gen.core.server
  (:nicknames :core-server)
  (:use :common-lisp ;; :cl-prevalence
	:arnesi :cl-ppcre
	:sb-bsd-sockets :tr.gen.core.install :bordeaux-threads :cffi)
  (:shadowing-import-from #:swank #:send #:receive #:accept-connection)
  (:shadowing-import-from #:arnesi #:name #:body #:self #:new)
;;   (:import-from #:cl-prevalence #:get-directory)
  (:import-from #:arnesi #:fdefinition/cc)
  (:import-from #:sb-ext #:make-timer #:schedule-timer #:unschedule-timer #:timer)
  (:import-from :sb-mop 
		compute-class-precedence-list
		validate-superclass
		standard-slot-definition
		standard-direct-slot-definition
		standard-effective-slot-definition
		direct-slot-definition-class
		effective-slot-definition-class
		slot-definition-name
		slot-definition-initform
		slot-definition-initfunction
		compute-effective-slot-definition
		class-slots
		slot-value-using-class
		slot-boundp-using-class
		slot-makunbound-using-class
		slot-definition-allocation
		slot-definition-initargs
		class-finalized-p
		finalize-inheritance
		ensure-class-using-class
		compute-slots)
  (:import-from :sb-pcl
		initialize-internal-slot-functions
		COMPUTE-EFFECTIVE-SLOT-DEFINITION
		compute-effective-slot-definition-initargs
		slot-definition-reader-function
		slot-definition-writer-function
		slot-definition-boundp-function
		slot-definition-allocation-class
		class-slot-cells
		plist-value
		+slot-unbound+)

  (:export 
   ;; [Threads]
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
   #:return-stream
   #:checkpoint-stream/cc
   #:rewind-stream/cc
   #:commit-stream/cc
   ;; [Standard Output Wrapper]
   #:*core-output*
   ;; [Stream Types]
   #:core-vector-io-stream
   #:core-string-io-stream
   #:core-fd-io-stream
   #:core-file-io-stream
   #:pipe-stream
   #:core-transformer-stream
   #:core-cps-stream
   #:core-cps-string-io-stream
   #:core-cps-fd-io-stream
   #:core-cps-file-io-stream
   #:make-transformer-stream
   #:make-core-stream
   
   ;; [Special Transformers 4 Javascript]   
   #:make-indented-stream
   #:make-compressed-stream
   #:increase-indent
   #:decrease-indent
   
   #:make-cps-stream
   ;; [Stream Helpers]
   #:with-core-stream
   #:with-core-stream/cc
   
   ;; [Class+]
   #:find-class+
   #:class+
   #:class+.name
   #:class+.direct-superclasses
   #:class+.direct-subclasses
   #:class+.superclasses
   #:class+.subclasses
   #:class+.slots
   #:class+.rest
   #:class+.default-initargs
   #:class+.slot-search
   #:class+.local-slots
   #:class+.remote-slots
   #:class+.methods
   #:class+.local-methods
   #:class+.remote-methods
   #:class+.search   
   #:defclass+
   #:class+.register
   #:class+.register-remote-method
   #:class+.register-local-method
   #:class+.ctor
   #:local
   #:both
   #:remote
   #:primitive
   
   ;; [Sockets]
   #:resolve-hostname
   #:make-server
   #:close-server
   #:accept
   #:connect
   ;; [Units]
   #:unit
   #:standard-unit
   #:local-unit
   #:me-p
   #:defmethod/unit
   #:run

   ;; [XML Markup]
   #:xml
   #:xml+
   #:xml.tag
   #:xml.attribute
   #:xml.attributes
   #:xml.children
   #:xml.equal
   #:generic-xml

   ;; [XML Stream]
   #:make-xml-stream
   #:xml-stream
   
   ;; [Dom Markup]
   #:dom-element
   #:dom.tag
   #:dom.namespace
   #:dom.attributes
   #:dom.children
   #:set-attribute
   #:get-attribute
   #:make-dom-element
   #:dom-successor
   #:dom-element!
   #:dom-element?
   #:dom2string
   ;; [Html markup]
   #:html-element
   #:empty-html-element
   #:defhtml-tag
   #:defhtml-empty-tag
   #:html?
   #:html!
   #:with-html-output
   ;; [CSS markup]
   #:css-element
   #:css.selector
   #:css.attributes
   #:css.children
   #:css
   #:css?
   #:css!
   ;; [RSS markup]
   #:rss-element
   #:defrss-tag
   #:rss?
   #:rss!

   ;; [Javascript]
   #:js
   #:js*
   #:with-js
   #:jambda
   #:defrender/js
   #:defun/javascript
   #:+indent-javascript+
   #:defjsmacro
   #:defmacro/js
   #:defsetf/js
   #:defrender/js
   #:true
   #:false
   #:undefined
   #:while
   #:regex
   #:--
   #:create
   #:with
   #:doeach
   #:try
   #:default
   #:typeof
   #:new
   #:instanceof
   #:with-field
   #:make-component
   #:make-service   
   
   ;; [RFC 2109]
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
   ;; [RFC 2045]
   #:quoted-printable?
   #:quoted-printable!
   #:base64?
   #:base64!
;;;; header symbol
   #:quoted-printable
   ;; [RFC 2046]
;;;; classes 
   #:mime
   #:top-level-media
   #:composite-level-media
;;;; accessors
   #:mime.headers
   #:mime.data
   #:mime.children
;;;; helpers methods
   #:mime.header
   #:mime.filename
   #:mime.name
   #:mime.content-type
   #:mime.serialize      

;;;; utilities
   #:mimes?
   #:make-top-level-media
   #:make-composite-level-media
   #:mime-search
   #:mime.header
;;;; header symbols
   #:content-type
   ;; [RFC 2388]
   #:rfc2388-mimes?
   ;; [RFC 2396]
   #:uri
   #:uri.scheme
   #:uri.username
   #:uri.password
   #:uri.server
   #:uri.port
   #:uri.paths
   #:uri.queries
   #:uri.fragments
   #:uri.query
   #:urip
   #:make-uri
   #:uri?
   #:query!
   #:uri!
   ;; [RFC 822]
   #:mailbox?
   #:comment?
   #:comment!
   ;; [RFC 2616]
   ;; Classes
   #:http-request
   #:http-response
   ;; Accessors
   #:http-message.version
   #:http-message.general-headers
   #:http-message.unknown-headers
   #:http-message.entities
   #:http-request.method
   #:http-request.uri
   #:http-request.headers
   #:http-request.entity-headers
   #:http-request.stream
   #:http-request.header
   #:http-request.cookies
   #:http-request.cookie
   #:http-response.response-headers
   #:http-response.status-code
   #:http-response.entity-headers
   #:http-response.stream
   #:http-response.add-cookie
   #:http-response.add-entity-header
   #:http-response.add-response-header
   #:http-response.set-content-type
;;; helpers
   #:escape-parenscript
   ;; Http Request
   #:http-accept?
   #:http-accept-charset?
   #:http-accept-encoding?
   #:http-accept-language?
   #:http-authorization?
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
   #:http-proxy-authorization?
   #:http-range?
   #:http-referer?
   #:http-te?
   #:http-user-agent?
   #:http-response!
   ;; HTTP Response
   #:http-accept-ranges!
   #:http-age!
   #:http-etag!
   #:http-location!
   #:http-proxy-authenticate!
   #:http-retry-after!
   #:http-server!
   #:http-vary!
   #:http-www-authenticate!
   ;; HTTP General Headers
   #:http-cache-control?
   #:http-cache-control!
   #:http-connection?
   #:http-connection!
   #:http-date?
   #:http-date!
   #:http-pragma?
   #:http-pragma!
   #:http-trailer?
   #:http-trailer!
   #:http-transfer-encoding?
   #:http-transfer-encoding!
   #:http-upgrade?
   #:http-upgrade!
   #:http-via?
   #:http-via!
   #:http-warning?
   #:http-warning!
   ;; HTTP Request methods
   #:OPTIONS
   #:GET
   #:HEAD
   #:POST
   #:PUT
   #:DELETE
   #:TRACE
   #:CONNECT
   ;; Cache Request Directives
   #:NO-CACHE
   #:NO-STORE
   #:MAX-AGE
   #:MAX-STALE
   #:MIN-FRESH
   #:NO-TRANSFORM
   #:ONLY-IF-CACHED
   ;; Cache Response Directives
   #:PUBLIC
   #:PRIVATE
   #:NO-CACHE
   #:NO-STORE
   #:NO-TRANSFORM
   #:MUST-REVALIDATE
   #:PROXY-REVALIDATE
   #:MAX-AGE
   #:S-MAXAGE
   ;; General Headers
   #:CACHE-CONTROL
   #:CONNECTION
   #:DATE
   #:PRAGMA
   #:TRAILER
   #:TRANSFER-ENCODING
   #:UPGRADE
   #:VIA
   #:WARNING
   ;; Request Headers
   #:ACCEPT
   #:ACCEPT-CHARSET
   #:ACCEPT-ENCODING
   #:ACCEPT-LANGUAGE
   #:AUTHORIZATION
   #:EXPECT
   #:100-CONTINUE
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
   ;; Response Headers
   #:ACCEPT-RANGES
   #:AGE
   #:ETAG
   #:LOCATION
   #:PROXY-AUTHENTICATE
   #:RETRY-AFTER
   #:SERVER
   #:VARY
   #:WWW-AUTHENTICATE
   ;; Entity Headers
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
   ;; Browser Symbols
   #:BROWSER
   #:VERSION
   #:OPERA
   #:MOZ-VER
   #:OS
   #:REVISION
   #:IE
   #:SEAMONKEY
   #:LANG
   ;; 
   ;; [Protocol]
   ;; Classes
   #:application
   #:web-application
   #:server
   #:web-server
   ;; Accessors
   #:application.server
   #:application.debug
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
   ;; Classes
   #:apache-server
   #:apache-web-application
   ;; Accessors
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

   ;; [Logger]
   #:logger-server
   #:log-me
   #:log-me-raw

   ;; [Database]
   ;; Classes
   #:database-server
   #:database

   ;; Interface
   #:serialization-cache
   #:xml-serialize
   #:xml-deserialize
   #:transaction
   #:with-transaction
   #:deftransaction
   #:execute
   #:snapshot
   #:database.root
   #:database.get
   #:database.serialize
   #:database.deserialize
   #:log-transaction
   
   ;; Object Database
   #:object-with-id
   #:get-id
   #:find-all-objects
   #:find-object-with-slot
   #:find-object-with-id
   #:update-object
   #:add-object
   #:delete-object
   #:next-id
   
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
   ;; db utils
;;;    #:make-tx
;;;    #:update-slots
   #:defcrud
   #:redefmethod
   ;; [Nameserver]
   ;; Classes
   #:name-server
   #:ns-model
   #:ns-mx
   #:ns-alias
   #:ns-ns
   #:ns-host
   ;; Accessors
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
   ;; Classes
   #:email-server
   #:postfix-server
   ;; API
   #:add-email
   #:del-email

   ;; [Ticket]
   ;; Classes
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
   ;; Classes
   #:core-server
   #:core-web-server
   #:*core-server*
   #:*core-web-server*
   ;; [Whois]
   ;; API
   #:whois
   ;; Helpers
   #:reduce0
   #:filter
   #:uniq
   #:prepend
   #:make-keyword
   #:with-current-directory
   #:make-project-path
   #:with-current-directory
   #:time->string
   #:+day-names+
   #:+month-names+
   #:take
   #:drop
   #:flatten
   #:flatten1
   
   ;; [Serializable Application]
   #:serializable-web-application
   ;; [Darcs Web Application]
   ;; Classes
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
   ;; [HTTP Server]
   #:find-application
   #:register
   #:unregister
   #:server.applications
   
   ;; [HTTP Application & Web Framework]

   ;; Constants
   #:+continuation-query-name+
   #:+session-query-name+
   #:+context+

   ;; Session
   #:http-session
   #:session.id
   #:session.continuations
   #:session.timestamp
   #:session.data
   #:make-new-session
   #:find-session-id
   #:find-continuation
   #:update-session
   #:query-session

   ;; Context
   #:http-context
   #:context.request
   #:context.response
   #:context.session
   #:context.application

   ;; Metaclass
   #:http-application+
   #:http-application+.handlers
   #:add-handler
   #:remove-handler
   
   ;; Class
   #:http-application
   #:http-application.sessions
   #:defapplication
   #:find-session
   #:render-404
   #:render-file
   #:dispatch

   ;; Macros
   #:with-query
   #:with-context
   #:defhandler
   #:defhandler/js
   #:defurl

   ;; CPS Style Web Framework
   #:send/suspend
   #:send/forward
   #:send/finish
   #:action/hash
   #:function/hash
   #:action/url
   #:function/url
   #:answer
   #:answer/dispatch
   #:answer/url
   #:javascript/suspend
   #:json/suspend
   #:xml/suspend
   #:css/suspend

   ;; Test Utilties
   #:with-test-context
   #:kontinue
   #:test-url
      
   ;; [HTTP Component Framework]
   #:component
   #:component.application
   #:component.javascript-reader
   #:component.javascript-writer
   #:component.remote-method-proxy
   #:component.local-method-proxy
   #:component.remote-slots
   #:component.local-slots
   #:component.remote-methods
   #:component.remote-ctor-arguments
   #:defcomponent-ctor
   #:defcomponent
   #:defmethod/local
   #:defmethod/remote
   #:component!
   #:funkall
   #:funkall/js
   #:to-json
   #:to-json/js 
   #:html-component
   #:defhtml-component
   #:defcomponent-accessors
   #:mtor!
   #:deftable
   #:defwebcrud

   ;; [ Web Component Stacks ]
   #:dojo
   #:jquery

   ;; [ Json ]
   #:json!
   #:json?
   #:json-serialize
   #:json-deserialize
   #:jobject
   #:object->jobject

   ;; [ Tags ]
   #:input

   ;; [[validating input]]
   #:get-default-value/js
   #:set-default-value/js
   #:adjust-default-value/js
   #:onfocus/js
   #:onblur/js

   #:init/js
   
   ;; [DOm Components]
   #:dom-element
   #:css-class
   #:tag
   #:id
   #:div-element
   ;; [HTTP Components]
   #:fckeditor-component
   #:toaster-component
   #:login-component
   #:feedback-component
   #:hedee-component
   #:make-hedee
   #:hilighter
   ;; [Helpers]
   #:make-keyword
   #:make-unique-random-string
   ;; [Search]
   #:core-search
   #:string-search
   #:integer-search
   ;; [Mail-Sender]
   #:mail-sender
   #:mail-sender.from
   #:mail-sender.server
   #:mail-sender.port
   #:sendmail
   #:make-mail
   ;; [Filesystem]
   #:filesystem
   #:filesystem.label
   #:filesystem.root
   #:readfile
   #:writefile
   #:ls
   ;; [Parser]
   #:string!
   #:char!
   #:fixnum!
   #:quoted-printable!
   #:make-accumulator
   ;; The server itself
   *server*
   ;; Form component (which emails a filled form)
   #:web-form-component
   ;; socialshare
   #:socialshare-component
   ;; [ Core Commands ]
   #:defcommand
   #:command
   #:command.output-stream
   #:command.input-stream
   #:command.verbose
   #:command.verbose-stream
   #:command.local-args
   #:command.remote-args
   #:shell
   #:whereis
   #:thumbnail
   ;; [ Core Parser ]
   #:defrule
   #:defparser
   #:defrender
   ))

(defpackage :tr.gen.core.server.io
  (:nicknames :io)
  (:use :cl :core-server :cffi))

(defpackage :tr.gen.core.server.html
  (:nicknames :< :core-server.html)
  (:use :core-server)
  (:export #:a #:abbr #:acronym #:address #:area #:b #:base #:bdo #:big
	   #:blockquote #:body #:br #:button #:caption #:cite #:code #:col
	   #:colgroup #:dd #:del #:dfn #:div #:dl #:dt #:em #:fieldset #:form
	   #:frame #:frameset #:h1 #:h2 #:h3 #:h4 #:h5 #:h6 #:head #:hr #:html
	   #:i #:iframe #:img #:input #:ins #:kbd #:label #:legend #:li #:link
	   #:map #:meta #:noframes #:noscript #:object #:ol #:optgroup #:option
	   #:p #:param #:pre #:q #:samp #:script #:select #:small #:span #:strong
	   #:style #:sub #:sup #:table #:tbody #:td #:textarea #:tfoot #:th #:thead
	   #:title #:tr #:tt #:ul #:var #:embed #:foo #:bar))

(defpackage :tr.gen.core.server.rss
  (:nicknames :<rss :core-server.rss)
  (:use :core-server))

(defpackage :tr.gen.core.server.tags
  (:nicknames :<core)
  (:use :cl)
  (:export #:input #:redirect #:table
	   #:validating-input #:default-value-input
	   #:domain-input #:email-input #:password-input
	   #:username-input #:tab #:crud
	   #:auth #:core))

;; (defpackage :tr.gen.core.server.html.dojo
;;   (:nicknames :<dojo :core-server.html.dojo)
;;   (:use :core-server :<))

;; (defpackage :tr.gen.core.server.html.dijit
;;   (:nicknames :<dijit :core-server.html.dijit)
;;   (:use :core-server :<))

;; (defpackage :tr.gen.core.server.html.dijit.layout
;;   (:nicknames :<dijit.layout :core-server.html.dijit.layout)
;;   (:use :core-server :<))