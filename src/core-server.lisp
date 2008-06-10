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
(defpackage :tr.gen.core.server
  (:nicknames :core-server)
  (:use :common-lisp :cl-prevalence :arnesi :cl-ppcre
	:sb-bsd-sockets :tr.gen.core.install :bordeaux-threads :js)
  (:shadowing-import-from #:swank #:send #:receive #:accept-connection)
  (:shadowing-import-from #:arnesi #:name #:body #:self #:new)
  (:import-from #:cl-prevalence #:get-directory)
  (:import-from #:arnesi #:fdefinition/cc)
  (:import-from #:sb-ext #:make-timer #:schedule-timer #:unschedule-timer #:timer)
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
   #:make-cps-stream
   ;; [Stream Helpers]
   #:with-core-stream
   #:with-core-stream/cc
   #:with-html-output
   ;; [Class]
   #:defclass+
   #:register-class
   #:register-remote-method-for-class
   #:register-local-method-for-class
   #:local-slots-of-class
   #:remote-slots-of-class
   #:default-initargs-of-class
   #:client-type-of-slot
   #:local-methods-of-class
   #:remote-methods-of-class
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
   #:http-response.response-headers
   #:http-response.status-code
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
   ;; 
   ;; [Protocol]
   ;; Classes
   #:application
   #:web-application
   #:server
   #:web-server
  
   ;; Accessors
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
   #:make-keyword
   #:with-current-directory
   #:make-project-path
   #:with-current-directory
   #:time->string
   #:+day-names+
   #:+month-names+
   #:take
   #:drop
   
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
   
   ;; [HTTP Application]
   #:http-application
   #:find-session
   #:find-continuation
   #:with-context ;; helper for defurl
   #:with-query	  ;; helper macro
   #:defurl
   #:register-url
   #:unregister-url
   #:make-dispatcher
   #:find-url
   #:http-session
   #:make-new-session
   #:http-context
   #:+context+
   #:+html-output+
   #:make-new-context
   #:copy-context
   #:session
   #:send/suspend
   #:javascript/suspend
   #:json/suspend
   #:xml/suspend
   #:function/hash
   #:action/hash
   #:function/url
   #:action/url
   #:answer
   #:dispatch
   #:kontinue
   #:test-url
   ;; [HTTP Component Framework]
   #:dojo
   #:defcomponent
   #:local
   #:remote
   #:both
   #:defmethod/local
   #:defmethod/remote
   #:send/component
   #:send/ctor
   #:+component-registry+
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
   ;; The server itself
   *server*
   ;; Javascript
   #:defjsmacr0
   #:js+
   #:true
   #:false
   #:undefined
   ;; Form component (which emails a filled form)
   #:web-form-component
   ;; socialshare
   #:socialshare-component
   ))

(defpackage :tr.gen.core.server.html
  (:nicknames :< :core-server.html)
  (:use :core-server)
  (:export #:ai #:ah #:js #:js+ #:a #:abbr #:acronym #:address #:area #:b
  #:base #:bdo #:big #:blockquote #:body #:br #:button #:caption
  #:cite #:code #:col #:colgroup #:dd #:del #:dfn #:div #:dl #:dt #:em 
  #:embed #:fieldset #:form #:frame #:frameset #:h1 #:h2 #:h3 #:h4
  #:h5 #:h6 #:head #:hr #:html #:i #:iframe #:img #:input #:ins #:kbd
  #:label #:legend #:li #:link #:map #:meta #:noframes #:noscript
  #:object #:ol #:optgroup #:option #:p #:param #:pre #:q #:samp
  #:script #:select #:small #:span #:strong #:style #:sub #:sup
  #:table #:tbody #:td #:textarea #:tfoot #:th #:thead #:title #:tr
  #:tt #:ul #:var))

(defpackage :tr.gen.core.server.html.dojo
  (:nicknames :<dojo :core-server.html.dojo)
  (:use :core-server :<))

(defpackage :tr.gen.core.server.html.dijit
  (:nicknames :<dijit :core-server.html.dijit)
  (:use :core-server :<))

(defpackage :tr.gen.core.server.html.dijit.layout
  (:nicknames :<dijit.layout :core-server.html.dijit.layout)
  (:use :core-server :<))