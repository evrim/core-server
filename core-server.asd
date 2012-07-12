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

;;;; -*- Mode: lisp; indent-tabs-mode: nil; outline-regexp: ";;;;;*"; -*-
(in-package :asdf)

;; CFFI-Grovel is needed
(cl:eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op 'cffi-grovel))

;; Add distribution based features
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :core-server *features*)
  (cond
    ((probe-file "/etc/pardus-release")
     (pushnew :pardus *features*))
    ((probe-file "/etc/gentoo-release")
     (pushnew :gentoo *features*))
    ((probe-file "/etc/debian_version")
     (pushnew :debian *features*))))

(defsystem :core-server
  :description "Core Server"
  :version "0.1"
  :author "Evrim Ulu <evrim@core.gen.tr>"
  :licence "MIT License"
  :components ((:static-file "core-server.asd")
	       (:module :src
                        :serial t
                        :components
			((:file "core-server")
			 (:file "bootstrap")
                         (:module :util
                                  :serial t
                                  :components
                                  ((:file "helper")
                                   (:file "turkiye")
                                   (:file "mop")))
			 (:module :class+
				  :serial t
				  :components
				  ((:file "protocol")
				   (:file "class+")
				   (:file "lift")
				   (:file "command")))
                         (:module :compat
                                  :serial t
                                  :components
                                  ((:file "sockets")
                                   (:file "threads")
				   ;; (:file "prevalence")
				   ))
			 (:module :units
                                  :serial t
                                  :components
                                  ((:file "units")))
;;; 			 (:module :gss
;;; 				  :serial t
;;; 				  :components
;;; 				  ((cffi-grovel:grovel-file "grovel")
;;; 				   (:file "gss")
;;; 				   (:file "interface")))
;;                          (:module :io
;; 				  :serial t
;; 				  :components
;; 				  ((cffi-grovel:grovel-file "grovel") 
;; 				   (:file "errno")
;; 				   (:file "bsd")
;;                                    (:file "uuid")
;; 				   (:file "interface")
;; 				   (:file "events")
;; 				   ;; (:module :libevent
;; ;; 					    :serial t
;; ;; 					    :components
;; ;; 					    ((:file "libevent-lib")
;; ;; 					     (cffi-grovel:grovel-file "grovel")
;; ;; 					     (:file "libevent")))
;; 				   (:module :libev
;; 				   	    :serial t
;; 				   	    :components
;; 				   	    ((:file "libev-lib")
;; 				   	     (cffi-grovel:grovel-file "grovel")
;; 				   	     (:file "libev") 
;; 				   	     (:file "interface")))
;; 				   ))
			 (:module :lisp
				  :serial t
				  :components
				  ((:file "successors")
				   (:file "cps")))
                         (:module :streams
                                  :serial t
                                  :components
                                  ((:file "streams")
				   (:file "faster")
                                   (:file "grammar")
				   (:file "util")
                                   (:file "parser")
                                   (:file "render")))			 
                         (:module :commands
                                  :serial t
                                  :components
                                  ((:file "shell")
				   (:file "admin")
				   (:file "scm")
                                   ;; (:file "hxpath")
				   (:file "image")
				   (:file "http")
				   (:file "facebook")
				   (:file "openid")
				   (:file "google")
				   (:file "paypal")
				   (:file "twitter")))
			 (:module :install
				  :serial t
				  :components
				  ((:file "install")))
			 (:file "vars")
                         ;; (:file "classes")
                         (:file "protocol")
                         (:file "application")
                         (:file "server")
			 (:file "peer")
			 (:module :javascript
				  :serial t
				  :components
				  ((:file "util")
				   (:file "render")
				   (:file "transform")
				   (:file "interface")
				   (:file "cps")
				   (:file "macro")
				   (:file "library")))
                         (:module :rfc
                                  :serial t
                                  :components
                                  ((:file "2109") ;;cookie
                                   (:file "2396") ;;uri
                                   (:file "2822") ;;text messages
                                   (:file "2617") ;;http auth
                                   (:file "2616") ;;http
                                   (:file "2045") ;;mime-part1
                                   (:file "2046") ;;mime-part2
                                   (:file "2388") ;;multpart/form-data
				   (:file "2821"))) ;;smtp
			 (:module :markup
				  :serial t
				  :components
				  ((:file "xml")				   
				   (:file "dom")
				   (:file "html")
				   (:file "schema")
				   (:file "wsdl")
				   (:file "css")
				   ;; (:file "json")
				   (:file "core-server")
				   ;; (:file "rss")
				   (:file "atom")
				   (:file "gphoto")
				   (:file "media")
				   (:file "open-search")
				   (:file "wordpress")
				   (:file "content")
				   (:file "dc")
				   (:file "excerpt")))
			 (:module :database
				  :serial t
				  :components
				  ((:file "serialize")
				   (:file "database")
				   (:file "object")
				   (:file "crud")))
                         (:module :applications
                                  :components
                                  ((:file "web")
				   (:file "mail")
				   (:file "dns")
				   (:file "serializable-application")
                                   (:file "darcs" :depends-on ("serializable-application"))
                                   (:file "git" :depends-on ("serializable-application"))
				   (:file "apache" :depends-on ("web"))
                                   (:file "http")
				   (:file "persistent")
				   (:file "postfix" :depends-on ("mail"))))
                         (:module :servers
                                  :components
                                  ((:file "tinydns")
                                   (:file "apache")
				   (:file "mail")
                                   (:file "postfix" :depends-on ("mail"))
                                   (:file "logger")
                                   ;; (:file "ticket")
                                   (:file "socket")
                                   (:file "http" :depends-on ("socket" "logger"))
				   (:file "persistent-http" :depends-on ("http"))))
                         (:module :services
                                  :components
                                  ((:file "whois")
                                   (:file "mail")
                                   (:file "filesystem")))
			 (:module :security
				  :components
				  ((:file "class")
				   (:file "authorize")
				   (:file "authenticate"))
				  :serial t)
                         (:module :web
                                  :serial t
                                  :components
                                  ((:file "json")
				   (:file "mime-types")
				   (:file "tags")
				   (:file "component")
				   (:file "rest")
				   (:file "jquery")
				   (:file "form")
				   (:file "dialog")
				   (:file "extra")
				   (:file "auth")
				   (:file "representations")
				   (:file "table")
				   (:file "crud")
				   (:file "table-with-crud")
				   (:file "tab")
				   (:file "image")
				   (:file "video")
				   (:file "editor")
				   (:file "dojo")
				   (:file "picasa")))
			 (:module :coretal
				  :serial t
				  :components
				  ((:file "map")
				   (:file "page")
				   (:file "widget")
				   (:file "controller")))
			 (:module :widget
				  :serial t
				  :components
				  ((:file "menu")
				   (:file "content")
				   (:file "tab")))
			 (:file "quicklisp"))))
  
  :depends-on (:swank :bordeaux-threads ;; :cl-prevalence
		      :sb-bsd-sockets :arnesi+ :cl-ppcre :cl-fad :cffi
		      :salza2 :ironclad)
  :serial t)

(defmethod perform :after ((o load-op) (c (eql (find-system :core-server))))
  (in-package :core-server))

(defsystem :core-server.examples
  :components ((:module :examples
			:serial t
			:components ((:file "hello")
				     (:file "websum")
				     (:file "quiz")))))

(defsystem :core-server.test
  :components ((:module :t
                        :serial t
			:components ((:file "packages")
                                     (:file "test-harness")
				     (:file "method")
				     (:file "class+")
                                     (:file "postfix")
                                     (:file "parser")
                                     (:file "streams")
                                     (:file "sockets")
				     (:file "markup")
                                     (:file "json")
                                     (:file "units")
                                     (:module :rfc
                                              :components ((:file "2109")
                                                           (:file "2396")
                                                           (:file "2045")
                                                           (:file "2388")
							   (:file "2616"))
                                              :serial t)
				     (:module :database
					      :components ((:file "serialization")
							   (:file "database")))
                                     ;; (:file "database")
                                     ;; (:file "dns")
                                     ;; (:file "apache")
                                     ;; (:file "ucw")
                                     ;; (:file "core")
				     (:file "javascript")
				     (:file "component"))))
  :depends-on (:core-server :rt))



;; (defmethod perform ((op asdf::load-op) (system (eql (find-system :core-server.test))))
;;   (core-server::with-package (find-package :core-server.test)
;;     (rt:do-tests)))

;; (defmethod perform ((op asdf:test-op) (system (eql (find-system :core-server))))
;;   (asdf:oos 'asdf:load-op :core-server.test)
;;   (funcall (intern (string :run!) (string :it.bese.FiveAM)) :core-server))
