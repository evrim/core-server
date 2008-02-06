;;;; -*- Mode: lisp; indent-tabs-mode: nil; outline-regexp: ";;;;;*"; -*-
(in-package :asdf)

;; Add distribution based features
(eval-when (:compile-toplevel :load-toplevel :execute)
  (cond
    ((probe-file "/etc/pardus-release")
     (pushnew :pardus *features*))
    ((probe-file "/etc/gentoo-release")
     (pushnew :gentoo *features*))
    ((probe-file "/etc/debian_version")
     (pushnew :debian *features*))))

(defsystem :core-server
  :description "Core Server"
  :version "0"
  :author "Evrim Ulu <evrim@core.gen.tr>"
  :licence "MIT License"
  :components ((:static-file "core-server.asd")
	       (:module :src
                        :serial t
                        :components
			((:file "packages")
                         (:file "helper")
                         (:file "sockets")
                         (:file "threads")
                         (:file "units")
                         (:file "streams")
                         (:file "parser")
                         (:file "prevalence")
                         (:file "search")
                         (:file "mop")
                         (:file "command")
                         (:file "config")
                         (:file "method")
                         (:file "classes")
                         (:file "protocol")
                         (:file "application")
                         (:file "server")
                         (:file "parenscript")
                         (:file "html")
                         (:module :commands
                                  :serial t
                                  :components
                                  ((:file "hxpath")))
                         (:module :rfc
                                  :serial t
                                  :components
                                  ((:file "2109") ;;cookie
                                   (:file "2396") ;;uri
                                   (:file "822")  ;;text messages
                                   (:file "2617") ;;http auth
                                   (:file "2616") ;;http
                                   (:file "2045") ;;mime-part1
                                   (:file "2046") ;;mime-part2
                                   (:file "2388") ;;multpart/form-data
                                   )) ;;http
                         (:module :applications
                                  :components
                                  (;; (:file "ucw")
                                   (:file "serializable-application")
                                   (:file "darcs")
                                   (:file "git")
                                   (:file "http")))
                         (:module :peers
                                  :components
                                  ((:file "peer")
                                   (:file "http")))
                         (:module :servers
                                  :components
                                  ((:file "database")
                                   (:file "tinydns")
                                   (:file "apache")
                                   (:file "postfix")
                                   (:file "logger")
                                   (:file "core")
                                   (:file "ticket")
                                   (:file "socket")
                                   (:file "http")))
                         (:module :services
                                  :components
                                  ((:file "whois")
                                   (:file "mail")
                                   (:file "filesystem")))
                         (:module :web
                                  :serial t
                                  :components
                                  ((:file "macros")
                                   (:file "json")
                                   (:file "component")
                                   (:module :components
                                            :components
                                            ((:file "toaster")
                                             (:file "fckeditor")
                                             (:file "login")
                                             (:file "feedback"))))))))
  :depends-on (:bordeaux-threads :cl-prevalence :sb-bsd-sockets
                                 :arnesi :cl-ppcre :cl-fad :parenscript)
  :serial t)

(defmethod perform :after ((o load-op) (c (eql (find-system :core-server))))
  (in-package :core-server))

(defsystem :core-server.test
  :components ((:module :t
                        :serial t
			:components ((:file "packages")
                                     (:file "test-harness")
                                     (:file "postfix")
                                     (:file "parser")
                                     (:file "streams")
                                     (:file "sockets")
                                     (:file "json")
                                     (:file "units")
                                     (:module :rfc
                                              :components ((:file "2109")
                                                           (:file "2396")
                                                           (:file "2045")
                                                           (:file "2388")
							   (:file "2616"))
                                              :serial t)
                                     ;; (:file "database")
                                     ;; (:file "dns")
                                     ;; (:file "apache")
                                     ;; (:file "ucw")
                                     ;; (:file "core")
                                     )))
  :depends-on (:core-server :rt))

;; (defmethod perform ((op asdf:test-op) (system (eql (find-system :core-server))))
;;   (asdf:oos 'asdf:load-op :core-server.test)
;;   (funcall (intern (string :run!) (string :it.bese.FiveAM)) :core-server))
