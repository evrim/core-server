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
                         (:file "streams")
                         (:file "parser")
                         (:file "sockets")
                         (:file "threads")
                         (:file "units")
                         (:file "prevalence")
                         (:file "config")
                         (:file "method")
                         (:file "classes")
                         (:file "protocol")
                         (:file "application")
                         (:file "server")
                         (:file "search")
                         (:module :applications
                                  :components
                                  ((:file "ucw")
                                   (:file "serializable-application")
                                   (:file "darcs")
                                   (:file "git")))
                         (:module :servers
                                  :components
                                  ((:file "database")
                                   (:file "dns")
                                   (:file "apache")
                                   (:file "postfix")
                                   (:file "ucw")
                                   (:file "core")
                                   (:file "ticket")
                                   (:file "socket")
                                   ;; (:module :web
;;                                             :components
;;                                             ((:module :backend
;;                                                        :components
;;                                                        ((:file "accept")
;;                                                         (:file "common")
;;                                                         (:file "httpd")
;;                                                         (:file "multithread-httpd")
;;                                                         (:file "mod-lisp")))))
                                   ))
                         (:module :rfc
                                  :serial t
                                  :components 
                                  ((:file "2109") ;;cookie
                                   (:file "2396") ;;uri
                                   (:file "2388") ;;mime
                                   (:file "2616")
                                   )) ;;http
                         (:module :peers
                                  :components
                                  ((:file "peer")
                                   (:file "http")
                                   ))
                         (:module :services
                                  :components
                                  ((:file "whois"))))))
  :depends-on (:iterate :cl-prevalence :ucw :sb-bsd-sockets)
  :serial t)

(defsystem :core-server.test
  :components ((:module :t
                        :serial t
			:components ((:file "packages")
                                     (:file "postfix")
                                     (:module :rfc
                                              :components ((:file "2109"))
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