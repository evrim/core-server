;;;; -*- Mode: lisp; indent-tabs-mode: nil; outline-regexp: ";;;;;*"; -*-
(in-package :asdf)

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
                         (:file "config")
                         (:file "method")
                         (:file "classes")
                         (:file "protocol")
                         (:file "application")
                         (:file "server")
                         (:file "helper")
                         (:module :applications
                                  :components
                                  ((:file "ucw")
                                   (:file "darcs")))
                         (:module :servers
                                  :components
                                  ((:file "database")
                                   (:file "dns")
                                   (:file "apache")
                                   (:file "postfix")
                                   (:file "ucw")
                                   (:file "core")
                                   (:file "ticket")))
                         (:module :services
                                  :components
                                  ((:file "whois"))))))
  :depends-on (:iterate :cl-prevalence :ucw)
  :serial t)

(defsystem :core-server.test
  :components ((:module :t
			:components ((:file "packages")
                                     (:file "postfix")
                                     ;; (:file "database")
                                     ;; (:file "dns")
                                     ;; (:file "apache")
                                     ;; (:file "ucw")
                                     ;; (:file "core")
                                     )))
  :depends-on (:core-server :FiveAM))

;; Add distribution related features
(defmethod asdf:perform :around ((o asdf:load-op) (c (eql (find-system :core-server))))
  (cond
    ((probe-file "/etc/pardus-release")
     (push :pardus *features*))
    ((probe-file "/etc/gentoo-release")
     (push :gentoo *features*)))
  (call-next-method))

(defmethod perform ((op asdf:test-op) (system (eql (find-system :core-server))))
  (asdf:oos 'asdf:load-op :core-server.test)
  (funcall (intern (string :run!) (string :it.bese.FiveAM)) :core-server))