;;;; -*- Mode: lisp; indent-tabs-mode: nil; outline-regexp: ";;;;;*"; -*-
(in-package :asdf)

(defsystem :project-name
  :description ""
  :version "0"
  :author "admin-email"
  :maintainer "admin-email"
  :licence "LGPLv2"
  :components ((:static-file "project-name.asd") 
               (:module :src
                        :serial t
                        :components
                        ((:file "packages")
			 (:file "model" :depends-on ("packages"))
                         (:file "project-name" :depends-on ("packages" "model"))
			 (:file "transactions" :depends-on ("packages" "model"))
			 (:file "interfaces" :depends-on ("transactions"))
			 (:module :ui
                                  :serial t
                                  :components
                                  ((:file "main"))))))
  :depends-on (:iterate :core-server)
  :serial t)

(defsystem :project-name.test
  :components ((:module :t
			:components
			((:file "packages"))))
  :depends-on (:core-server :FiveAM))

(defmethod perform ((op asdf:test-op) (system (eql (find-system :project-name))))
  (asdf:oos 'asdf:load-op :project-name.test)
  (funcall (intern (string :run!) (string :it.bese.FiveAM)) :project-name))