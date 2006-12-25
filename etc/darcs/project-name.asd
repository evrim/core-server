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
                         (:file "project-name" :depends-on ("model" "packages")))))
  :depends-on (:ucw+ :core-server)
  :serial t)
