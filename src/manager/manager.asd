(in-package :asdf)

;; -------------------------------------------------------------------------
;; Manager ASDefinition
;; Author: Evrim Ulu <evrim@core.gen.tr>
;; Date: 2011
;; -------------------------------------------------------------------------
(defsystem :manager
  :description "core template application"
  :version "1"
  :author "Evrim Ulu <evrim@core.gen.tr>"
  :maintainer "Evrim Ulu <evrim@core.gen.tr>"
  :licence "lgpl v2"
  :components
  ((:static-file "manager.asd")
   (:module :src
	    :serial t
	    :components
	    ((:file "packages")
	     (:file "model" :depends-on ("packages"))
	     (:file "application" :depends-on ("packages" "model"))
	     (:module :auth
		      :serial t
		      :components
		      ((:file "interface")
		       (:file "login")
		       (:file "accounts")
		       (:file "controller")))
	     (:module :api
		      :serial t
		      :components
		      ((:file "markup")
		       (:file "api")))
	     (:file "dynamic" :depends-on ("application"))
	     (:module :mixin
		      :serial t
		      :components
		      ((:file "application")
		       (:file "plugin")
		       (:file "widget")))
	     (:module :ui
	     	      :serial t
	     	      :components
	     	      ((:file "info")
		       (:file "server")
		       (:module :servers
				:serial t
				:components
				((:file "socket")
				 (:file "database")
				 (:file "mail-sender")))
		       (:module :applications
				:serial t
				:components
				((:file "web")
				 (:file "dynamic")
				 (:file "http")))
		       (:file "application")
		       (:file "settings")
		       ;; (:file "sites")
	     	       (:file "admin")
		       (:file "controller")))
	     (:file "init"))))
  :depends-on (:arnesi+ :core-server)
  :serial t)

(defsystem :manager.test
  :components
  ((:module :t
	    :components ((:file "packages"))))
  :depends-on (:manager :core-server :rt))

(defmethod perform ((op asdf:test-op)
		    (system (eql (asdf:find-system :manager))))
  (asdf:oos 'asdf:load-op :manager.test))

