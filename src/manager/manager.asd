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
	     (:module :ui
		      :serial t
		      :components
		      ((:file "sites")
		       (:file "users")
		       (:file "main"))))))
  :depends-on (:arnesi :core-server)
  :serial t)

(defsystem :manager.test
  :components
  ((:module :t
	    :components ((:file "packages"))))
  :depends-on (:manager :core-server :rt))

(defmethod perform ((op asdf:test-op)
		    (system (eql (asdf:find-system :manager))))
  (asdf:oos 'asdf:load-op :manager.test))

