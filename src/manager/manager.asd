(in-package :asdf)

(asdf:defsystem :manager :description "core template application" :version ".1"
                :author "root@localhost" :maintainer "root@localhost" :licence
                "lgpl v2" :components
                ((:static-file "manager.asd")
                 (:module :src :serial t :components
                  ((:file "packages") (:file "model" :depends-on ("packages"))
                   (:file "tx" :depends-on ("model"))
                   (:file "application" :depends-on ("packages" "model" "tx"))
                   (:file "interfaces" :depends-on ("application"))
                   (:module :ui :serial t :components ((:file "main"))))))
                :depends-on (:arnesi :core-server) :serial t)

(asdf:defsystem :manager.test :components
                ((:module :t :components ((:file "packages")))) :depends-on
                (:manager :core-server :rt))

(defmethod perform
           ((op asdf:test-op) (system (eql (asdf:find-system :manager))))
           (asdf:oos 'asdf:load-op :manager.test))

