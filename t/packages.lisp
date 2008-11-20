(in-package :cl-user)

(defpackage :tr.gen.core.server.test
  (:nicknames :core-server.test)
  (:use :common-lisp ;; :cl-prevalence
	:core-server :arnesi :rt
	;; :cl-store :5am :iterate
	)
  (:shadowing-import-from #:arnesi #:name))

;;(in-package :core-server.test)
;;(def-suite :core-server)