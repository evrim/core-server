(in-package :cl-user)

(defpackage :tr.gen.core.server.test
  (:nicknames :core-server.test)
  (:use :common-lisp :iterate :cl-prevalence :core-server :5am :arnesi
	;; :cl-store
	)
  (:shadowing-import-from #:arnesi #:name))