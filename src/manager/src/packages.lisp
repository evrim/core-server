(in-package :cl-user)

(defpackage :tr.gen.core.manager
  (:nicknames :manager)
  (:use :common-lisp :core-server :cl-prevalence :arnesi)
  (:shadowing-import-from #:cl-prevalence #:name)
  (:shadowing-import-from #:arnesi #:new))

