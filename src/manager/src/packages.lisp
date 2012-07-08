(in-package :cl-user)

(defpackage :tr.gen.core.manager
  (:nicknames :manager)
  (:use :common-lisp :core-server :arnesi))


(defpackage :tr.gen.core.manager.widget
  (:nicknames :<manager)
  (:export #:settings))