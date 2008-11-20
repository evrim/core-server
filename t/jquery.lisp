(in-package :core-server.test)

(defpackage :jquery.test
  (:use :common-lisp :core-server :jquery))

(in-package :jquery.test)

(defclass jquery-test-application (database-server apache-web-application http-application)
  ()
  (:default-initargs :directory #P"/tmp/jquery-test/"
		     :fqdn "jquery"
		     :admin-email "evrim@core.gen.tr"
		     :auto-start t))

(defparameter *app* (make-instance 'jquery-test-application))

(register *server* *app*)

(defcomponent jquery-table (jquery)
  ())

(defurl *app* "index.core" ()
  (<:html
   (<:head)
   (<:body
    (<:))))