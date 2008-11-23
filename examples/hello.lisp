;; Hello World Web Example
;;
;; load the file with C-c C-l and visit, 
;; http://localhost:8080/hello

;; defining a new namespace
(defpackage :hello
  (:use :cl :core-server :arnesi))

;; Switch to new namespace
(in-package :hello)

(defapplication hello-app (http-application)
  ()
  (:default-initargs :fqdn "localhost"
    :admin-email "aycan@core.gen.tr"))

;; Create application object
(defparameter *hello*
  (make-instance 'hello-app))

;; Our page is a function which gets body as a parameter
(defun/cc page (body)
  (<:html
   (<:body
    body)))

;; Create a handler
(defhandler "hello" ((self hello-app))
  (page
   (<:p "Hello, World!")))

;; Register application
(register *server* *hello*)
