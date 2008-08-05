;; Hello World Web Example
;;
;; load the file with C-c C-l and visit, 
;; http://localhost:8080/hello

;; defining a new namespace
(defpackage :hello
  (:use :cl :core-server :arnesi))

;; Switch to new namespace
(in-package :hello)

;; Create application object
(defparameter *hello*
  (make-instance 'http-application
		 :fqdn "localhost"
		 :admin-email "aycan@core.gen.tr"))

;; Our page is a function which gets body as a parameter
(defun/cc page (body)
  (<:html
   (<:body
    body)))

;; Create a handler
(defurl *hello* "hello" ()
  (page
   (<:p "Hello, World!")))

;; Register application
(register *server* *hello*)
