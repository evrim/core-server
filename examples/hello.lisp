;; a new namespace
(defpackage :hello
  (:use :cl :core-server :arnesi))

;; switch to new namespace
(in-package :hello)

;; create application object
(defparameter *hello*
  (make-instance 'http-application
		 :fqdn "localhost"
		 :admin-email "aycan@core.gen.tr"))

;; create a handler
(defurl *hello* "hello" ()
  (template
   (<:p "Hello, World!")))

(defun/cc template (body)
  (<:html
   (<:body
    body)))

;; register application
(register *server* *hello*)

;; load the file with C-c C-l and visit http://localhost:8080/hello

;; TODO:
;;
;; - We need to simplify importing other packages, why we need arnesi?
;; - or js?
;;
;; - we need to simplify html-output and introduce more content types
;;   like text/plain etc..
;;
;; - 