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
  (with-html-output (core-server::http-response.stream (core-server::response +context+))
    "Hello, World!"))

(defun/cc template (body)
  (with-html-output (core-server::http-response.stream (core-server::response +context+))
    (<:html
     (<:body
      body))))

(defurl *hello* "guestbook" ()
  (let ((message (send/suspend
		   (template
		    (<:form :action (action/url ((text "text"))
				      (answer text))
			    :method "POST"
			    (<:div (<:textarea :rows "3" :cols "60" :name "text" ""))
			    (<:div (<:input :type "submit" :value "Sign Guestbook")))))))
    (template message)))

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