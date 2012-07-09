;; -------------------------------------------------------------------------
;; [Core-serveR] Web Sum Example
;; -------------------------------------------------------------------------
;; Load the file with C-c C-l and visit, 
;; http://localhost:8080/websum/

;; -------------------------------------------------------------------------
;; Define a new namespace
;; -------------------------------------------------------------------------
(defpackage :websum
  (:use :cl :core-server :arnesi))

;; -------------------------------------------------------------------------
;; Switch to new namespace
;; -------------------------------------------------------------------------
(in-package :websum)

;; -------------------------------------------------------------------------
;; Web Sum Application Definition
;; -------------------------------------------------------------------------
(defapplication websum-application (http-application)
  ()
  (:default-initargs :fqdn "websum" :admin-email "aycan@core.gen.tr"))

;; -------------------------------------------------------------------------
;; Define 'page' function which gets body as a parameter
;; -------------------------------------------------------------------------
(defun/cc page (body)
  (<:html (<:head
	   (<:meta :http--equiv "Content-Type" :content "text/html; charset=utf-8")
	   (<:title "[Core-serveR] - Web Sum Example"))
	  (<:body
	   (<:h1 "[Core-serveR] - Web Sum Example")
	   body)))

;; -------------------------------------------------------------------------
;; Display the result
;; -------------------------------------------------------------------------
(defun/cc web-display (num)
  (send/suspend
    (page
     (<:div
      (<:p (format nil "The result is ~D" num))
      (<:a :href "index" "Restart")))))

;; -------------------------------------------------------------------------
;; Read an integer
;; -------------------------------------------------------------------------
(defun/cc web-read (msg)
  (send/suspend
    (page
     (<:form :method "POST"
	     :action (action/url ((num "num"))
		       (answer (parse-integer num :junk-allowed t)))
	     (<:p msg)
	     (<:input :type "text" :name "num")
	     (<:input :type "submit" :value "Next")))))

;; -------------------------------------------------------------------------
;; Register a handler
;; -------------------------------------------------------------------------
(defhandler "index" ((self websum-application))
  (web-display (+ (web-read "Enter first number to add:")
		  (web-read "Enter a second Number:"))))

;; -------------------------------------------------------------------------
;; Create an application instance
;; -------------------------------------------------------------------------
(defparameter *websum* (make-instance 'websum-application))

;; -------------------------------------------------------------------------
;; Register our application to the server
;; -------------------------------------------------------------------------
(register *server* *websum*)

;; EoF
