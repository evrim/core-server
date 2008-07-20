;; Websum example
;; http://localhost:8080/websum/sumtwo

(defpackage :websum
  (:use :cl :core-server :arnesi))

(in-package :websum)

;; Create an application
(defparameter *websum-app*
  (make-instance 'http-application
		 :fqdn "websum"
		 :admin-email "aycan@core.gen.tr"))

;; Render a page
(defun/cc page (body)
  (<:html
   (<:head
    (<:meta :http--equiv "Content-Type" :content "text/html; charset=utf-8"))
   (<:body
    body)))

;; Display the result
(defun/cc web-display (num)
  (page
   (<:div
    (<:p (format nil "The sum is ~D" num))
    (<:a :href "sumtwo" "Restart"))))

;; Read an integer
(defun/cc web-read (msg)
  (send/suspend
    (page
     (<:form :method "POST"
	     :action (action/url ((num "num"))
		       (answer (parse-integer num :junk-allowed t)))
	     (<:p msg)
	     (<:input :type "text" :name "num")
	     (<:input :type "submit" :value "Next")))))

;; Register a handler
(defurl *websum-app* "sumtwo" ()
  (web-display (+ (web-read "First Num")
		  (web-read "Second Num"))))

;; Register our application to the server
(register *server* *websum-app*)