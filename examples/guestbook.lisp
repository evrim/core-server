;; A persistent guestbook web application 
;; Just compile it with C-c C-k or load it with C-c C-l and then visit
;; http://localhost:8080/guestbook/guestbook

(defpackage :guestbook
  (:use :cl :core-server :arnesi))

(in-package :guestbook)

;; a class for messages and a constructor
(defclass+ message ()
  ((sender)
   (subject)
   (text)
   (timestamp :initform (get-universal-time))))
(defcrud message)

(defclass guestbook-application (http-application database-server)
  ()
  (:default-initargs
   :fqdn "guestbook"
    :admin-email "john@doe.com"
    :directory #P"/tmp/guestbook/"
    :auto-start t))

(defparameter *guestbook* (make-instance 'guestbook-application))
(defparameter *text/css* ".message ul { list-style: none; margin:0; padding: 0 0 15px 0;}")

;; Our template function which gets body as a parameter
(defun/cc template (body)
  (<:html
   (<:head
    (<:meta :http--equiv "Content-Type" :content "text/html; charset=utf-8")
    (<:style :type "text/css" *text/css*))
   (<:body
    body)))

;; render a message as html
(defun/cc render-message (msg)
  (with-slots (sender subject text timestamp) msg
    (<:div :class "message"
	   (<:ul
	    (<:li (<:p sender ": " subject))
	    (<:li (<:p text))
	    (<:li (<:p (time->string timestamp)))))))

;; A function which shows guestbook messages.
(defun/cc guestbook/messages ()
  (template
   (<:div :id "body"
	  (<:a :href (function/url ()
		       (let ((params (send/suspend (guestbook/add))))
			 (apply #'message-add *guestbook* params)
			 (guestbook/messages)))
	       "Add message")
	  (<:div :id "messages"
		 (mapcar #'(lambda (item) (render-message item))
			 (message-list *guestbook*))))))

;; A form for new messages
(defun/cc guestbook/add ()
  (template 
   (<:form :action (action/url ((sender "sender") (subject "subject") (text "text"))
		     (answer (list :sender sender :subject subject :text text)))
	   :method "POST"
	   (<:table
	    (<:tr (<:td "Who are you:")
		  (<:td (<:input :size "40" :name "sender")))
	    (<:tr (<:td "Subject:") (<:td (<:input :size "40" :name "subject")))
	    (<:tr (<:td :valign "top" "Message: ") (<:td (<:textarea :rows "10" :cols "40" :name "text" "")))
	    (<:tr (<:td :colspan "2"
			(<:input :type "submit" :value "Sign Guestbook")))))))

;; http://localhost:8080/guestbook/guestbook
(defurl *guestbook* "guestbook" ()
 (guestbook/messages))

(register *server* *guestbook*)