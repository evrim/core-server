;; Just compile it with C-c C-k or load it with C-c C-l and then visit
;; http://localhost:8080/guestbook/guestbook

(defpackage :guestbook
  (:use :cl :core-server :arnesi :js)
  (:shadowing-import-from #:arnesi #:new))

(in-package :guestbook)

;; a class for messages and a constructor
(defclass+ guest-message ()
  ((sender :host local :accessor sender)
   (subject :host local :accessor subject)
   (text :host local :accessor text)
   (timestamp :host local :accessor timestamp))
  (:ctor (sender subject text &optional (timestamp (get-universal-time)))))

;; Here we instantiate from local-unit, so that we can write methods
;; that will be automatically synchronized
(defclass+ guestbook (local-unit)
  ((owner :host local :accessor owner)
   (messages :host local :accessor messages :initform nil))
  (:ctor (owner &optional messages)))

;; owner email
(defparameter *siteowner* "aycan@core.gen.tr")

;; Our guestbook instance
(defparameter *guestbook* (guestbook *siteowner* (list (guest-message "System" "Initial message" "This is initial message"))))

;; here we implement add-message method for the guestbook unit.
(defmethod/unit add-message :sync ((self guestbook) message)
  (push message (messages self))
  message)

;; http://localhost:8080/guestbook/*
(defparameter *guestbook-example*
  (make-instance 'http-application
		 :fqdn "guestbook"
		 :admin-email *siteowner*))

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
		       (let ((content (send/suspend (guestbook/add))))
			 (add-message *guestbook* content)
			 (guestbook/messages)))
	       "Add message")
	  (<:div :id "messages"
		 (mapcar #'(lambda (item) (render-message item))
			 (messages *guestbook*))))))

;; A form for new messages
(defun/cc guestbook/add ()
  (template 
   (<:form :action (action/url ((owner "owner") (subject "subject") (text "text"))
		     (answer (guest-message owner subject text)))
	   :method "POST"
	   (<:table
	    (<:tr (<:td "Who are you:")
		  (<:td (<:input :size "40" :name "owner")))
	    (<:tr (<:td "Subject:") (<:td (<:input :size "40" :name "subject")))
	    (<:tr (<:td :valign "top" "Message: ") (<:td (<:textarea :rows "10" :cols "40" :name "text" "")))
	    (<:tr (<:td :colspan "2"
			(<:input :type "submit" :value "Sign Guestbook")))))))

;; http://localhost:8080/guestbook/guestbook
(defurl *guestbook-example* "guestbook" ()
 (guestbook/messages))

(register *server* *guestbook-example*)