;; Links web application demonstrating javascript usage
;; http://localhost:8080/links/main

;; TODO: will be rewritten with upcoming javascript components
(defpackage :links
  (:use :cl :core-server :arnesi))

(in-package :links)

;; Create an application
(defparameter *links-app*
  (make-instance 'http-application
		 :fqdn "links"
		 :admin-email "aycan@core.gen.tr"))

(defparameter *links-db* (make-hash-table :test #'equal))

(defun put-link (url-n-title)
  (setf (gethash (car url-n-title) *links-db*) (cons (cadr url-n-title) 1)))

(defun get-link (url)
  (gethash url *links-db*))

(defun rate-link (url rate)
  (let ((val (gethash url *links-db*)))
    (setf (gethash url *links-db*) (cons (car val) (+ rate (cdr val))))))

(defun/cc all-links (db)
  (mapcar #'display-link (hash-table-keys db) (hash-table-values db)))

;; Render a page
(defun/cc page (body)
  (<:html
   (<:head
    (<:meta :http--equiv "Content-Type" :content "text/html; charset=utf-8"))
   (<:body
    (<:h1 "Links Example")
    body)))

;; Display the result
(defun/cc display-link (url title-n-count)
  (<:div :class "link"
	 (<:p (format nil "~D" (cdr title-n-count))
	      (<:a :href url (car title-n-count))
	      (<:a :href (action/url () (answer (list :rate url 1))) "+1")
	      (<:a :href (action/url () (answer (list :rate url -1))) "-1"))))

;; Read a link
(defun/cc read-link ()
  (page
   (<:form :method "POST"
	   :action (action/url ((title "title") (link "link"))
		     (answer (list :addlink link title)))
	   (<:p "Please enter title and URL:")
	   (<:input :type "text" :name "title")
	   (<:input :type "text" :name "link")
	   (<:input :type "submit" :value "Post"))))

;; Register a handler
(defhandler "main" ((self http-application))
  (labels ((ctl (ans)
	     (case (car ans)
	       (:new
		(ctl (send/forward (read-link))))
	       (:addlink
		(put-link (cdr ans)))
	       (:rate
		(rate-link (cadr ans) (caddr ans)))
	       (otherwise
		(let ((res (send/suspend
			     (page
			      (list (<:p "Add Link: " (<:a :href (action/url () (answer (list :new))) "+"))
				    (<:div :id "links" (all-links *links-db*)))))))
		  (ctl res))))
	     (ctl nil)))
    (ctl nil)))

(register *server* *links-app*)