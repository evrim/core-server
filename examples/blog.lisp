(in-package :core-server)

(defpackage :blog
  (:use :cl :core-server :arnesi :js)
  (:shadowing-import-from #:arnesi #:new))

(in-package :blog)

(defclass blog-application (http-application apache-web-application)
  ()
  (:default-initargs :fqdn "blog"
    :admin-email "evrim@core.gen.tr"))

(defvar *app* (make-instance 'blog-application))

(defun register-me () (register *server* *app*))

(defclass+ blog (cl-prevalence::object-with-id)
  ((author :host local :accessor blog.author :initarg :author :initform (error "please specify :author"))
   (text :host local :accessor blog.text :initarg :text :initform (error "please specify :text"))
   (timestamp :accessor blog.timestamp :initarg :timestamp :initform (get-universal-time)))
  (:ctor (author text)))

(defparameter *blogs*
  (list (blog "evrim@core.gen.tr" "This is blog1")
	(blog "aycan@core.gen.tr" "This is blog2")))

(defun/cc blog/blog (blog)
  (<:div :id (format nil "blog-~A" (core-server::get-id blog))
	 (<:div :class "author"
		(blog.author blog))
	 (<:div :class "timestamp"
		(time->string (blog.timestamp blog) :long))
	 (<:div :class "text"
		(blog.text blog))))

(defun/cc blog/menu ()
  (<:div :class "menu"
	 (<:ul
	  (<:li (<:a :href "index.core" "Blogs"))
	  (<:li (<:a :href (function/url () (answer 'new)) "Write")))))

(defun/cc blog/new ()
  (blog/main
   (<:h2 "Add a new blog")))

(defun/cc blog/main (body)
  (<:html
   (<:head
    (<:title "CoreServer Blog Example"))
   (<:body
    (<:div :class "header" (<:h1 "CoreServer Blogs"))
    (blog/menu)
    body
    (<:div :class "footer"
	   (<:i "Blogs - 2008 &copy; Core Server")))))

(defurl *app* "index.core" ()  
  (labels ((loopy (result)
	      (let ((r (send/suspend			 
			 (cond
			   ((eq 'new result)
			    (blog/new))
			   (t
			    (blog/main (mapcar #'blog/blog *blogs*)))))))
		(loopy r))))
    (loopy nil)))

;; TODO: fix core-cps-io-stream of http context
;; TODO: fix session preservance via cookies
