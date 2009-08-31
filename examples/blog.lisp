(in-package :core-server)

;; +----------------------------------------------------------------------------
;; | Core Server Example - Blog
;; +----------------------------------------------------------------------------
(defpackage :blog
  (:use :cl :core-server :arnesi))

(in-package :blog)
;;
;; A persistent multi-user blog example.
;;
;; http://localhost:8080/blog/index.core
;;
;; To run this example type in the repl:
;;
;; (in-package :blog)
;; (register *server* *app*)
;; (start *app*)
;; (init-blog-test-data)
;;

;; we define classes and their relations here
(defclass+ author ()
  ((email :initform (error "Please specify :email"))
   (password :initform (error "Please specify :password"))
   (name :initform nil)
   (creation-date :initform (get-universal-time))
   (blogs :type blog* :relation author)
   (comments :type comment* :relation author)))

(defclass+ comment ()
  ((author :type author :relation comments :initform (error "please specify :author"))
   (text :initform (error "please specify :text"))
   (timestamp :initform (get-universal-time))
   (blog :type blog :relation comments))
  (:ctor (author text)))

(defclass+ blog ()
  ((author :type author :relation blogs)
   (title :type string :initform (error "Please specify :title"))
   (text :type string :initform (error "please specify :text"))
   (timestamp :initform (get-universal-time))
   (comments :type comment* :relation blog))
  (:ctor (author title text)))

;; generates add, delete, update, find and list methods over these classes.
(defcrud comment)
(defcrud blog)
(defcrud author)

(defapplication blog-app (http-application database-server)
  ()
  (:default-initargs
   :fqdn "blog"
    :admin-email "evrim@core.gen.tr"
    :database-directory #P"/tmp/db-blog-app/"
    :auto-start t))

(defvar *app* (make-instance 'blog-app))

(defun init-blog-test-data ()
  (with-transaction (*app*)
    (author.add *app* :name "evrim" :password "mypass" :email "evrim@core.gen.tr")
    (author.add *app* :name "aycan" :password "mypass" :email "aycan@core.gen.tr")))

(defun userp ()
  (query-session 'user))

(defun/cc blog/short (blog &optional (len 500))
  (<:div :class "blog view"
	 :id (format nil "blog-~A" (get-id blog))
	 (<:div :class "title" (blog.title blog))
	 (<:div :class "author" (author.email (blog.author blog)) " - ")
	 (<:div :class "timestamp" (time->string (blog.timestamp blog) :long :en))
	 (let* ((blog-length (length (blog.text blog)))
		(show-n (min blog-length len)))
	   (<:div :class "text" (subseq (blog.text blog) 0 show-n)
		  (if (> blog-length len)
		      "....")))))

(defun/cc blog/view (blog)
  (<:div :class "blog view"
	 :id (format nil "blog-~A" (get-id blog))
	 (<:div :class "title" (blog.title blog))
	 (<:div :class "author" (author.email (blog.author blog)) " - ")
	 (<:div :class "timestamp" (time->string (blog.timestamp blog) :long :en))
	 (<:div :class "text" (blog.text blog))))

(defun/cc blog/edit (blog)
  (<:div :class "blog edit"
	 :id (format nil "blog-~A" (get-id blog))
   (<:form :method "POST"
	   :action (action/url ((author "author") (text "text") (title "title"))
		     (answer (list 'save :title title :text text :author (query-session 'user))))
	   (<:div :class "field title"
		  (<:div :class "description" "Title:")
		  (<:div :class "value" (<:input :type "text" :name "title" :value (blog.title blog))))
	   (<:div :class "field text"
		  (<:div :class "description" "Text:")
		  (<:script :type "text/javascript"
			    :src "http://wiki.moxiecode.com/examples/tinymce/jscripts/tiny_mce/tiny_mce.js")
		  (<:script :type "text/javascript" *tinymce-config*)
		  (<:div :class "value" (<:textarea :name "text" (blog.text blog))))
	   (<:div :class "submit" (<:input :type "submit" :value "Save")))))

(defun/cc blog/new ()
  (blog/main
   (<:h2 "Add a new blog")
   (blog/edit (blog nil nil nil))))

(defun/cc blog/menu ()
  (<:div :class "menu"
	 (<:ul
	  (<:li (<:a :href "index.core" "Blogs"))
	  (<:li (<:a :href (function/url () (answer 'new)) "Write"))
	  (<:li (if (userp)
		    (<:a :href (action/url ()
				 (update-session 'user nil)
				 (answer nil))
			 "Logout"))))))

(defun/cc blog/list (blogs &optional (n 20))
  (<:div :class "list"
   (<:h2 "Latest Blogs")
   (<:ul
    (mapcar #'(lambda (blog seq)
		(<:li
		 (<:a :href (action/url () (answer (cons 'view blog)))
		      (blog.title blog))))
	    blogs (core-server::seq n)))))

(defun/cc auth/login ()
  (<:div :class "login"
	 (<:form :method "POST"
		 :action (action/url ((email "email") (password "password"))
			   (answer (list 'auth/login :email email :password password)))
		 (<:div (<:p "Email:")
			(<:p (<:input :type "text" :name "email")))
		 (<:div (<:p "Password:")
			(<:p (<:input :type "text" :name "password")))
		 (<:div (<:input :type "submit" :value "Gir")))))

(defun/cc blog/main (&rest body)
  (<:html
   (<:head
    (<:title "CoreServer Blog Example")
    (<:style :type "text/css" *blog-css*))
   (<:body
    (<:div :class "container"
	   (<:div :class "header" (<:h1 "CoreServer Blogs"))
	   (if (userp)
	       (blog/menu))
	   (<:div :class "right"
		  (blog/list (blog.list *app*))
		  (if (not (userp)) 
		      (auth/login)))
	   (<:div :class "left" body)
	   (<:div :class "footer"
		  (<:i "Blogs - 2008 &copy; Core Server"))))))

;; entrypoint to our application
(defhandler "index.core" ((app blog-app) (blog "blog"))  
  (labels ((loopy (result)
	      (loopy		     
		 (send/suspend		 
		   (cond
		     ((eq 'new result)
		      (blog/new))
		     ((eq 'save (car result))
		      (apply #'blog.add (cons app (cdr result)))
		      (blog/main (mapcar #'blog/short (blog.list app))))
		     ((eq 'view (car result))
		      (blog/main (blog/view (cdr result))))
		     ((eq 'auth/login (car result))		      
		      (let ((user (apply #'author.find (cons app (cdr result)))))
			(when user
			  (if (equal (author.password user) (getf (cdr result) :password))
			      (update-session 'user user)))
			(loopy nil)))
		     (t
		      (blog/main (mapcar #'blog/short (blog.list *app*)))))))))
    (loopy nil)))

(defparameter *blog-css*
  "
* { margin:0; padding:0; }
html { margin:0; padding:0; }
h1 { padding-top:10px; padding-bottom:5px; }
h2 { padding-top: 5px; padding-bottom:2px; }
.body { margin:auto; width:100%;}
.container { margin:auto; width:80%; text-align:left;}
.left { float:left; width:70%;}
.right {float :right; }
.footer { clear: both; text-align:center; margin:5px; }
.menu { padding: 5px 0 5px 0; }
.menu ul { display:inline; }
.menu ul li { display: inline; }
.view .title { font-size: 1.5em; font-weight: bold; }
.view .author {float:left; padding:0 2px 0 0; }
.view .timestamp { }
.view .text { padding: 20px 0 5px 0; }
.edit .title .value input { width: 500px; padding:5px; font-size:1.5em; }
.edit .author .value input { padding:5px; width:300px;} 
.edit .text .value textarea { width: 550px; padding:5px; }
.edit .submit input { background:transparent; padding:10px; margin:5px; margin-left:0; font-size:1.2em;}
.login .value input { padding:5px; font-size:1.5em; width:220px;}
.login .submit input { background:transparent; padding:10px; margin:5px; margin-left:0; font-size:1.2em;")

(defparameter *tinymce-config* "
tinyMCE.init({
	// General options
	mode : 'textareas',
	theme : 'advanced',
	plugins : 'safari,pagebreak,style,layer,table,save,advhr,advimage,advlink,emotions,iespell,inlinepopups,insertdatetime,preview,media,searchreplace,print,contextmenu,paste,directionality,fullscreen,noneditable,visualchars,nonbreaking,xhtmlxtras,template',

	// Theme options
	theme_advanced_buttons1 : 'bold,italic,underline,strikethrough,|,justifyleft,justifycenter,justifyright,justifyfull,|,styleselect,formatselect,fontselect,fontsizeselect',
	theme_advanced_buttons2 : 'cut,copy,paste,pastetext,pasteword,|,search,replace,|,bullist,numlist,|,outdent,indent,blockquote,|,undo,redo,|,link,unlink,anchor,image,cleanup,help,code,|,insertdate,inserttime,preview,|,forecolor,backcolor',
	theme_advanced_buttons3 : 'tablecontrols,|,hr,removeformat,visualaid,|,sub,sup,|,charmap,emotions,iespell,media,advhr,|,print,|,ltr,rtl,|,fullscreen',
	theme_advanced_buttons4 : 'insertlayer,moveforward,movebackward,absolute,|,styleprops,|,cite,abbr,acronym,del,ins,attribs,|,visualchars,nonbreaking,template,pagebreak',
	theme_advanced_toolbar_location : 'top',
	theme_advanced_toolbar_align : 'left',
	theme_advanced_statusbar_location : 'bottom',
	theme_advanced_resizing : true,
});
")

(register *server* *app*)
;; TODO: fix core-cps-io-stream of http context
;; TODO: fix session preservance via cookies
