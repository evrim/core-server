;; http://localhost:8080/blog/index.core
(in-package :core-server)

(defpackage :blog
  (:use :cl :core-server :arnesi :cl-prevalence))

(in-package :blog)

(defvar *app* (make-instance 'http-application :fqdn "blog" :admin-email "evrim@core.gen.tr"))

(defclass+ comment (object-with-id)
  ((author :host local :initform (error "please specify :author"))
   (text :host local :initform (error "please specify :text"))
   (timestamp :initform (get-universal-time)))
  (:ctor (author text)))

(defclass+ blog (object-with-id)
  ((author :host local :initform (error "please specify :author"))
   (title :host local :initform (error "Please specify :title"))
   (text :host local :initform (error "please specify :text"))
   (timestamp :initform (get-universal-time))
   (comments :initform '()))
  (:ctor (author title text)))

(defparameter *blogs*
  (list (blog "evrim@core.gen.tr" "Blog #1" "This is blog1")
	(blog "aycan@core.gen.tr" "Blog #2" "This is blog2")))

(defun/cc blog/short (blog &optional (len 500))
  (<:div :class "blog view"
	 :id (format nil "blog-~A" (get-id blog))
	 (<:div :class "title" (blog.title blog))
	 (<:div :class "author" (blog.author blog) " - ")
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
	 (<:div :class "author" (blog.author blog) " - ")
	 (<:div :class "timestamp" (time->string (blog.timestamp blog) :long :en))
	 (<:div :class "text" (blog.text blog))))

(defun/cc blog/edit (blog)
  (<:div :class "blog edit"
	 :id (format nil "blog-~A" (get-id blog))
   (<:form :method "POST"
	   :action (action/url ((author "author") (text "text") (title "title"))
		     (answer (list'save author title text)))
	   (<:div :class "field title"
		  (<:div :class "description" "Title:")
		  (<:div :class "value" (<:input :type "text" :name "title" :value (blog.title blog))))
	   (<:div :class "field author"
		  (<:div :class "description" "Author:")
		  (<:div :class "value" (<:input :type "text" :name "author" :value (blog.author blog))))
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
	  (<:li (<:a :href (function/url () (answer 'new)) "Write")))))

(defun/cc blog/list (blogs &optional (n 20))
  (<:div :class "list"
   (<:h2 "Latest Blogs")
   (<:ul
    (mapcar #'(lambda (blog seq)
		(<:li
		 (<:a :href (action/url () (answer (cons 'view blog)))
		      (blog.title blog))))
	    blogs (core-server::seq n)))))

(defun/cc blog/main (&rest body)
  (<:html
   (<:head
    (<:title "CoreServer Blog Example")
    (<:style :type "text/css" *blog-css*))
   (<:body
    (<:div :class "container"
	   (<:div :class "header" (<:h1 "CoreServer Blogs"))
	   (blog/menu)
	   (<:div :class "right" (blog/list *blogs*))
	   (<:div :class "left" body)
	   (<:div :class "footer"
		  (<:i "Blogs - 2008 &copy; Core Server"))))))

(defurl *app* "index.core" ((blog "blog"))  
  (labels ((loopy (result)
	      (loopy		     
		 (send/suspend		 
		   (cond
		     ((eq 'new result)
		      (blog/new))
		     ((eq 'save (car result))
		      (push (apply #'blog (cdr result)) *blogs*)
		      (blog/main (mapcar #'blog/short *blogs*)))
		     ((eq 'view (car result))
		      (blog/main (blog/view (cdr result))))		     
		     (t
		      (blog/main (mapcar #'blog/short *blogs*))))))))
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
