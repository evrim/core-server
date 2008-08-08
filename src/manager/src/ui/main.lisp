(in-package :manager)

(defun make-css (css-list)
  (apply #'concatenate 'string (mapcar #'dom2string css-list)))

;; generate a css string compile time
(defparameter *css*
  (make-css (list (css "body" :font-family "verdana, sans-serif")
		  (css "#footer" :font-size "80%" :margin "2em auto" :width "60em")
		  (css ".chapter" :margin-left "auto" :margin-right "auto" :width "50em")
		  (css "code, pre" :font-family "monospace" :font-weight "normal")
		  (css ".path" :color "#448844")
		  (css "div.sysinfo"
		       :background "#FFFFC9 none repeat scroll 0 0"
		       :padding "1em"
		       :border "1px solid #B4BAEA"))))

(defvar *cslink* (<:a :href "http://labs.core.gen.tr" "Core Server"))
(defvar *examples* (<:code :class "path" (format nil "~A" (bootstrap:in-home #P"examples/"))))
(defvar *header* (<:div :id "header"))
(defvar *footer*
  (<:div :id "footer"
    (<:hr)
    (<:p (<:a :href "http://labs.core.gen.tr" (format nil "Core Server ~A" (core-server-version)))
	 " | "
	 (format nil "~A ~A" (lisp-implementation-type) (lisp-implementation-version)))))

(defun box (summary body)
  (<:div :class "box"
    (<:table :border "0" :summary summary
	     (<:tbody
	      (<:tr
	       (<:td :align "center" :width "25" :valign "top" :rowspan "2"
		     (<:img :src "images/note.png" :alt "info icon"))
	       (<:th :align "left" summary))
	      (<:tr
	       (<:td :align "left" :valign "top"
		     body))))))

(defun/cc page (body)
  (<:html
   (<:head
    (<:title "Core Server Manager")
    (<:meta :http--equiv "Content-Type" :content "text/html; charset=utf-8")
    (<:style :type "text/css" *css*))
   (<:body
    *header*
    (<:div :class "chapter"
      body)
    *footer*)))

(defun/cc main ()
  (page
   (<:div :id "content"
     (<:div :id "introduction"
       (<:h1 "Core Server Manager")
       (<:p *cslink* " is an application server written in Common Lisp. You can find sample applications at " *examples* "."))
     (<:div :class "sysinfo"
       (box "System Information" (<:p (format nil "Hostname: ~A" (hostname))))))))

(defurl *app* "manager" ()
  (main))