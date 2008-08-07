(in-package :manager)

(defun make-css (css-list)
  (apply #'concatenate 'string (mapcar #'dom2string css-list)))

;; generate a css string compile time
(defparameter *css*
  (make-css (list (css "body" :margin "0" :padding "0")
		  (css "#footer" :text-align "center"))))

(defun/cc header ()
  (<:div :id "header"))

(defun/cc footer ()
  (<:div :id "footer"
	 (<:a :href "http://labs.core.gen.tr" "Core Server Project")))

(defun/cc page (body)
  (<:html
   (<:head
    (<:title "Core Server Manager")
    (<:meta :http--equiv "Content-Type" :content "text/html; charset=utf-8")
    (<:style :type "text/css" *css*))
   (<:body
    (header)
    body
    (footer))))

(defurl *app* "" ()
  (page
   (<:div :id "content"
	  (<:h1 "Core Server Manager")
	  (<:p "Welcome to your core-server instance.")
	  (<:p (format nil "Hostname: ~A" (hostname))))))
