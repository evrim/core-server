;; File upload example
;; http://localhost:8080/fupload/upload

(defpackage :fupload
  (:use :cl :core-server :arnesi))

(in-package :fupload)

;; Create an application
(defparameter *fupload-app*
  (make-instance 'http-application
		 :fqdn "fupload"
		 :admin-email "aycan@core.gen.tr"))

;; Render a page
(defun/cc page (body)
  (<:html
   (<:head
    (<:meta :http--equiv "Content-Type" :content "text/html; charset=utf-8"))
   (<:body
    body)))

;; Read a text and a file
(defun/cc read-file (msg)
  (send/suspend
    (page
     (<:form :enctype "multipart/form-data" 
	     :method "POST"
	     :action (action/url ((descr "descr") (photo "photo"))
		       (answer (cons descr photo)))
	     (<:p msg)
	     (<:input :type "text" :name "descr")
	     (<:input :type "file" :name "photo")
	     (<:input :type "submit" :value "Send")))))

;; Preview image with inline image tag.
(defun/cc preview (media)
  (send/suspend
    (page
     (<:div :id "frame"
	    (<:p (car media))
	    ;; Preview with an inline image: <IMG SRC=\"data:image/jpg;base64,[...]\">
	    (make-inline-image (cdr media))
	    ;; A form to handle saving
	    (<:form :method "POST"
		    :action (action/url ()
			      (let* ((tlm (cdr media))
				     (path (pathname (format nil "/tmp/~A" (mime.filename tlm)))))
				(mime.serialize tlm path))
			      (answer t))
		    (<:input :type "submit" :name "save" :value "Save"))
	    (<:form :method "POST"
		    :action (action/url () (answer nil))
		    (<:input :type "submit" :value "Cancel"))))))

;; Register a handler
(defurl *fupload-app* "upload" ()
  (if (preview (read-file "Send us your photo with a description line:"))
      (page "File saved succesfuly.")
      (page "Operation cancelled.")))

;; Register our application to the server
(register *server* *fupload-app*)