(in-package :tr.gen.core.server)

(defmethod apache-web-application.config-pathname ((self apache-web-application) (server apache-server))
  (merge-pathnames (make-pathname :name (web-application.fqdn self) :type *apache-default-config-extenstion*)
		   (apache-server.vhosts.d-pathname server)))

(defmethod apache-web-application.docroot-pathname ((self apache-web-application) (server apache-server))
  (merge-pathnames (make-pathname :directory (list :relative (web-application.fqdn self)))
		   (apache-server.htdocs-pathname server)))

#+pardus
(defmethod comar ((self apache-server) command)
  (unwind-protect
       (sb-ext:run-program +sudo+
			   (cons "/bin/service" (cons "apache" command)))))

(defmethod apachectl ((self apache-server) params)
  (unwind-protect
       (sb-ext:run-program +sudo+
			   (cons (sb-ext::native-namestring (apache-server.apachectl-pathname self)) params))))

(defmethod validate-configuration ((self apache-server))
  (eq 0 (sb-impl::process-exit-code (apachectl self '("configtest")))))

(defmethod start ((self apache-server))
  (when (validate-configuration self)
    (eq 0 (#+pardus comar #-pardus apachectl self '("start")))) t)

(defmethod stop ((self apache-server))
  (when (validate-configuration self)
    (eq 0 (#+pardus comar #-pardus apachectl self '("stop")))))

(defmethod graceful ((self apache-server))
  (when (validate-configuration self)
    (with-server-mutex (self)
      (eq 0 (sb-impl::process-exit-code (#+pardus comar #-pardus apachectl self '("reload")))))))

(defmethod status ((self apache-server))
  #+debian (> (length (cl-fad:list-directory #P"/var/run/apache2/")) 0)
  (eq 0 (sb-impl::process-exit-code (#+pardus comar #-pardus apachectl self '("status")))))

(defmethod create-redirector ((self apache-server) (app apache-web-application))
  (when (apache-web-application.default-entry-point app)
    (let ((redirector-pathname (merge-pathnames 
				(make-pathname :name "index"
					       :type "html"
					       :directory (list :relative (web-application.fqdn app)))
				(apache-server.htdocs-pathname self))))
      (with-open-file (s redirector-pathname
			 :direction :output :if-exists :supersede :if-does-not-exist :create)
	(it.bese.yaclml:with-yaclml-stream s	  
	  (<:html
	   (<:head
	    (<:meta :http--equiv "Refresh"
		    :content (format nil "0;URL=~A" (apache-web-application.default-entry-point app)))))))
      (fix-apache-permissions redirector-pathname))))

(defmethod create-vhost-config ((self apache-server) (app apache-web-application))
  (let* ((config-pathname (apache-web-application.config-pathname app self))
	 (config-data (load-file-into-string (apache-web-application.vhost-template-pathname app))))
    (with-open-file (s config-pathname :direction :output :if-exists :supersede :if-does-not-exist :create) 
      (write-string  
       (reduce #'(lambda (acc item)
		   (cl-ppcre:regex-replace-all (car item) acc (cdr item)))
	       (list (cons "\\$fqdn" (web-application.fqdn app))
		     (cons "\\$vhosts.d" (namestring (apache-server.vhosts.d-pathname self))) 
		     (cons "\\$admin-email" (web-application.admin-email app)) 
		     (cons "\\$htdocs" (namestring (apache-server.htdocs-pathname self))))
	       :initial-value config-data)
       s))
    (fix-apache-permissions (namestring config-pathname))))

(defmethod create-docroot ((self apache-server) (app apache-web-application))
  (let* ((site (web-application.fqdn app))
	 (docroot-pathname (pathname (format nil "~A~A" (apache-server.htdocs-pathname self) site))))
    (unless (probe-file docroot-pathname)
      (if (apache-web-application.skel-pathname app)
	  (sb-ext:run-program (namestring +cp+) 
			      (list "-a"
				    (namestring (apache-web-application.skel-pathname app))
				    (namestring docroot-pathname)))
	  (sb-ext:run-program +sudo+ (cons +mkdir+ (list "-p" (namestring docroot-pathname)))))
      (fix-apache-permissions (namestring docroot-pathname))
      (sb-ext:run-program +sudo+ (cons (namestring +find+)
				       (list (namestring docroot-pathname)
					     "-type" "d"
					     "-exec" (namestring +chmod+) "770" "{}" "\;"))))))

(defmethod apache-server.refresh ((self apache-server) (app apache-web-application))
  (create-redirector self app)
  (create-vhost-config self app)
  (graceful self))

(defmethod register ((self apache-server) (app apache-web-application))
  (create-docroot self app)
  (apache-server.refresh self app))

(defmethod unregister ((self apache-server) (app apache-web-application))  
  (let* ((config-pathname (apache-web-application.config-pathname app self)))
    (when (probe-file config-pathname)
      (delete-file config-pathname))
    (graceful self)))

(defmethod apache-server.destroy ((self apache-server) (app apache-web-application))
  (unregister self app)
  (sb-ext:run-program +rm+ (list "-r" (namestring (apache-web-application.docroot-pathname app self)))))

