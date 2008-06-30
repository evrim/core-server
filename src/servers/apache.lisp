;; Core Server: Web Application Server

;; Copyright (C) 2006-2008  Metin Evrim Ulu, Aycan iRiCAN

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :tr.gen.core.server)

;;+----------------------------------------------------------------------------
;;| Apache2 Web Server
;;+----------------------------------------------------------------------------
;;
;; This file implements Apache2 Web Server Controller
;;

;; FIXmE: move two accessor to apache web application file.
(defmethod apache-web-application.config-pathname
    ((self apache-web-application) (server apache-server))
  "Returns confiugration pathname of the application 'self'"
  (merge-pathnames (make-pathname :name (web-application.fqdn self)
				  :type *apache-default-config-extenstion*)
		   (apache-server.vhosts.d-pathname server)))

(defmethod apache-web-application.docroot-pathname
    ((self apache-web-application) &optional (server nil))
  (merge-pathnames (make-pathname :directory (list :relative (web-application.fqdn self)))
		   (apache-server.htdocs-pathname (or server (application.server self)))))

;;-----------------------------------------------------------------------------
;; Apache2Ctl Command
;;-----------------------------------------------------------------------------
(defmethod apachectl ((self apache-server) params)  
  (sb-ext:run-program (namestring +sudo+)
		      (cons (namestring (apache-server.apachectl-pathname self)) 
			    params
			    ;; (cons "--nocolor"
			    ;; (cons "--quiet" params))
			    )))

(defmethod validate-configuration ((self apache-server))
  "Validates Apache2 configuration"
  (eq 0 (sb-impl::process-exit-code (apachectl self '("configtest")))))

;;-----------------------------------------------------------------------------
;; Comar Command
;;-----------------------------------------------------------------------------
#+pardus
(defmethod comar ((self apache-server) command)  
  (sb-ext:run-program +sudo+
		      (cons "/bin/service" (cons "apache" command))))

;;-----------------------------------------------------------------------------
;; Configuration Helpers
;;-----------------------------------------------------------------------------
(defmethod create-redirector ((self apache-server) (app apache-web-application))
  "Creates an index.html file for 'app' which redirectory to
'default-entry-point' slot of 'app'"
  (when (apache-web-application.default-entry-point app)
    (let* ((redirector-pathname (merge-pathnames 
				 (make-pathname :name "index"
						:type "html"
						:directory (list :relative (web-application.fqdn app)))
				 (apache-server.htdocs-pathname self)))
	   (s (make-core-file-output-stream redirector-pathname)))      
      (with-html-output s	  
	(<:html
	 (<:head
	  (<:meta :http--equiv "Refresh"
		  :content (format nil "0;URL=~A" (apache-web-application.default-entry-point app))))))
      (close-stream s)
      (fix-apache-permissions redirector-pathname))))

(defmethod create-vhost-config ((self apache-server) (app apache-web-application))
  "Creates a vhost configuration file for 'app' and writes it to
server config directory"
  (let* ((config-pathname (apache-web-application.config-pathname app self))
	 (config-data (read-string-from-file (apache-web-application.vhost-template-pathname app))))
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
  "Creates a document root directory for 'app' and adjustes
permissions accordingly"
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

;;-----------------------------------------------------------------------------
;; Server Protocol Implementation
;;-----------------------------------------------------------------------------
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

(defmethod register ((self apache-server) (app apache-web-application))
  (create-docroot self app)
  (apache-server.refresh self app))

(defmethod unregister ((self apache-server) (app apache-web-application))  
  (let* ((config-pathname (apache-web-application.config-pathname app self)))
    (when (probe-file config-pathname)
      (delete-file config-pathname))
    (graceful self)))

(defmethod apache-server.refresh ((self apache-server) (app apache-web-application))
  (create-redirector self app)
  (create-vhost-config self app)
  (graceful self))

(defmethod apache-server.destroy ((self apache-server) (app apache-web-application))
  (unregister self app)
  (sb-ext:run-program +rm+ (list "-r" (namestring (apache-web-application.docroot-pathname app self)))))
