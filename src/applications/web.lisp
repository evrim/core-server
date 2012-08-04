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

;; +-------------------------------------------------------------------------
;; | Web Application
;; +-------------------------------------------------------------------------
(defclass+ web-application (application)
  ((fqdn :reader web-application.fqdn :initarg :fqdn :host local
	 :initform (error "Fqdn must be supplied.")
	 :documentation "Fully qualified domain name of this application")
   (admin-email :accessor web-application.admin-email :initarg :admin-email
		:host local :initform (error "Admin email must be supplied.")
		:documentation "Administrators' email address")
   (project-name :accessor web-application.project-name
		 :host local :initarg :project-name :initform nil
		 :documentation "Name/Symbol of the project")
   (project-pathname :accessor web-application.project-pathname
		     :initarg :project-pathname :initform nil
		     :host local :documentation "Pathname of the project")
   (htdocs-pathname :accessor web-application.htdocs-pathname
		    :initarg :htdocs-pathname :initform nil
		    :host local :documentation "Htdocs pathname of the project,
		    used for serving static files"))
  (:documentation "Base Web Application Class"))

(defmethod print-object ((self web-application) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (if (typep self 'server)
	(format stream "FQDN:\"~A\" is ~A running"
		(web-application.fqdn self)
		(if (status self) "" "*not*"))
	(format stream "FQDN:\"~A\"" (web-application.fqdn self)))))

(defmethod web-application.base-url ((self web-application))
  (format nil "/~A" (web-application.fqdn self)))

(defmethod web-application.serve-url ((self web-application) (req t))
  (format nil "/~A/TESTREQUEST" (web-application.fqdn self)))

(defmethod web-application.serve-url ((self web-application) (req http-request))
  (format nil "~A~A" (web-application.base-url self)
	  (with-core-stream (s "")
	    (mapc #'(lambda (path)
		      (char! s core-server::+uri-path-seperator+)
		      (string! s (car path))
		      (mapc #'(lambda (path)
				(core-server::char!
				 s core-server::+uri-segment-seperator+)
				(core-server::string! s path))
			    (cdr path)))
		  (core-server::uri.paths (http-request.uri req)))	    
	    (return-stream s))))

;; --------------------------------------------------------------------------
;; Overriden get-directory method: This allows us to use default
;; database directory for database driven http applications.
;; --------------------------------------------------------------------------
(defmethod database.directory ((application web-application))
  (ensure-directories-exist
   (merge-pathnames
    (make-pathname :directory (list :relative "var"
				    (web-application.fqdn application) "db"))
    (bootstrap::home))))

;; +-------------------------------------------------------------------------
;; | Root Http Application (App to serve at /)
;; +-------------------------------------------------------------------------
(defclass+ root-web-application-mixin ()
  ())

;; (defmethod web-application.base-url ((self root-web-application-mixin))
;;   (call-next-method self)
;;   ;; (let ((server (application.server self)))
;;   ;;   (format nil "http://~A:~A/" (web-application.fqdn self)
;;   ;; 	    (socket-server.port server)))
;;   )

;; (defmethod web-application.serve-url ((self root-web-application-mixin) (req t))
;;   (format nil "~A/TESTREQUEST" (web-application.base-url self)))

;; (defmethod web-application.serve-url ((self root-web-application-mixin) (req http-request))
;;   ;; (format nil "~A~A" (web-application.base-url self)
;;   ;; 	  (with-core-stream (s "")
;;   ;; 	    (flet ((one (path)
;;   ;; 		     (string! s (car path))
;;   ;; 		     (mapc #'(lambda (path)
;;   ;; 			       (core-server::char!
;;   ;; 				s core-server::+uri-segment-seperator+)
;;   ;; 			       (core-server::string! s path))
;;   ;; 			   (cdr path))))
;;   ;; 	      (mapc #'(lambda (path)
;;   ;; 			(one path)
;;   ;; 			(char! s core-server::+uri-path-seperator+))
;;   ;; 		    (core-server::uri.paths (http-request.uri req))))	    
;;   ;; 	    (return-stream s)))
;;   (call-next-method self req)
;;   )

