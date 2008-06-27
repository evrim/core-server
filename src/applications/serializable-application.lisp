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

(in-package :core-server)

;;+----------------------------------------------------------------------------
;;| Serializable Application
;;+----------------------------------------------------------------------------
;;
;; This file contains base implementation of serializable application. The main
;; aim is to create a stub object that holds some important properties of our
;; application. This object is created by executing function:
;;
;; (make-serializable-application (fqdn project-name admin-email project-pathname
;;                                 &optional htdocs-pathname use depends-on))
;;
;; Above method creates a stub application object so that we serialize it to disk:
;;
;; (serialize *stub*)
;; 
;; See http://labs.core.gen.tr/#firstapp for a detailed tutorial.
;;
;; Currently, there are three types of serializable application:
;;
;;    SCM     Class
;; ------------------------------------
;; 1)  -      serializable-application
;; 2) Darcs   darcs-application
;; 3)  Git    git-application
;;
;; Refer to src/classes.lisp for class definitions.
;; 
(defmethod package-keyword ((self serializable-web-application) &optional long)
  (or (and long (make-keyword (strcat "tr.gen.core." (web-application.project-name self))))
      (make-keyword (web-application.project-name self))))

(defmethod package-test-keyword ((self serializable-web-application))
  (make-keyword (format nil "~A.test" (package-keyword self))))

(defmethod model-class ((self serializable-web-application))  
  (intern (format nil "~A" (string-upcase (strcat (web-application.project-name self) "-model")))))

(defmethod application-class ((self serializable-web-application))
  (intern (format nil "~A" (string-upcase (strcat (web-application.project-name self) "-application")))))

(defmethod serialize-source ((self serializable-web-application) symbol)
  (with-package :core-server
    (with-open-file (out (source-to-pathname self symbol)
			 :direction :output :if-does-not-exist :create :if-exists :supersede)
      (let ((*print-escape* nil)
	    (*print-pretty* t))
	(mapcar #'(lambda (line)
		    (format out "~A" (string-downcase (format nil "~S~%~%" line))))
		(cdr (apply symbol (list self))))))))

(defmethod serialize-asd ((self serializable-web-application))
  (with-open-file (out (merge-pathnames (make-pathname :name (web-application.project-name self) :type "asd")
					(web-application.project-pathname self))
		       :direction :output :if-does-not-exist :create :if-exists :supersede)
    (mapcar #'(lambda (line)
		(format out "~A" (string-downcase (format nil "~S~%~%" line))))
	    (cdr (apply 'asd (list self))))))

(defmethod source-to-pathname ((self serializable-web-application) symbol)
  (let* ((result (cl-ppcre:split "(/)" (string-downcase (string symbol))))
	 (directories (butlast result))
	 (file (last1 result)))
    (merge-pathnames (make-pathname :directory (cons :relative directories) :name file :type "lisp")
		     (web-application.project-pathname self))))

(defmethod serialize ((self serializable-web-application))
  ;; Create template folders
  (mapcar #'(lambda (dir)
	      (ensure-directories-exist (merge-pathnames dir (web-application.project-pathname self))))
	  (serializable-web-application.directories self))

  (serialize-asd self)

  (mapcar (curry #'serialize-source self) (serializable-web-application.sources self)))

(defmethod evaluate ((self serializable-web-application))
  (pushnew (web-application.project-pathname self)
	   asdf:*central-registry*)
  (asdf:oos 'asdf:load-op (package-keyword self)))

(defmethod asd ((self serializable-web-application))
  `(progn
     (in-package :asdf)
     
     (asdf::defsystem ,(package-keyword self)
       :description "Core Template Application"
       :version ".1"
       :author ,(web-application.admin-email self)
       :maintainer ,(web-application.admin-email self)
       :licence "LGPL v2"
       :components ((:static-file ,(strcat (web-application.project-name self) ".asd"))
		    (:module :src
			     :serial t
			     :components
			     ((:file "packages")
			      (:file "model" :depends-on ("packages"))
			      (:file "tx" :depends-on ("model"))
			      (:file "interfaces" :depends-on ("tx"))
			      (:file "application" :depends-on ("interfaces"))
			      (:module :ui
				       :serial t
				       :components
				       ((:file "main"))))))
       :depends-on ,(serializable-web-application.depends-on self)
       :serial t)
     
     (asdf::defsystem ,(package-test-keyword self)
       :components ((:module :t
			     :components
			     ((:file "packages"))))
       :depends-on (,(package-keyword self) :core-server :rt))

     (defmethod perform ((op asdf:test-op) (system (eql (asdf::find-system ,(package-keyword self)))))
       (asdf:oos 'asdf:load-op ,(package-test-keyword self))
       ;; (funcall (intern (string :run!) (string :it.bese.FiveAM)) ,(package-keyword self))
       )))

(defmethod src/packages ((self serializable-web-application))
  `(progn
     (in-package :cl-user)
     (defpackage ,(package-keyword self t)
       (:nicknames ,(package-keyword self))
       (:use ,@(serializable-web-application.use self))
       (:shadowing-import-from #:cl-prevalence #:name)
       (:shadowing-import-from #:arnesi #:new))))

(defmethod src/model ((self serializable-web-application))  
  `(progn
     (in-package ,(package-keyword self))
     (defclass ,(model-class self) ()
       ())
     (defmethod print-object ((self ,(model-class self)) stream)
       (print-unreadable-object (self stream :type t :identity t)))))

(defmethod src/tx ((self serializable-web-application))
  `(progn
     (in-package ,(package-keyword self))))

(defmethod src/interfaces ((self serializable-web-application))
  `(progn
     (in-package ,(package-keyword self))))

(defmethod src/application ((self serializable-web-application))
  `(progn
     (in-package ,(package-keyword self))
     (defvar *wwwroot* (make-project-path ,(symbol-name (package-keyword self)) "wwwroot"))
     
     (defvar *db-location* (merge-pathnames
			    (make-pathname :directory '(:relative "var" ,(web-application.fqdn self) "db"))
			    (sb-posix:getenv (string-upcase "CORESERVER_HOME"))))
     (defclass ,(application-class self) (http-application
					  apache-web-application
					  database-server
					  logger-server
					  serializable-web-application)
       ()
       (:default-initargs
	 :directory *db-location*
	 :db-auto-start t
	 :model-class ',(model-class self)
	 :fqdn ,(web-application.fqdn self)
	 :admin-email ,(web-application.admin-email self)
	 :project-name ,(web-application.project-name self)
	 :project-pathname ,(web-application.project-pathname self)
	 :htdocs-pathname ,(or (web-application.htdocs-pathname self)
			       `*wwwroot*)
	 :sources ',(serializable-web-application.sources self)
	 :directories ',(serializable-web-application.directories self)
	 :use ',(serializable-web-application.use self)
	 :depends-on ',(serializable-web-application.depends-on self)
	 :urls nil))
     (defvar *app* (make-instance ',(application-class self)))
     (defun register-me (&optional (server *server*))
       (core-server::register server *app*))
     (defun unregister-me (&optional (server *server*))
       (core-server::unregister server *app*))))

(defmethod src/security ((self serializable-web-application))
  `(progn
     (in-package ,(package-keyword self))))

(defmethod src/ui/main ((self serializable-web-application))
  `(progn
     (in-package ,(package-keyword self))
     ;; (defun header ()
;;        (<:h1 "Header"))
;;      (defun footer ()
;;        (<:h1 "Footer"))
;;      (defcomponent main-window (ajax-window)
;;        ())
;;      (defmethod render ((self main-window))
;;        (<:div :id "header" (header))
;;        (<:div :id "body" (<:h1 "Hi, i'm a template *core* application."))
;;        (<:div :id "footer" (footer)))
     ))

(defun make-serializable-application (fqdn project-name admin-email project-pathname
				      &optional htdocs-pathname use depends-on)
  "Returns a new serializable application object having provided parameters"
  (let ((params (list :fqdn fqdn
		      :project-name project-name
		      :admin-email admin-email
		      :project-pathname project-pathname)))
    (and htdocs-pathname (nconc params (list :htdocs-pathname htdocs-pathname)))
    (and use (nconc params (list :use use)))
    (and depends-on (nconc params (list :depends-on depends-on)))
    (apply #'make-instance 'serializable-web-application params)))