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
  ((fqdn :reader web-application.fqdn :initarg :fqdn :initform (error "Fqdn must be supplied.")
	 :documentation "Fully qualified domain name of this application")
   (admin-email :accessor web-application.admin-email :initarg :admin-email
		:initform (error "Admin email must be supplied.")
		:documentation "Administrators' email address")
   (project-name :accessor web-application.project-name
		 :initarg :project-name :initform nil
		 :documentation "Name/Symbol of the project")
   (project-pathname :accessor web-application.project-pathname
		     :initarg :project-pathname :initform nil
		     :documentation "Pathname of the project")
   (htdocs-pathname :accessor web-application.htdocs-pathname
		    :initarg :htdocs-pathname :initform nil
		    :documentation "Htdocs pathname of the project,
		    used for serving static files"))
  (:documentation "Base Web Application Class"))
