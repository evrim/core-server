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
;; | Apache Web Application
;; +-------------------------------------------------------------------------
(defclass apache-web-application (web-application)
  ((vhost-template-pathname
    :accessor apache-web-application.vhost-template-pathname :initarg :vhost-template-pathname
    :initform (merge-pathnames
	       (make-pathname :directory '(:relative "etc") :name "vhost" :type "conf")
	       (asdf:component-pathname (asdf:find-system :core-server)))
    :documentation "Apache Virtual Host Template Configuration Pathname - see etc/vhost.conf") 
   (default-entry-point
     :accessor apache-web-application.default-entry-point :initarg :default-entry-point
     :initform "index.core"
     :documentation "Default Entry Point for redirector creation, setq nil not to.")
   (skel-pathname
    :accessor apache-web-application.skel-pathname :initarg :skel-pathname
    :initform (merge-pathnames (make-pathname :directory '(:relative "etc" "skel"))
			       (asdf:component-pathname (asdf:find-system :core-server)))
    :documentation "Skeleton Pathname which is copied to htdoc directory. setq nil no to."))
  (:documentation "Apache Web Application Class - This class is used
this to manage vhost configuration for this application. It generates
a new vhost configuration from 'vhost-template-pathname' and writes it
to apache vhost configuration directory.  See src/servers/apache.lisp
for implementation."))
