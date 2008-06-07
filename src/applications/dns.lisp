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

(defvar +ns1+ "139.179.139.251")
(defvar +ns2+ "212.175.40.11")
(defvar +mx+ "212.175.40.55")

(defclass dns-application (web-application)
  ((ns :accessor dns-application.ns :initform (list +ns1+ +ns2+) :initarg :ns)
   (mx :accessor dns-application.mx :initform (list +mx+) :initarg :mx)
   (alias :accessor dns-application.alias :initform nil :initarg :alias)))

(defmethod deploy-ns ((self dns-application))
  (mapcar (lambda (ns)
	    (when (null (find-ns (application.server self) (web-application.fqdn self)))	      
	      (add-ns (application.server self) (web-application.fqdn self) ns)))
	  (ensure-list (dns-application.ns self))))

(defmethod deploy-mx ((self dns-application))
  (mapcar (lambda (mx)
	    (when (null (find-mx (application.server self) (web-application.fqdn self)))	      
	      (add-mx (application.server self) (web-application.fqdn self) mx)))
	  (ensure-list (dns-application.mx self))))

(defmethod deploy-alias ((self dns-application))
  (mapcar (lambda (alias)
	    (when (null (find-alias (application.server self) (web-application.fqdn self)))	      
	      (add-alias (application.server self) (car alias) (cdr alias))))
	  (ensure-list (dns-application.alias self))))

(defmethod start ((self dns-application))
  (deploy-ns self)
  (deploy-mx self)
  (deploy-alias self))

(defmethod stop ((self dns-application))
  )


