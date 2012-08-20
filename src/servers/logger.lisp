;;+----------------------------------------------------------------------------
;;| Logger Server
;;+----------------------------------------------------------------------------
(in-package :tr.gen.core.server)

(defclass logger-server (local-unit)
  ((log-directory :accessor log-directory :initarg :log-path :initform nil)
   (log-stream :accessor log-stream :initarg :log-stream :initform nil)) ;;*core-output*
  (:documentation "Log Server mixin class - Mix this class with your
server to enable logging features. See src/servers/logger.lisp for
implementation"))

(defmethod log-directory ((self logger-server))
  (or (slot-value self 'log-directory)
      (merge-pathnames (make-pathname :directory '(:relative "var" "log"))
		       (bootstrap:home))))

(defmethod log-pathname ((self logger-server))
  (merge-pathnames (log-directory self)
		   (make-pathname :name "core-server" :type "log")))

(defmethod log! ((stream core-stream) (tag symbol) (message string))
  (checkpoint-stream stream)
  (string! stream (time->string (get-universal-time) :log))
  (char! stream #\Space)
  (string! stream (symbol-name tag))
  (char! stream #\Space)
  (string! stream message)
  (char! stream #\Newline)
  (commit-stream stream))

(defmethod/unit logger-server.log :async-no-return ((self logger-server)
						    (stream core-stream)
						    (tag symbol)
						    (message string))
  (log! stream tag message))

(defmethod log-me ((self logger-server) (tag symbol) (message string))
  (logger-server.log self (log-stream self) tag message))

;;-----------------------------------------------------------------------------
;; Server Protocol Implementation
;;-----------------------------------------------------------------------------
(defmethod start ((self logger-server))
  (unless (log-stream self)
    (setf (log-stream self) (make-core-stream
			     (open (log-pathname self)
				   :direction :output
				   :element-type '(unsigned-byte 8)
				   :if-exists :append
				   :if-does-not-exist :create
				   :external-format :utf8)))))

(defmethod stop ((self logger-server))
  (when (log-stream self)
    (close-stream (log-stream self))
    (setf (log-stream self) nil)))

(defmethod logger-server.status ((self logger-server))
  (if (log-stream self) t))

(defmethod status ((self logger-server))
  (logger-server.status self))

;; -------------------------------------------------------------------------
;; Logger Application (client of Advanced Logger Server)
;; -------------------------------------------------------------------------
(defclass logger-application (application)
  ((log-pathname :accessor logger-application.log-pathname
		 :initform nil :initarg :log-pathname)
   (log-stream :accessor logger-application.log-stream :initform nil)))

(defmethod logger-application.log-pathname ((self logger-application))
  (or (slot-value self 'log-pathname)
      (error "Please set (slot-value ~A 'log-pathname)"
	     (class-name (class-of self)))))

(defmethod start ((self logger-application))
  (with-slots (log-stream) self
    (setf log-stream (make-core-stream
		      (open (logger-application.log-pathname self)
			    :direction :output
			    :element-type '(unsigned-byte 8)
			    :if-exists :append
			    :if-does-not-exist :create
			    :external-format :utf8)))))

(defmethod stop ((self logger-application))
  (with-slots (log-stream) self
    (close-stream log-stream)
    (setf log-stream nil)))

(defmethod log-me ((self logger-application) (tag symbol) (message string))
  (aif (application.server self)
       (logger-server.log it (logger-application.log-stream self) tag message)
       (log! (logger-application.log-stream self) tag message)))

;; -------------------------------------------------------------------------
;; Logger Web Application Override
;; -------------------------------------------------------------------------
(defmethod logger-application.log-pathname ((self web-application))
  (merge-pathnames (merge-pathnames (make-pathname :directory '(:relative "var" "log"))
				    (bootstrap:home))
		   (make-pathname :name (web-application.fqdn self)
				  :type "log")))

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
