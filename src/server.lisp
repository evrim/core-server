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
;;| Server Implementation
;;+----------------------------------------------------------------------------
(defclass+ server ()
  ((name :accessor server.name :initarg :name :initform "Abstract Server"
	 :documentation "Name of the server" :host local)
   (mutex :accessor server.mutex :initarg :mutex :initform (sb-thread:make-mutex :name "Server mutex")
	  :documentation "Lock used to synchronize some operations on server"
	  :host none)
   (auto-start :accessor server.auto-start :initarg :auto-start :initform nil
	       :documentation "If t, the server would be started when created"
	       :host local)
   (debug  :accessor server.debug :initarg :debug :initform t
	   :documentation "Debugging flag of generic server"
	   :host local))
  (:documentation "Server Base Class"))

(defmethod print-object ((self server) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "\"~A\" is~A running." (server.name self)
	    (if (status self) "" " *not*"))))

(defmacro with-server-mutex (server &body body)
  "Execute 'body' while holding 'server' lock"
  `(sb-thread:with-recursive-lock ((server.mutex ,@server))
     ,@body))

(defmacro with-server-lock (server &body body)
  "Execute 'body' while holding 'server' lock"
  `(sb-thread:with-recursive-lock ((server.mutex ,@server))
     ,@body))

(defmacro defsynhronized (name args &body body)
  `(progn
     (redefmethod ,name :around ,args
       (with-server-lock (,(caar args))
	  (call-next-method)))
     (redefmethod ,name ,args ,@body)))

;; +----------------------------------------------------------------------------
;; | Auto Start Feature for Servers
;; +----------------------------------------------------------------------------
(defmethod shared-initialize :after ((self server) slot-name &rest initargs
				     &key &allow-other-keys)
  (declare (ignore initargs))
  (when (server.auto-start self)
    (start self)))

;;-----------------------------------------------------------------------------
;; Wrapper :around methods for protocol
;;-----------------------------------------------------------------------------
;;
;; They wrap protocol methods to detect errors and implement appropriate
;; restarts.
;;
(defmethod start :around ((self server))
  (with-server-mutex (self)
    (let ((failed))
      (unwind-protect
	(restart-case
	    (let ((swank::*sldb-quit-restart* 'give-up))
	      (setf failed t)
	      (multiple-value-prog1 (call-next-method)
		(setf failed nil)))
	  (give-up ()
	    :report "Give up starting server."
	    (format t "Giving up.~%"))
	  (try-again ()
	    :report "Try again."
	    (start self)))
	(when failed
	  (format t "stopping server.~%")
	  (stop self))))))

(defmethod stop :around ((self server))
  (with-server-mutex (self)
    (call-next-method)))

(defmethod register :around ((self server) (app application))
  (with-server-mutex (self)
    (let ((failed))
      (unwind-protect
	(restart-case
	    (let ((swank::*sldb-quit-restart* 'give-up))
	      (setf failed t)
	      (call-next-method)
	      (setf failed nil)
	      (setf (application.server app) self))
	  (give-up ()
	    :report "Give up registering app."
	    (format t "Giving up.~%"))
	  (try-again ()
	    :report "Try again."
	    (start self)))
	(when failed
	  (format t "Unregistering application.")
	  (unregister self app))))))

(defmethod unregister :around ((self server) (app application))
  (call-next-method)
  (setf (application.server app) nil))

(defmethod shared-initialize :after ((self server) slot-names
				     &rest initargs &key &allow-other-keys)
  "If auto-start slot of the server is t, start server"
  (declare (ignore initargs))
  (when (s-v 'auto-start)
    (start self)))

(defmethod stop-start ((self server))
  (stop self)
  (start self))

;; +-------------------------------------------------------------------------
;; | Web Server
;; +-------------------------------------------------------------------------
(defclass+ web-server (server)
  ()
  (:documentation "Web Server Base Class"))

