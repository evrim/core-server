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

;;;;
;;;; Usage:
;;;;
;;
;; for console output:
;; (defparameter *logger* (make-instance 'logger-server :log-stream *standard-output*))
;;
;; for file output (to $CORESERVER_HOME/var/log/core-server.log):
;; (defparameter *logger* (make-instance 'logger-server :log-stream nil))

;; log lines are like that:
;; <time> <tag> <message>
(defmethod/unit log-me :async-no-return ((self logger-server) tag message)
  (string! (log-stream self) (time->string (get-universal-time) :short))
  (char! (log-stream self) #\Space)
  (string! (log-stream self) (symbol-name tag))
  (char! (log-stream self) #\Space)
  (string! (log-stream self) message)
  (char! (log-stream self) #\Newline)
  (when (typep (log-stream self) 'core-fd-io-stream)
    (force-output (slot-value (log-stream self) '%stream))))

;; raw logging with newline at the end of the message
(defmethod/unit log-me-raw :async-no-return ((self logger-server) message)
  (string! (log-stream self) message)
  (char! (log-stream self) #\Newline)
  (when (typep (log-stream self) 'core-fd-io-stream)
    (force-output (slot-value (log-stream self) '%stream))))

;; Start logger unit
(defmethod start ((self logger-server))
  (unless (log-stream self)
    (setf (log-stream self)
	  (make-core-stream
	   (open (make-pathname :directory (pathname-directory (log-path self))
				:name "core-server"
				:type "log")
		 :direction :output
		 :element-type '(unsigned-byte 8)
		 :if-exists :append
		 :if-does-not-exist :create
		 :external-format :utf8)))))

;; Stop logger unit
(defmethod stop ((self logger-server))
  (when (and (log-stream self) 
	     (typep (log-stream self) 'core-fd-io-stream))
    (close-stream (log-stream self))
    (setf (log-stream self) nil)))

(defmethod status ((self logger-server))
  (if (log-stream self) t))
