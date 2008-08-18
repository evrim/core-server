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

(defclass+ database (server)
  ((database-directory :type pathname :accessor database.directory
		       :initarg database-directory
		       :initform (error "Please specify :database-directory"))
   (database-log-stream :type core-stream :accessor database.stream)
   (database-root :type hash-table :accessor database.root
		  :initform (make-hash-table))))

(defmethod database.transaction-log-pathname ((self database) &optional suffix)
  "Return the name of the transaction-log filename, optionally using a suffix"
  (merge-pathnames (make-pathname :name (format nil "transaction-log~@[-~a~]" suffix)
				  :type "log")
		   (database.directory self)))

(defmethod database.snapshot-pathname ((self database) &optional suffix)
  "Return the name of the snapshot filename, optionally using a suffix"
  (merge-pathnames (make-pathname :name (format nil "snapshot~@[-~a~]" suffix)
				  :type "log")
		   (database.directory self)))

(defmethod database.deserialize ((self database) (stream core-stream)
				 &optional (cache (serialization-cache)))
  (deserialize-xml stream cache))

(defmethod restore ((self database))
  (clrhash (database.root self))

  ;; Load Snapshot
  (when (probe-file (database.snapshot-pathname self))
    (with-core-stream (s (database.snapshot-pathname self))
      (setf (database.root self) (database.deserialize self s))))

  ;; Load Transaction Log
  (when (probe-file (database.transaction-log-pathname self))
    (with-core-stream (s (database.transaction-log-pathname self))
      (let ((cache (serialization-cache)))
	(do ((tx (database.deserialize self s cache) (database.deserialize self s cache)))
	    ((null tx) nil)
	  (execute self tx))))))

(defmethod start ((self database))  
  (ensure-directories-exist (database.directory self))
  (restore self)
  (setf (database.stream self)
	(make-core-stream
	 (open (database.transaction-log-pathname self)
	       :direction :output :if-does-not-exist :create :if-exists :append))))

(defmethod stop ((self database))
  (clrhash (database.root self))
  (close-stream (database.stream self))
  (setf (database.stream self) nil))

