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

(defclass+ transaction ()
  ((code :initarg :code :host local)
   (env :initarg :env :host local))
  (:ctor (code env))
  (:documentation "A simple Transaction object joining a function and
  its arguments"))

(defprint-object (self transaction :identity t :type t)
  (format t "(~A~{ ~@S~})" (transaction.code self)
	  (transaction.env self)))

(defclass+ database (server)
  ((database-directory :type pathname :accessor database.directory
		       :initarg :database-directory
		       :initform (error "Please specify :database-directory"))
   (database-log-stream :type core-stream :accessor database.stream)
   (database-root :type hash-table :accessor database.root
		  :initform (make-hash-table))
   (database-cache :type serialization-cache
		   :initform (serialization-cache)
		   :accessor database.cache)
   (database-closure-cache :type hash-table
			   :initform (make-hash-table)
			   :accessor database.closure-cache))
  (:default-initargs :name "Database Server"))

(defmethod database.transaction-log-pathname ((self database) &optional suffix)
  "Return the name of the transaction-log filename, optionally using a suffix"
  (merge-pathnames (make-pathname :name (format nil "transaction-log~@[-~a~]" suffix)
				  :type "xml")
		   (database.directory self)))

(defmethod database.snapshot-pathname ((self database) &optional suffix)
  "Return the name of the snapshot filename, optionally using a suffix"
  (merge-pathnames (make-pathname :name (format nil "snapshot~@[-~a~]" suffix)
				  :type "xml")
		   (database.directory self)))

(defmethod database.deserialize ((self database) (stream core-stream))
  (deserialize-xml stream (database.cache self)))

(defmethod database.serialize ((self database) (stream core-stream) (transaction transaction)) 
  (serialize-xml stream transaction (database.cache self)))

(defun find-free-variables (form)
  (let (lst)
    (mapc (lambda (vars)
	    (pushnew (name vars) lst))
	  (ast-search-type form 'free-variable-reference))
    lst))

(defmacro with-transaction ((server) &body body)
  (with-unique-names (tx)
    (let* ((form (walk-form `(progn ,@body)))
	   (vars (filter (lambda (a)
			   (not (eq server a)))
			 (find-free-variables form)))
	   (thunk (walk-form `(lambda (,server ,@vars)
				,@body))))
      (describe thunk)
      `(let ((,tx (transaction ,thunk (list ,@vars))))
	 (execute ,server ,tx)))))

(defmethod log-transaction ((self database) (transaction transaction))
  (checkpoint-stream (database.stream self))
  (database.serialize self (database.stream self) transaction)
  (commit-stream (database.stream self)))

(defmethod execute-on ((self database) (tx transaction))
  (etypecase (transaction.code tx)
    (symbol
     (if (not (fboundp (transaction.code tx)))
	 (error "Transactional function ~A is unbound." (transaction.function tx))
	 (apply (transaction.code tx) (transaction.env tx))))
    (lambda-function-form     
     (let ((fun (or (gethash (transaction.code tx) (database.closure-cache self))
		    (setf (gethash (transaction.code tx) (database.closure-cache self))
			  (eval (unwalk-form (transaction.code tx)))))))
       (describe tx)
       (describe (transaction.code tx))
       (apply fun self (transaction.env tx))))))

(defmethod execute ((self database) (transaction transaction))
  "Execute a transaction on a system and log it to the transaction log"
  (let ((result
	 (handler-bind ((error #'(lambda (condition)
				   (format *standard-output* 
                                             ";; Notice: system rollback/restore due to error (~a)~%" 
                                             condition)
				   (restore self))))
	   (execute-on self transaction))))
    (log-transaction self transaction)
    result))

(defmethod restore ((self database))
  (clrhash (database.root self))

  ;; Load Snapshot
  (when (probe-file (database.snapshot-pathname self))
    (with-core-stream (s (database.snapshot-pathname self))
      (aif (database.deserialize self s)
	   (setf (database.root self) it))))

  ;; Load Transaction Log
  (when (probe-file (database.transaction-log-pathname self))
    (with-core-stream (s (database.transaction-log-pathname self))      
      (do ((tx (database.deserialize self s) (database.deserialize self s)))
	  ((null tx) nil)
	(execute-on self tx)))))

(defmethod start ((self database))
  (when (not (equal (directory-namestring (database.directory self))
		    (namestring (database.directory self))))
    (error "Database directory is a filename, please check. ~A"
	   (database.directory self)))
  
  (when (not (database.status self))
    (ensure-directories-exist (database.directory self))
    (restore self)
    (setf (database.stream self)
	  (make-core-stream
	   (open (database.transaction-log-pathname self)
		 :direction :output :if-does-not-exist :create :if-exists :append
		 :element-type '(unsigned-byte 8))))))

(defmethod stop ((self database))
  (when (database.status self)
    (clrhash (database.root self))
    (clrhash (database.closure-cache self))
    (setf (database.cache self) (serialization-cache))
    (when (database.stream self)
      (close-stream (database.stream self))
      (setf (database.stream self) nil))))

(defmethod database.status ((self database))
  (not (null (database.stream self))))

(defmethod status ((self database))
  (database.status self))