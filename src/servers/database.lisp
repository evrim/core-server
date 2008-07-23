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
;;| Cl-Prevalence Database Server
;;+----------------------------------------------------------------------------
;;
;; This file contains base in-memory database server.
;;
(defun start-prevalence-system (system)
  "Restores and returns 'system'"
  (let ((directory (get-directory system)))    
    (ensure-directories-exist directory)
    (setf (cl-prevalence::get-snapshot system)
	  (merge-pathnames (make-pathname :name (cl-prevalence::get-snapshot-filename system) 
					  :type (cl-prevalence::get-file-extension system)) 
			   directory))
    (setf (cl-prevalence::get-transaction-log system) 
	  (merge-pathnames (make-pathname :name (cl-prevalence::get-transaction-log-filename system)
					  :type (cl-prevalence::get-file-extension system))
			   directory)))
  (restore system))

(defun open-transaction-log-stream (system)
  "Opens transaction log streams"
  (with-slots (cl-prevalence::transaction-log-stream) system
    (unless cl-prevalence::transaction-log-stream
      (setf cl-prevalence::transaction-log-stream (open (cl-prevalence::get-transaction-log system)
					 :direction :output :if-does-not-exist :create :if-exists :append)))))

(defmethod cl-prevalence::initialize-instance :after
    ((system prevalence-system) &rest initargs &key &allow-other-keys)
  "After a system is initialized, derive its file paths and try to restore it"
  (declare (ignore initargs))
  (unless (typep system 'database-server)
    (start-prevalence-system system)))

(defun create-guard-with-mutex (mutex)
  "Returns a lambda that would execute 'thunk' while holding lock 'mutext'"
  #'(lambda (thunk)
      (sb-thread::with-mutex (mutex)
	(funcall thunk))))

(defmethod model ((system prevalence-system))
  "Returns :model from 'system's root, can be nil"
  (get-root-object system :model))

(defun tx-model-create (system model-class)
  "Transaction function that is used to create an instance of
'model-class'. This instance is then set to :model keyword inside
'system's root"
  (setf (get-root-object system :model) (make-instance model-class)))

(defun tx-set-creation-date (system date)
  "Transaction function that is used to set creation date of the
model."
  (setf (standard-model-class.creation-date (get-root-object system :model)) date))

(defmethod snapshot :after ((self database-server))
  (open-transaction-log-stream self))

(defmethod shared-initialize :after ((self database-server) slot-name
				     &rest initargs &key &allow-other-keys)
  "Start database if auto-start it t"
  (declare (ignorable initargs))
  (when (database-server.db-auto-start self)
    (%start-database self)))

(defmethod %start-database ((self database-server))
  (start-prevalence-system self)
  (open-transaction-log-stream self)
  ;; synchronize transactions with a mutex
  (setf (get-guard self) 
	(create-guard-with-mutex (sb-thread::make-mutex :name "tx-mutex")))
  (unless (get-root-object self :id-counter)
    ;; create global id
    (execute self (make-transaction 'tx-create-id-counter)))
  ;; create model
  (unless (get-root-object self :model)
    ;; create a model
    (awhen (database-server.model-class self)
      (execute self (make-transaction 'tx-model-create it)))
    (when (typep (get-root-object self :model) 'standard-model-class)
      (execute self (make-transaction 'tx-set-creation-date (get-universal-time))))))

(defmethod start ((self database-server))
  "Starts database server"
  (%start-database self))

(defmethod stop ((self database-server))
  "Stops database server"
  (cl-prevalence::close-open-streams self)
  (setf (cl-prevalence::get-root-objects self) (make-hash-table :test 'eq))
  (setf (slot-value self 'cl-prevalence::serialization-state) (cl-prevalence::make-serialization-state))
  (setf (cl-prevalence::get-snapshot self) nil)
  (setf (cl-prevalence::get-transaction-log self) nil))

;;; This is here since i don't want the database to be
;;; corrupted. 
(defmethod database-server.status ((self database-server))
  (and (slot-value self 'cl-prevalence::transaction-log-stream) t))

(defmethod status ((self database-server))
  (database-server.status self))

(defun make-database (db-location model-class)
  "Create a new database at 'db-location' with an instance of 'model-class'"
  (let ((system (make-prevalence-system db-location
					:prevalence-system-class 'guarded-prevalence-system
					:init-args '(:serializer cl-prevalence::serialize-sexp
						     :deserializer cl-prevalence::deserialize-sexp
						     :file-extension "sexp"))))
    ;; synchronize transactions with a mutex
    (setf (get-guard system) (create-guard-with-mutex (sb-thread::make-mutex :name "database-mutex")))
    (unless (get-root-object system :id-counter)      
      ;; create global id
      (execute system (make-transaction 'tx-create-id-counter)))
    ;; create model
    (unless (get-root-object system :model)
      ;; create a model
      (execute system (make-transaction 'tx-model-create model-class)))
    system))

(defun update-slots (instance lst)
  (let ((clazz (class-of instance))
	(key nil))
    (mapc #'(lambda (item)
	      (cond
		((and (null key) (keywordp item))
		 (setf key item))
		((not (null key))		 
		 (let ((slot-name (intern (symbol-name key))))
		   (if (sb-pcl::find-slot-definition clazz slot-name)
		       (progn (setf (slot-value instance slot-name) item
				    key nil))
		       (error "slot ~A not found in class ~A" slot-name (class-name clazz)))))
		(t (error "Malformed update-slots slot-vals list"))))
	  lst)
    instance))

(defun filter-object (an-atom &optional (key #'get-id))
  (etypecase an-atom
    (object-with-id
     (funcall key an-atom))
    (standard-object
     (cerror "Continue?" "Unknown object found when entering transaction: ~A " an-atom)
     (funcall key an-atom))
    (t an-atom)))

(defun filter-objects (lst &optional (key #'get-id))
  (if (atom lst)
      (filter-object lst key)
      (nreverse
       (reduce #'(lambda (acc item)
                   (cons (filter-object item key) acc))
               lst :initial-value nil))))

(defun make-tx (function &rest args)
  (apply #'cl-prevalence::make-transaction function (filter-objects args)))
