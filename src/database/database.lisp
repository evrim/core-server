(in-package :core-server)

;; +----------------------------------------------------------------------------
;; | Prevalence Database Reworked
;; +----------------------------------------------------------------------------
(defgeneric database.serialize (database object &optional k)
  (:documentation "Serialize object"))

(defgeneric database.deserialize (self object &optional k)
  (:documentation "Deserialize object"))

(defclass transaction ()
  ((code :initarg :code :accessor transaction.code)
   (env :initarg :env :accessor transaction.env))
  (:documentation "A simple Transaction object joining a function and
  its arguments"))

(defun transaction (code &rest env)
  (make-instance 'transaction :code code :env env))

(defprint-object (self transaction :identity t :type t)
  (format t "(~A~{ ~@S~})" (transaction.code self)
	  (transaction.env self)))

(defclass+ serialization-cache ()
  ((counter :initform 0)
   (cache :initform (make-hash-table :weakness :key-or-value)))
  (:ctor ()))

(defclass+ abstract-database (server)
  ((database-directory :type pathname :accessor database.directory
		       :initarg :database-directory
		       :initform nil :host local)
   (database-log-stream :type core-stream :accessor database.stream
			:initform nil :host none)
   (database-root :type hash-table :accessor database.root
		  :initform (make-hash-table :test #'equal)
		  :host none)
   (database-cache :type serialization-cache
		   :initform (serialization-cache)
		   :accessor database.cache
		   :host none)
   (database-closure-cache :type hash-table
			   :initform (make-hash-table)
			   :accessor database.closure-cache
			   :host none))
  (:default-initargs :name "Database Server"))

(defmethod database.transaction-log-pathname ((self abstract-database) &optional suffix)
  "Return the name of the transaction-log filename, optionally using a suffix"
  (merge-pathnames (make-pathname :name (format nil "transaction-log~@[-~a~]" suffix)
				  :type "xml")
		   (database.directory self)))

(defmethod database.snapshot-pathname ((self abstract-database) &optional suffix)
  "Return the name of the snapshot filename, optionally using a suffix"
  (merge-pathnames (make-pathname :name (format nil "snapshot~@[-~a~]" suffix)
				  :type "xml")
		   (database.directory self)))

;; -------------------------------------------------------------------------
;; Serialization Overrides
;; -------------------------------------------------------------------------
(defgeneric database.serialize (db object &optional k)
  (:method ((self abstract-database) object &optional k)
    (xml-serialize object (or k (curry #'database.serialize self))))
  (:method ((self abstract-database) object &optional k)
    (xml-serialize object (or k (curry #'database.serialize self))))
  (:method ((self abstract-database) (object standard-class) &optional k)
    (xml-serialize object (or k (curry #'database.serialize self)))))

;; -------------------------------------------------------------------------
;; Deserialization Overrides
;; -------------------------------------------------------------------------
(defgeneric database.deserialize (db object &optional k)
  (:method ((self abstract-database) object &optional k)
    (xml-deserialize object (or k (curry #'database.deserialize self))))
  (:method ((self abstract-database) (object <db:class) &optional k)
    (xml-deserialize object (or k (curry #'database.deserialize self))))
  (:method ((self abstract-database) (object null) &optional k)
    (declare (ignore k)) nil))

;; -------------------------------------------------------------------------
;; Clone Interface
;; -------------------------------------------------------------------------
(defgeneric database.clone (db object)
  (:method ((self abstract-database) object) object)
  (:method ((self abstract-database) (object list))
    (mapcar (lambda (a) (database.clone self a)) object)))

;; ----------------------------------------------------------------------------
;; Transaction
;; ----------------------------------------------------------------------------
(defxml <db:transaction)

(defmethod database.deserialize ((self abstract-database) (xml <db:transaction)
				 &optional (k (curry #'database.deserialize self)))
  (with-slots (class children) xml
    (let ((instance (allocate-instance (find-class 'transaction))))
      (initialize-instance
       (reduce (lambda (instance slot)
		 (multiple-value-bind (name value) (funcall k slot k)
		   (setf (slot-value instance name) value)
		   instance))
	       children :initial-value instance)))))

(defmethod database.serialize ((self abstract-database) (object transaction)
			       &optional (k (curry #'database.serialize self)))
  (<db:transaction
   (mapcar (lambda (slot)
	     (<db:slot :name (symbol->string slot)
		       (funcall k (slot-value object slot) k)))
	   (slots-of object))))

;; -------------------------------------------------------------------------
;; Reference Object for Graph Detection
;; -------------------------------------------------------------------------
(defxml <db:ref id)

(defmethod database.deserialize ((self abstract-database) (object <db:ref) &optional k)
  (declare (ignore k))
  (with-slots (id) object
    (let ((id (read-from-string id)))
      (with-slots (cache) (database.cache self)
	(multiple-value-bind (object foundp) (gethash id cache)
	  (if foundp
	      object
	      (prog1 nil
		(warn "reference to id ~A is not found, relations are broken now." id))))))))

;; -------------------------------------------------------------------------
;; Instance Serialization Override
;; -------------------------------------------------------------------------
(defmethod database.deserialize ((self abstract-database) (xml <db:instance) &optional k)
  (let ((k (or k (curry #'database.deserialize self))))
    (with-slots (class children id) xml
      (let ((instance (allocate-instance (find-class (read-from-string class))))
	    (id (read-from-string id)))
	(with-slots (cache counter) (database.cache self)
	  (setf (gethash id cache) instance counter (max counter id))
	  (apply #'initialize-instance
		 (reduce (lambda (instance slot)
			   (multiple-value-bind (name value) (funcall k slot k)
			     (setf (slot-value instance name) value)
			     instance))
			 children
			 :initial-value instance)
		 (reduce0 (lambda (acc atom) (cons (car atom) (cons (cadr atom) acc)))
			  (class-default-initargs (class-of instance)))))))))

(defmethod database.serialize ((self abstract-database) (object standard-object)
			       &optional (k (curry #'database.serialize self)))
  (with-slots (cache counter) (slot-value self 'database-cache)
    (multiple-value-bind (id foundp) (gethash object cache)
      (if foundp
	  (<db:ref :id (format nil "~D" id))
	  (let ((counter (incf counter)))
	    (setf (gethash object cache) counter)
	    (<db:instance :class (symbol->string (class-name (class-of object)))
			  :id counter
			  (mapcar (lambda (slot)
				    (<db:slot :name (symbol->string slot)
					      (funcall k (slot-value object slot) k)))
				  (slots-of object))))))))


;; -------------------------------------------------------------------------
;; HashMap & Structure Serialization Override
;; -------------------------------------------------------------------------
(defmacro %defserialization-cache (type xml-type)
  `(progn
     (defmethod database.serialize ((self abstract-database) (object ,type)
				    &optional (k (curry #'database.serialize self)))
       (with-slots (cache counter) (slot-value self 'database-cache)
	 (multiple-value-bind (id foundp) (gethash object cache)
	   (if foundp
	       (<db:ref :id (format nil "~D" id))
	       (let ((counter (incf counter)))
		 (setf (gethash object cache) counter)
		 (let ((xml (xml-serialize object k)))
		   (setf (slot-value xml 'id) counter)
		   xml))))))
     (defmethod database.deserialize ((self abstract-database) (xml ,xml-type)
				      &optional (k (curry #'database.deserialize self)))
       (declare (ignore k))
       (let ((object (call-next-method)))
	 (setf (gethash (parse-integer (slot-value xml 'id))
			(slot-value (database.cache self) 'cache))
	       object)
	 object))))

(%defserialization-cache hash-table <db:hash-table)
(%defserialization-cache structure-object <db:struct)

(defmethod log-transaction ((self abstract-database) (transaction transaction))
  (checkpoint-stream (database.stream self))
  (write-stream (database.stream self) (database.serialize self transaction))
  (commit-stream (database.stream self)))

(defmethod execute-on ((self abstract-database) (tx transaction))
  (etypecase (transaction.code tx)
    (symbol ;; Function Call
     (if (not (fboundp (transaction.code tx)))
	 (error "Transactional function ~A is unbound." (transaction.code tx))
	 (apply (transaction.code tx) (cons self (transaction.env tx)))))
    (list ;; Setf Call
     (apply (eval `(function ,(transaction.code tx)))
	    (cons (car (transaction.env tx))
		  (cons self (cdr (transaction.env tx))))))
    (lambda-function-form ;; Lambda Function Call
     (let ((fun (or (gethash (transaction.code tx) (database.closure-cache self))
		    (setf (gethash (transaction.code tx) (database.closure-cache self))
			  (handler-bind ((style-warning (lambda (c) (muffle-warning c))))
			    (eval (unwalk-form (transaction.code tx))))))))
       (apply fun self (transaction.env tx))))))

(defvar +transactionalp+ nil)
(defmethod execute ((self abstract-database) (transaction transaction))
  "Execute a transaction on a system and log it to the transaction log"
  (flet ((execute ()
	   ;; (handler-bind ((error #'(lambda (condition)
;;	   (format *standard-output* "Notice: system rollback/restore due to error (~a)~%" condition)
;; 				   (restore self))))
;; 	   (execute-on self transaction))
	   (execute-on self transaction)
	   ))
    (cond
      (+transactionalp+
       (execute))
      (t
       (unless (database.status self)
	 (error "Database should be running in order to execute a transaction"))
       
       (let* ((+transactionalp+ t))
	 (prog1 (execute)
	   (log-transaction self transaction)))))))

(defmethod restore ((self abstract-database))
  (clrhash (database.root self))
  (setf (database.cache self) (serialization-cache))
    
  ;; Load Snapshot
  (when (probe-file (database.snapshot-pathname self))
    (with-core-stream (s (database.snapshot-pathname self))
      (let ((xml (read-stream (make-xml-stream s))))
	(aif (and xml (database.deserialize self xml))
	     (setf (database.root self) it)))))

  (let ((cache (slot-value (database.cache self) 'cache)))
    (maphash (lambda (k v) (setf (gethash v cache) k))
	     cache))
  
  ;; Load Transaction Log
  (when (probe-file (database.transaction-log-pathname self))
    (let ((+transactionalp+ t))
      (with-core-stream (s (database.transaction-log-pathname self))
	(let ((s (make-xml-stream s)))	  
	  (do* ((xml (read-stream s) (read-stream s))
		(tx (database.deserialize self xml) (database.deserialize self xml)))
	      ((null xml) nil)
	    (execute self tx)))))))

(defun open-database-stream (pathname)
  (make-xml-stream
   (make-indented-stream
    (make-core-stream
     (open pathname
	   :direction :output :if-does-not-exist :create :if-exists :append
	   :element-type '(unsigned-byte 8))))))

(defmethod snapshot ((self abstract-database))
  (unless (database.status self)
    (error "Database should be running to get a snapshot"))
  
  (let ((timestamp (time->string (get-universal-time) :tag))
	(snapshot (database.snapshot-pathname self))
	(tx-log (database.transaction-log-pathname self)))

    (close-stream (database.stream self))

    (when (probe-file snapshot)
      (cp :from snapshot :to (database.snapshot-pathname self timestamp)))

    (let ((stream (make-xml-stream
		   (make-indented-stream
		    (make-core-stream
		     (open (database.snapshot-pathname self)
			   :direction :output :if-does-not-exist :create :if-exists :supersede
			   :element-type '(unsigned-byte 8)))))))
      
      (setf (database.cache self) (serialization-cache))
      (unwind-protect (write-stream stream (database.serialize self (database.root self)))
	(close-stream stream)))

    (when (probe-file tx-log)
      (cp :from tx-log :to (database.transaction-log-pathname self timestamp))
      (rm :path tx-log))
    
    (restore self)
    (setf (database.stream self) (open-database-stream tx-log))

    (values (database.snapshot-pathname self)
	    (database.snapshot-pathname self timestamp))))


(defmethod purge ((self abstract-database))
  (let ((state (database.status self)))
    (if state (stop self))
    (if (probe-file (database.directory self))
	(rm :args '("-r") :path (database.directory self)))
    (if state (start self))
    self))

(defmethod database.status ((self abstract-database))
  (not (null (database.stream self))))

(defclass+ database (abstract-database)
  ())

(defprint-object (self database)
  (format t "Status: ~A" (status self)))

(defclass+ database-server (database)
  ())

(defmethod start ((self database))
  (if (null (database.directory self))
      (error "Please set database directory"))
  
  (when (not (equal (directory-namestring (database.directory self))
		    (namestring (database.directory self))))
    (error "Database directory is a filename, please check. ~A"
	   (database.directory self)))  
  
  (when (not (database.status self))
    (setf (database.cache self) (serialization-cache))
    (ensure-directories-exist (database.directory self))
    (restore self)
    (setf (database.stream self)
	  (open-database-stream (database.transaction-log-pathname self)))))

(defmethod stop ((self database))
  (when (database.status self)
    (clrhash (database.root self))
    (clrhash (database.closure-cache self))
    (setf (database.cache self) (serialization-cache))
    (when (database.stream self)
      (close-stream (database.stream self))
      (setf (database.stream self) nil))))

(defmethod status ((self database))
  (database.status self))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun find-free-variables (form)
    (let (lst)
      (mapc (lambda (vars)
	      (pushnew (name vars) lst))
	    (ast-search-type form 'free-variable-reference))
      (nreverse lst))))

(defmacro with-transaction ((server) &body body)
  (with-unique-names (tx)
    (let* ((form (walk-form `(progn ,@body)))
	   (vars (filter (lambda (a)
			   (not (eq server a)))
			 (find-free-variables form)))
	   (thunk (walk-form `(lambda (,server ,@vars)
				,@body))))  
      `(let ((,tx (transaction ,thunk ,@vars)))
	 (execute ,server ,tx)))))

(defmacro deftransaction (name args &body body)
  (let* ((arg-names (extract-argument-names args :allow-specializers t))
	 (setf-p (and (listp name) (eq 'setf (car name)))))
    `(progn
       (warn "deftransaction overrides :around method")
       (defmethod ,name :around ,args
	 (if +transactionalp+
	     (call-next-method)
	     (execute ,(if setf-p (cadr arg-names) (car arg-names))
		      ,(if (member '&rest args )
			   `(apply #'transaction
				   ',name ,@(if setf-p
						(cons (car arg-names) (cddr arg-names))
						(cdr arg-names)))
			  `(transaction ',name ,@(if setf-p
						     (cons (car arg-names) (cddr arg-names))
						     (cdr arg-names)))))))
       (defmethod ,name ,args ,@body))))

;; (defmacro deftransaction (name args &body body)
;;   `(defmethod ,name ,args
;;      (with-transaction (,(let ((arg-names (extract-argument-names args :allow-specializers t)))
;; 			      (if (and (listp name) (eq 'setf (car name)))
;; 				  (cadr arg-names)
;; 				  (car arg-names))))
;;        ,@body)))

(deftransaction database.get ((server database) key)
  (gethash key (database.root server)))

(deftransaction (setf database.get) (value (server database) key)
  (setf (gethash key (database.root server)) value))

(deftrace database '(execute execute-on snapshot restore start stop
		     database.serialize database.deserialize
		     xml-serialize xml-deserialize log-transaction))

;; TODO: implement file truncation on bad transaction log

;; Core Server: Web Application Server

;; Copyright (C) 2006-2008  Metin Evrim Ulu

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

;; (defmacro deftransaction (name args &body body)
;;   (let* ((arg-names (extract-argument-names args :allow-specializers t))
;; 	 (setf-p (and (listp name) (eq 'setf (car name))))
;; 	 (tx (intern (if setf-p
;; 			 (format nil "TX-SETF-~A" (cadr name))
;; 			 (format nil "TX-~A" name)))))    
;;     `(progn
;;        (defun ,tx ,(if setf-p
;; 		       (cons (cadr arg-names) (cons (car arg-names) (cddr arg-names)))
;; 		       arg-names)
;; 	 ,@body)
;;        (defmethod ,name ,args
;; 	 (execute ,(if setf-p (cadr arg-names) (car arg-names))
;; 		  (transaction ',tx ,@(if setf-p
;; 					  (cons (car arg-names) (cddr arg-names))
;; 					  (cdr arg-names))))))))
