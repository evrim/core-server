(in-package :tr.gen.core.server)

(defun start-prevalence-system (system)
  (with-slots (directory) system
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
  (with-slots (cl-prevalence::transaction-log-stream) system
    (unless cl-prevalence::transaction-log-stream
      (setf cl-prevalence::transaction-log-stream (open (cl-prevalence::get-transaction-log system)
					 :direction :output :if-does-not-exist :create :if-exists :append)))))

(defmethod cl-prevalence::initialize-instance :after ((system prevalence-system) &rest initargs &key &allow-other-keys)
  "After a system is initialized, derive its file paths and try to restore it"
  (declare (ignore initargs))
  (unless (typep system 'database-server)
    (start-prevalence-system system)))

(defun create-guard-with-mutex (mutex)
  #'(lambda (thunk)
      (sb-thread::with-mutex (mutex)
	(funcall thunk))))

(defmethod model ((system prevalence-system))
  (get-root-object system :model))

(defun tx-model-create (system model-class)
  (setf (get-root-object system :model) (make-instance model-class)))

(defmethod snapshot :after ((self database-server))
  (open-transaction-log-stream self))

(defmethod start ((self database-server))  
  (start-prevalence-system self)
  (open-transaction-log-stream self)
  ;; synchronize transactions with a mutex
  (setf (get-guard self) 
	(create-guard-with-mutex (sb-thread::make-mutex :name "tx-mutex")))
  ;; create model
  (unless (get-root-object self :model)
    ;; create global id
    (execute self (make-transaction 'tx-create-id-counter))
    ;; create a model
    (awhen (database-server.model-class self)
      (execute self (make-transaction 'tx-model-create it)))))

(defmethod stop ((self database-server))  
  (cl-prevalence::close-open-streams self)
  (setf (cl-prevalence::get-root-objects self) (make-hash-table :test 'eq))
  (setf (slot-value self 'cl-prevalence::serialization-state) (cl-prevalence::make-serialization-state))
  (setf (cl-prevalence::get-snapshot self) nil)
  (setf (cl-prevalence::get-transaction-log self) nil))

;;; This is here since i don't want the database to be
;;; corrupted. 
(defmethod %status ((self database-server))
  (and (slot-value self 'cl-prevalence::transaction-log-stream) t))

(defmethod status ((self database-server))
  (%status self))

(defun make-database (db-location model-class)
  (let ((system (make-prevalence-system db-location
					:prevalence-system-class 'guarded-prevalence-system
					:init-args '(:serializer cl-prevalence::serialize-sexp
						     :deserializer cl-prevalence::deserialize-sexp
						     :file-extension "sexp"))))
    ;; synchronize transactions with a mutex
    (setf (get-guard system) (create-guard-with-mutex (sb-thread::make-mutex :name "database-mutex")))	 
    ;; create model
    (unless (get-root-object system :model)
      ;; create global id
      (execute system (make-transaction 'tx-create-id-counter))
      ;; create a model
      (execute system (make-transaction 'tx-model-create model-class)))
    system))