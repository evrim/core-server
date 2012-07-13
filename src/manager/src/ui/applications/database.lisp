(in-package :manager)

(defwebcrud database-server/crud ()
  ((database-directory :label "Database Directory")
   (log-pathname :label "Transaciton Log")
   (snapshot-pathname :label "Snapshot"))
  (:default-initargs :editable-p nil :deletable-p nil
		     :title "Database Properties"))

(defcomponent database-server/view (<:div)
  ((%database-server :host lift :type database-server
		     :reader %database-server :initarg :database-server)
   (database-directory :host remote)
   (log-pathname :host remote)
   (snapshot-pathname :host remote)
   (crud :host remote :initform (database-server/crud)))
  (:ctor %make-database-server/view))

(defun make-database-server/view (&key database-server)
  (%make-database-server/view :database-server database-server
			      :database-directory
			      (namestring
			       (database.directory database-server))
			      :log-pathname
			      (namestring
			       (core-server::database.transaction-log-pathname
				database-server))
			      :snapshot-pathname
			      (namestring
			       (core-server::database.snapshot-pathname
				database-server))))

(defmethod/remote init ((self database-server/view))  
  (append self (make-component (crud self) :instance self))
  (call-next-method self))