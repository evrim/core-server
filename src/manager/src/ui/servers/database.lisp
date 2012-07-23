(in-package :manager)

;; -------------------------------------------------------------------------
;; Database Server
;; -------------------------------------------------------------------------
(defwebcrud <manager:database-server/crud ()
  ((database-directory :label "Database Directory")
   (log-pathname :label "Transaciton Log")
   (snapshot-pathname :label "Snapshot"))
  (:default-initargs :editable-p nil :deletable-p nil :title nil))

(defcomponent <manager:database-server ()
  ((database-directory :host remote)
   (log-pathname :host remote)
   (snapshot-pathname :host remote))
  (:ctor %make-database-server))

(defun <manager:database-server (&key server)
  (%make-database-server :database-directory
			 (namestring (database.directory server))
			 :log-pathname
			 (namestring (database.transaction-log-pathname server))
			 :snapshot-pathname
			 (namestring (database.snapshot-pathname server))))
