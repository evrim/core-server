(in-package :manager)

;; -------------------------------------------------------------------------
;; Server Crud
;; -------------------------------------------------------------------------
(defwebcrud <manager:server/crud ()
  ((name :label "Server Name")
   (hostname :label "Hostname")
   (memory :label "Memory Usage")
   (date :label "Server Timestamp" :remote-type timestamp)
   (auto-start :label "Autostart?" :remote-type checkbox)
   (debug :label "In debug mode?" :remote-type checkbox))
  (:default-intiargs :title "Server Info" :editable-p t :deletable-p nil))

;; -------------------------------------------------------------------------
;; Info Component
;; -------------------------------------------------------------------------
(defcomponent <manager:server-info (<widget:simple)
  ((crud :host remote :initform (<manager:server/crud))))

(defmethod/local get-server-info ((self <manager:server-info))
  (let ((server (application.server application)))
    (with-slots (name auto-start debug) server
      (jobject :name name
	       :hostname (hostname)
	       :memory (format nil "~10:D MB" (sb-kernel::dynamic-usage))
	       :date (get-universal-time)
	       :auto-start auto-start
	       :debug debug))))

(defmethod/remote init ((self <manager:server-info))
  (append self (make-component (crud self) :instance (get-server-info self))))

;; -------------------------------------------------------------------------
;; Socket Server
;; -------------------------------------------------------------------------
(defwebcrud <manager:socket-server/crud ()
  ((host-port :label "Bind Address")
   (protocol :label "Protocol")
   (peers-max :label "# of Peers")
   (peer-class :label "Peer Class"))
  (:default-initargs :title "Socket Server Info"))

;; -------------------------------------------------------------------------
;; Socket  Component
;; -------------------------------------------------------------------------
(defcomponent <manager:socket-server-info (<widget:simple)
  ((crud :host remote :initform (<manager:socket-server/crud))))

(defmethod/local get-server-info ((self <manager:socket-server-info))
  (let ((server (application.server application)))
    (with-slots (host port peers-max peer-class protocol) server
      (jobject :host-port (format nil "~A:~A" host port)
	       :peers-max peers-max
	       :protocol (symbol->js protocol)
	       :peer-class (symbol->js (car peer-class))))))

(defmethod/remote init ((self <manager:socket-server-info))
  (append self (make-component (crud self) :instance (get-server-info self))))

;; -------------------------------------------------------------------------
;; Database Server
;; -------------------------------------------------------------------------
(defwebcrud <manager:database-server/crud ()
  ((database-directory :label "Database Directory")
   (log-pathname :label "Transaciton Log")
   (snapshot-pathname :label "Snapshot"))
  (:default-initargs :editable-p nil :deletable-p nil
		     :title "Database Server Info"))

(defcomponent database-server/view (<:div)
  ((%database-server :host lift :type database-server
		     :reader %database-server :initarg :database-server)
   (database-directory :host remote)
   (log-pathname :host remote)
   (snapshot-pathname :host remote))
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

;; -------------------------------------------------------------------------
;; Database Server Info
;; -------------------------------------------------------------------------
(defcomponent <manager:database-server-info (<widget:simple)
  ((crud :host remote :initform (<manager:database-server/crud))))

(defmethod/local get-server-info ((self <manager:database-server-info))
  (make-database-server/view :database-server
			     (application.server application)))

(defmethod/remote init ((self <manager:database-server-info))
  (append self (make-component (crud self) :instance
			       (make-component (get-server-info self)))))


;; -------------------------------------------------------------------------
;; Mail Sender
;; -------------------------------------------------------------------------
(defwebcrud <manager:mail-sender/crud ()
  ((server :label "Mail Server")
   (port :label "SMTP Port" :remote-type number)
   (ssl :label "Enable TLS?" :remote-type checkbox)
   (username :label "Username")
   (password :label "Password" :remote-type password))
  (:default-initargs :title "Mail Gateway"))

(defcomponent <manager:mail-sender-info (<widget:simple)
  ((crud :host remote :initform (<manager:mail-sender/crud))))

(defmethod/local get-server-info ((self <manager:mail-sender-info))
  (with-slots (username password mail-port
			server ssl) (application.server application)
    (jobject :username username
	     :password password
	     :port mail-port
	     :ssl ssl
	     :server server)))

(defmethod/remote init ((self <manager:mail-sender-info))
  (append self (make-component (crud self) :instance
			       (get-server-info self))))