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
  (:default-intiargs :title "Server Info" :editable-p nil :deletable-p nil))

;; -------------------------------------------------------------------------
;; Info Component
;; -------------------------------------------------------------------------
(defcomponent <manager:server (<widget:simple)
  ((crud :host remote :initform (<manager:server/crud))
   (_tab :host remote :initform (<core:tab))))

(defmethod/local get-server-info ((self <manager:server))
  (let ((server (application.server application)))
    (with-slots (name auto-start debug) server
      (jobject :name name
	       :hostname (hostname)
	       :memory (format nil "~10:D MB" (sb-kernel::dynamic-usage))
	       :date (get-universal-time)
	       :auto-start auto-start
	       :debug debug))))

(defmethod/cc get-server-tab ((self <manager:server) class)
  (let ((server (application.server (component.application self))))
    (case class
      (socket-server
       (let ((_server (<manager:socket-server :server server)))
	 (list "Socket Server"
	       (<manager:socket-server/crud :instance _server))))
      (database-server
       (let ((_server (<manager:database-server :server server)))
	 (list "Database Server"
	       (<manager:database-server/crud :instance _server))))
      (mail-sender
       (let ((_server (<manager:mail-sender :server server)))
	 (list "Mail Gateway"
	       (<manager:mail-sender/crud :instance _server)))))))

(defmethod/local get-server-tabs ((self <manager:server))
  (let ((server (application.server application)))
    (remove-if #'null
	       (mapcar (lambda (class) (get-server-tab self class))
		       (mapcar #'class-name
			       (class+.superclasses (class-of server)))))))

(defmethod/remote init ((self <manager:server))
  (call-next-method self)
  (append self (make-component (crud self) :instance (get-server-info self)))
  (append self (make-component (_tab self)
			       :tabs (mapcar-cc (lambda (c)
						  (destructuring-bind (a b) c
						    (list a (make-component b))))
						(get-server-tabs self))
			       :tab-title "Capabilities")))

