(in-package :manager)

;; -------------------------------------------------------------------------
;; Server Crud
;; -------------------------------------------------------------------------
(defwebcrud <manager:server-info/crud ()
  ((name :label "Server Name")
   (hostname :label "Hostname")
   (memory :label "Memory Usage")
   (date :label "Server Timestamp" :remote-type timestamp)
   (apps :label "# of applications" :remote-type number)
   (auto-start :label "Autostart?" :remote-type checkbox)
   (debug :label "In debug mode?" :remote-type checkbox))
  (:default-intiargs :title "Server Info" :editable-p nil :deletable-p nil))

;; -------------------------------------------------------------------------
;; Info Component
;; -------------------------------------------------------------------------
(defcomponent <manager:server-info (<widget:simple)
  ((crud :host remote :initform (<manager:server-info/crud))))

(defmethod/local get-server-info ((self <manager:server-info))
  (let ((server (application.server application)))
    (with-slots (name auto-start debug) server
      (jobject :name name
	       :hostname (hostname)
	       :memory (format nil "~10:D MB" (sb-kernel::dynamic-usage))
	       :date (get-universal-time)
	       :auto-start auto-start
	       :debug debug
	       :apps (length (server.applications server))))))

(defmethod/remote init ((self <manager:server-info))
  (call-next-method self)
  (append self (make-component (crud self) :instance (get-server-info self))))

