(in-package :manager)

;; -------------------------------------------------------------------------
;; Mail Sender
;; -------------------------------------------------------------------------
(defwebcrud <manager:mail-sender/crud ()
  ((server :label "Mail Server")
   (port :label "SMTP Port" :remote-type number)
   (ssl :label "Enable TLS?" :remote-type checkbox)
   (username :label "Username")
   (password :label "Password" :remote-type password))
  (:default-initargs :title nil :editable-p nil :deletable-p nil))

(defcomponent <manager:mail-sender ()
  ((username :host remote)
   (password :host remote)
   (port :host remote)
   (ssl :host remote)
   (server :host remote :accessor mail-sender.server))
  (:ctor %make-mail-sender))

(defun <manager:mail-sender (&key server)
  (with-slots (username password mail-port server ssl) server
    (%make-mail-sender :username username
		       :password password
		       :port mail-port		       
		       :ssl ssl
		       :server server)))
