(in-package :manager)

;; -------------------------------------------------------------------------
;; Socket Server
;; -------------------------------------------------------------------------
(defwebcrud <manager:socket-server/crud ()
  ((host-port :label "Bind Address")
   (protocol :label "Protocol")
   (peers-max :label "# of Peers" :remote-type number)
   (peer-class :label "Peer Class"))
  (:default-initargs :title nil :editable-p nil :deletable-p nil))

;; -------------------------------------------------------------------------
;; Socket  Component
;; -------------------------------------------------------------------------
(defcomponent <manager:socket-server ()
  ((_server :host lift :type core-server::socket-server)
   (host-port :host remote)
   (protocol :host remote)
   (peers-max :host remote :lift t)
   (peer-class :host remote))
  (:ctor %make-socket-server))

(defun <manager:socket-server (&key server)
  (with-slots (host port peers-max peer-class protocol) server
    (%make-socket-server :_server server
			 :host-port (format nil "~A:~A" host port)
			 :protocol (symbol->js protocol)
			 :peer-class (symbol->js (car peer-class)))))

