(in-package :tr.gen.core.server)

(defclass socket-server (server)
  ((host :initarg :host :initform "0.0.0.0")
   (port :initarg :port :initform 3009)
   (reuse-address :initarg :reuse-address :initform t)
   (backlog :initarg :backlog :initform 1)
   (protocol :initarg :protocol :initform :tcp)   
   (peers-max :initarg :peers-max :initform 32)
   (element-type :initarg :element-type :initform '(unsigned-byte 8))
   (external-format :initarg :external-format :initform :utf-8)
   (peer-class :initarg :peer-class :initform 'stream-peer
	       :documentation "Class to instantiate as peer thread.")
;;   (request-timeout-length :initarg :request-timeout-length :initform 90)
   (%socket :initform nil) (%peers :initform nil) (%thread :initform nil)))

(defmethod start ((self socket-server))
  (when (not (%status self))
    (setf (s-v '%socket) (make-server :host (s-v 'host)
				      :port (s-v 'port)
				      :reuse-address (s-v 'reuse-address)
				      :backlog (s-v 'backlog)
				      :protocol (s-v 'protocol))
	  (s-v '%peers) (mapcar
			 #'(lambda (n)
			     (declare (ignore n))
			     (let ((p (if (listp (s-v 'peer-class))
					  (apply #'make-instance
						 (car (s-v 'peer-class)) (cdr (s-v 'peer-class)))
					  (make-instance (s-v 'peer-class)))))
			       (start p)
			       (setf (peer.server p) self)
			       p))
			 (seq (s-v 'peers-max)))
	  (s-v '%thread) (thread-spawn #'(lambda () (run self))
				       :name (format nil "Socket Server at ~A:~A"
						     (s-v 'host) (s-v 'port))))
    t))

(defmethod stop ((self socket-server))
  (if (s-v '%socket)
      (close-server (s-v '%socket)))
  (when (%status self)
    (thread-kill (s-v '%thread))
    (mapc #'stop (s-v '%peers))
    (setf (s-v '%socket) nil
	  (s-v '%thread) nil
	  (s-v '%peers) nil)
    t))

(defmethod %status ((self socket-server))
  (and (threadp (s-v '%thread)) (thread-alive-p (s-v '%thread))))

(defmethod status ((self socket-server))
  (%status self))

(defmethod run ((self socket-server))  
  (loop
     (progn
       (mapc #'(lambda (peer)
		 (multiple-value-bind (stream address) (accept (s-v '%socket))
		   (when stream
		     (if (not (status peer)) (start peer))
		     (handle-stream peer stream address))))
	     (s-v '%peers)))))
