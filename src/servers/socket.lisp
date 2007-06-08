(in-package :tr.gen.core.server)

(defparameter *handler-counter* 0)

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
   %socket %peers (%thread :initform nil)))

(defmethod start ((self socket-server))
  (when (not (status self))
    (setf (s-v '%socket) (make-server :host (s-v 'host)
				      :port (s-v 'port)
				      :reuse-address (s-v 'reuse-address)
				      :backlog (s-v 'backlog)
				      :protocol (s-v 'protocol))
	  (s-v '%peers) (mapcar #'(lambda (n)
				    (declare (ignore n))
				    (let ((p (make-instance (s-v 'peer-class))))
				      (start p)
				      p))
				(seq (s-v 'peers-max)))
	  (s-v '%thread) (thread-spawn #'(lambda () (run self))
				       :name (format nil "Socket Server at ~A:~A"
						     (s-v 'host) (s-v 'port))))
    t))

(defmethod stop ((self socket-server))
  (when (status self)
    (thread-kill (s-v '%thread))
    (close-server (s-v '%socket))
    (mapc #'(lambda (peer)
	      (if (status peer)
		  (stop peer)))
	  (s-v '%peers))
    (setf (s-v '%socket) nil
	  (s-v '%thread) nil
	  (s-v '%peers) nil)
    t))

(defmethod status ((self socket-server))
  (and (threadp (s-v '%thread)) (thread-alive-p (s-v '%thread))))

(defmethod run ((self socket-server))
  (loop
     (progn
       (setf *handler-counter* (+ *handler-counter* (s-v 'peers-max)))
       (mapc #'(lambda (peer)
		 (multiple-value-bind (stream address) (accept (s-v '%socket))
		   (when stream
		     (handle-stream peer stream address))))
	     (s-v '%peers)))))
