(in-package :tr.gen.core.server)

;;;-----------------------------------------------------------------------------
;;; BSD SOCKETS, mostly from trivial-sockets.
;;;-----------------------------------------------------------------------------
(defun resolve-hostname (name)
  (cond
   ((typep name '(vector * 4)) name)
   (t (car (host-ent-addresses (get-host-by-name name))))))

(defun make-server (&key (host (vector 0 0 0 0)) (port 0) (reuse-address t) (backlog 1)
			 (protocol :tcp))
  "Returns a SERVER object and the port that was bound, as multiple values"
  (let ((socket (make-instance 'inet-socket :type :stream :protocol protocol)))
    (when reuse-address
      (setf (sockopt-reuse-address socket) t))
    (socket-bind socket (resolve-hostname host) port)
    (socket-listen socket backlog)
    (apply #'values socket
	    (multiple-value-list (socket-name socket)))))

(defun close-server (server)
  (socket-close server))

(defun accept (socket &key (element-type '(unsigned-byte 8)))
  (multiple-value-bind (s peer) (socket-accept socket)
    (values (make-core-stream (socket-make-stream s
						  :input t :output t
						  :element-type element-type
						  :buffering :full))
            peer)))

(defun connect (server-host server-port
		&key (element-type '(unsigned-byte 8)) (protocol :tcp))  
  (let ((socket (make-instance 'inet-socket :type :stream :protocol protocol)))
    (socket-connect socket (resolve-hostname server-host) server-port)
    (make-core-stream (socket-make-stream socket :input t :output t
					  :element-type element-type						 
					  :buffering :full))))
