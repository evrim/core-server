;; Core Server: Web Application Server

;; Copyright (C) 2006-2008  Metin Evrim Ulu, Aycan iRiCAN

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :tr.gen.core.server)

;;+----------------------------------------------------------------------------
;;| Socket Server
;;+----------------------------------------------------------------------------
(defclass+ socket-server (server)
  ((host :initarg :host :initform "0.0.0.0"
	 :documentation "IP address that this server binds to")
   (port :initarg :port :initform 3009
	 :documentation "Port that this server binds to")
   (protocol :initarg :protocol :initform :tcp
	     :documentation "Network Protocol, can be :udp, :tcp")
   (reuse-address :initarg :reuse-address :initform t
		  :documentation "TCP reuse address option")
   (backlog :initarg :backlog :initform 1
	    :documentation "TCP backlog option")
   (peers-max :initarg :peers-max :initform 4
	      :documentation "Number of peers that this server manages")
   (element-type :initarg :element-type :initform '(unsigned-byte 8)
		 :documentation "Data type for socket stream")
   (external-format :initarg :external-format :initform :utf-8
		    :documentation "External format for stream")
   (peer-class :initarg :peer-class :initform 'stream-peer
	       :documentation "Class to instantiate as peer thread.")
   ;;   (request-timeout-length :initarg :request-timeout-length :initform 90)
   (%socket :initform nil) (%peers :initform nil) (%socket-thread :initform nil)
   (%debug-unit :initform nil))
  (:documentation "Socket Server Class"))

(defmethod socket-server.run ((self socket-server))
  "Run method of Socket Server"
  (loop
     (progn
       (mapc #'(lambda (peer)
		 (multiple-value-bind (stream address) (accept (s-v '%socket))
		   (when stream
		     (if (not (status peer)) (start peer))
		     (handle-stream peer stream address))))
	     (s-v '%peers)))))

;;-----------------------------------------------------------------------------
;; Server Procotol Implementation
;;-----------------------------------------------------------------------------
(defmethod make-bind-socket ((self socket-server))
  (make-server :host (s-v 'host)
	       :port (s-v 'port)
	       :reuse-address (s-v 'reuse-address)
	       :backlog (s-v 'backlog)
	       :protocol (s-v 'protocol)))

(defmethod start ((self socket-server))
  (when (not (socket-server.status self))
    (when (null (s-v 'debug))
      (setf (s-v '%debug-unit) (make-instance 'local-unit :name "Http Server Debugging Unit"))
      (start (s-v '%debug-unit)))
    (setf (s-v '%socket) (make-bind-socket self)
	  (s-v '%peers) (mapcar
			 #'(lambda (n)
			     (declare (ignore n))
			     (let ((p (if (listp (s-v 'peer-class))
					  (apply #'make-instance
						 (car (s-v 'peer-class))
						 (cons :debug-unit
						       (cons (if (null (s-v 'debug))
								 (s-v '%debug-unit)
								 nil)
							     (cdr (s-v 'peer-class)))))
					  (make-instance (s-v 'peer-class)
							 :debug-unit (if (null (s-v 'debug))
									 (s-v '%debug-unit)
									 nil)))))
			       (start p)
			       (setf (peer.server p) self)
			       p))
			 (seq (s-v 'peers-max)))
	  (s-v '%socket-thread) (thread-spawn #'(lambda () (socket-server.run self))
					      :name (format nil "Socket Server at ~A:~A"
							    (s-v 'host) (s-v 'port))))))

(defmethod stop ((self socket-server))
  (when (socket-server.status self)
    (and (s-v '%socket-thread) (thread-kill (s-v '%socket-thread)))
    (and (s-v '%socket) (close-server (s-v '%socket)))
    (mapc #'stop (s-v '%peers))
    (stop (s-v '%debug-unit))
    (setf (s-v '%socket) nil
	  (s-v '%socket-thread) nil
	  (s-v '%peers) nil)))

(defmethod socket-server.status ((self socket-server))
  (and (threadp (s-v '%socket-thread)) (thread-alive-p (s-v '%socket-thread))))

(defmethod status ((self socket-server))
  (socket-server.status self))

;; +----------------------------------------------------------------------------
;; | Non Blocking Socket IO Server
;; +----------------------------------------------------------------------------
;; (defclass nio-socket-server (local-unit)
;;   ((host :initarg :host :initform "0.0.0.0"
;; 	 :documentation "IP address that this server binds to")
;;    (port :initarg :port :initform 3009
;; 	 :documentation "Port that this server binds to")
;;    (protocol :initarg :protocol :initform :tcp
;; 	     :documentation "Network Protocol, can be :udp, :tcp")
;;    (reuse-address :initarg :reuse-address :initform t
;; 		  :documentation "TCP reuse address option")
;;    (backlog :initarg :backlog :initform 1
;; 	    :documentation "TCP backlog option")
;;    (peers-max :initarg :peers-max :initform 6
;; 	      :documentation "Number of peers that this server manages")
;;    (element-type :initarg :element-type :initform '(unsigned-byte 8)
;; 		 :documentation "Data type for socket stream")
;;    (external-format :initarg :external-format :initform :utf-8
;; 		    :documentation "External format for stream")
;;    (peer-class :initarg :peer-class :initform 'stream-peer
;; 	       :documentation "Class to instantiate as peer thread.")
;;    ;;   (request-timeout-length :initarg :request-timeout-length :initform 90)
;;    (%socket :initform nil) (%peers :initform nil)
;;    (%debug-unit :initform nil))
;;   (:documentation "Non blocking socket server"))

;; (defmethod start ((self nio-socket-server))  
;;   (when (null (s-v 'debug))
;;     (setf (s-v '%debug-unit) (make-instance 'local-unit :name "Http Server Debugging Unit"))
;;     (start (s-v '%debug-unit)))

  
;;   (setf (s-v '%socket) (core-ffi::bind (s-v '%host) (s-v '%port))
;; 	(s-v '%peers) (mapcar
;; 		       #'(lambda (n)
;; 			   (declare (ignore n))
;; 			   (let ((p (if (listp (s-v 'peer-class))
;; 					(apply #'make-instance
;; 					       (car (s-v 'peer-class))
;; 					       (cons :debug-unit
;; 						     (cons (if (null (s-v 'debug))
;; 							       (s-v '%debug-unit)
;; 							       nil)
;; 							   (cdr (s-v 'peer-class)))))
;; 					(make-instance (s-v 'peer-class)
;; 						       :debug-unit (if (null (s-v 'debug))
;; 								       (s-v '%debug-unit)
;; 								       nil)))))
;; 			     (start p)
;; 			     (setf (peer.server p) self)
;; 			     p))
;; 		       (seq (s-v 'peers-max)))
;; 	(s-v '%socket-thread) (thread-spawn #'(lambda () (socket-server.run self))
;; 					    :name (format nil "Socket Server at ~A:~A"
;; 							  (s-v 'host) (s-v 'port)))))


;; (defmethod stop ((self socket-server))
;;   (when (socket-server.status self)
;;     (thread-kill (s-v '%socket-thread))
;;     (and (s-v '%socket) (close-server (s-v '%socket)))
;;     (mapc #'stop (s-v '%peers))
;;     (stop (s-v '%debug-unit))
;;     (setf (s-v '%socket) nil
;; 	  (s-v '%socket-thread) nil
;; 	  (s-v '%peers) nil)))


;; (defmethod run ((self nio-socket-server))
;;   )

;; (defmethod make-bind-socket ((self nio-socket-server))
;;   (nio-make-server :host (s-v 'host)
;; 		   :port (s-v 'port)
;; 		   :reuse-address (s-v 'reuse-address)
;; 		   :backlog (s-v 'backlog)
;; 		   :protocol (s-v 'protocol)))

;; (defmethod socket-server.run ((self nio-socket-server))
;;   "Run method of Socket Server"
;;   (loop
;;      (progn
;;        (mapc #'(lambda (peer)
;; 		 (multiple-value-bind (stream address) (nio-accept (s-v '%socket))
;; 		   (when stream
;; 		     (if (not (status peer)) (start peer))
;; 		     (handle-stream peer stream address))))
;; 	     (s-v '%peers)))))
