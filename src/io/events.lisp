(in-package :core-server)

;;; example usage?
;; (let ((accepter (with-listener-socket (listener "127.0.0.1" 9000)
;; 		      (make-instance 'accept-unit :socket listener))))
;;   (add-worker accepter 4)
;;   (start accepter))

(defclass core-socket-stream (core-fd-io-stream)
  ((socket :accessor socket)))

(defmethod stream.fd ((self core-socket-stream))
  (swank-backend::socket-fd (socket self)))

(defmethod %peek-stream ((self core-socket-stream))
  (socket.recv (socket self) '() 1))

(defmethod read-stream ((self core-socket-stream))
  )

(defmethod write-stream ((self core-socket-stream) octet)
  (socket.send (socket self) #(octet)))

(defmethod close-stream ((self core-socket-stream))
  (socket.close (socket self)))


;;;
;;; non-blocking io stream
;;;
(defclass nio-socket-stream (core-socket-stream)
  ((worker-unit :accessor nio-socket-stream.worker-unit)))

(defmethod stream.worker ((self nio-socket-stream))
  (nio-socket-stream.worker-unit self))

(defmethod stream.dispatcher ((stream nio-socket-stream))
  (event-dispatcher (stream.worker stream)))

(defmethod stream.queue ((stream nio-socket-stream))
  (event-queue (stream.dispatcher stream)))

(defmethod stream.rqueue ((stream nio-socket-stream))
  (event-dispatcher.rfds (stream.dispatcher stream)))

(defmethod stream.wqueue ((stream nio-socket-stream))
  (event-dispatcher.wfds (stream.dispatcher stream)))

(defmethod read-stream ((stream nio-socket-stream))
  (flet ((retk (val)
	   (return-from read-stream val)))
    (handler-bind
	((kernel-error (lambda (c)
			 (if (eagain? c)
			     (progn
			       (setf (gethash (stream.fd stream) (stream.rqueue stream))
				     #'(lambda ()
					 (send-message (stream.worker stream)
						       #'(lambda ()
							   (funcall (function retk) (read-stream stream))))))
			       (add-fd (stream.queue stream) (stream.fd stream) `(epollin epollet))
			       (run (event-dispatcher stream)))))))
      (call-next-method))))

(defmethod write-stream ((stream nio-socket-stream) octet)
  (flet ((retk (val)
	   (return-from write-stream val)))
    (handler-bind
	((kernel-error (lambda (c)
			 (if (eagain? c)
			     (progn
			       (setf (gethash (stream.fd stream) (stream.wqueue stream))
				     #'(lambda ()
					 (send-message (worker-unit stream)
						       #'(lambda ()
							   (funcall (function retk) (read-stream stream))))))
			       (add-fd (stream.queue stream) (stream.fd stream) '(epollout epollet))
			       (run (event-dispatcher stream)))))))
      (call-next-method))))


(defclass event-dispatcher (unit)
  ((epoll-device :accessor event-dispatcher.epoll-device
		:initarg :epoll-device
		:initform (make-epoll-device))
   (rfds :accessor event-dispatcher.rfds :initarg :rfds)
   (wfds :accessor event-dispatcher.wfds :initarg :wfds)
   (efds :accessor event-dispatcher.efds :initarg :efds)))

(defmethod event-queue ((self event-dispatcher))
  (event-dispatcher.epoll-device self))

(defmethod recipient? ((self event-dispatcher) event)
  (or (apply #'gethash (epoll-event.fd event)
	     (cond 
	       ((eq epollin (logand epollin (epoll-event.events event)))
		(event-dispatcher.rfds self))
	       ((eq epollout (logand epollout (epoll-event.events event)))
		(event-dispatcher.wfds self))
	       ((eq epollerr (logand epollerr (epoll-event.events event)))
		(event-dispatcher.efds self))))
      #'(lambda ()
	  (format t "missed event on fd: ~D" (epoll-event.fd event)))))

(defmethod/unit run ((self event-dispatcher))
  (loop (mapcar #'(lambda (ev)
		    (funcall (recipient? self ev)))
		(wait (epoll-device self) 100))))

;;;
;;; Worker Unit (worker for each new client)
;;;
(defclass worker-unit (unit)
  ((accept-unit :accessor worker-unit.accept-unit :initarg :accept-unit)
   (event-dispatcher :accessor worker-unit.event-dispatcher :initarg :event-dispatcher)))

(defmethod event-dispatcher ((self worker-unit))
  (worker-unit.event-dispatcher self))

(defmethod/unit handle-fd ((self worker-unit) socket)
  ;; create nio-socket for each client.
  (let ((s (make-instance 'nio-socket-stream
			  :worker-unit self
			  :socket socket)))
    ;; request response cycle here...
    (print-response (eval-request (read-request s)))))

;;;
;;; Fixed entrypoint
;;;
(defclass accept-unit (unit)
  ((event-dispatcher :accessor accept-unit.event-dispatcher
		     :initarg :event-dispatcher
		     :initform (make-instance 'event-dispatcher))
   (socket :accessor accept-unit.socket :initarg :socket :initform nil)
   (workers :accessor accept-unit.workers :initarg :workers :initform nil)))

(defmethod add-worker ((self accept-unit) num)
  (let ((workers (loop
		    for i from 1 to num
		    collect (make-instance 'worker-unit
					   :accept-unit self
					   :event-dispatcher (accept-unit.event-dispatcher self)))))
    (mapcar #'(lambda (w)
		(start w)
		(push w (accept-unit.workers self)))
	    workers)))

(defmethod/unit run ((self accept-unit))
  (loop
     (mapcar #'(lambda (w)
		 (handle-fd w (accept (socket self))))
	     (accept-unit.workers self))))

