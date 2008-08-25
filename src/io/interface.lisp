(in-package :tr.gen.core.ffi)



(defun np () (null-pointer))
(defun bzero (s n) (%bzero s n))

(defun print-range (ptr len &optional (type :uint8))
  (loop for i from 0 below len
     do (format t "~2,'0D " (or (and (= 0 (mem-ref ptr type i)) #\.)
				(mem-ref ptr type i)))
     do (if (= 0 (mod len 80)) (format t "~%"))))

;; addrinfo helpers
(define-c-struct-wrapper addrinfo ())
(define-c-struct-wrapper sockaddr ())

;; (defun make-addrinfo (ptr &optional (family 0) (socktype 0) (flags af-unspec)
;; 		      (protocol 'sock-stream) (canonname (np)) (addr (np)) (next (np)))
;;   (bzero ptr size-of-addrinfo)
;;   (with-foreign-slots ((ai-flags ai-family ai-socktype ai-protocol ai-canonname ai-addr ai-next) ptr addrinfo)
;;     (setf ai-family family
;; 	  ai-socktype socktype
;; 	  ai-flags flags
;; 	  ai-protocol protocol
;; 	  ai-canonname canonname
;; 	  ai-addr addr
;; 	  ai-next next))
;;   (values ptr))

(defmacro with-addrinfo ((var family socktype flags protocol) &body body)
  `(with-foreign-object (,var 'addrinfo)
     (with-foreign-slots ((ai-family ai-flags ai-socktype ai-protocol) ,var addrinfo)
       (setf ai-family ,family ai-flags ,flags ai-socktype ,socktype ai-protocol ,protocol)
       ,@body)))

(defun bind (node service
	     &optional (protocol :tcp) (backlog 10) (reuse-address t))
  ;; ptr for results
  (with-foreign-object (res :pointer)
    ;; construct an addrinfo for hints
    (with-addrinfo (hints af-inet (ecase protocol
				    (:tcp sock-stream)
				    (:udp sock-dgram))
			  flag-ai-passive 0)
      ;; call getaddrinfo
      (let ((r (%getaddrinfo node (if (numberp service)
				      (format nil"~D" service)
				      service)
			     hints
			     res)))
	(if (not (eq 0 r))
	    (error (format nil "Error: ~A" (%gai_strerror r)))
	    ;; otherwise we'll have addrinfos in res. let's recurse
	    ;; over them to bind any
	    (labels ((bind-any (ptr)
		       (cond
			 ((null-pointer-p ptr) nil)
			 (t
			  (let* ((obj (make-instance 'addrinfo :pointer (mem-ref ptr 'addrinfo)))
				 (sfd (%socket (addrinfo-ai-family obj)
					       (addrinfo-ai-socktype obj)
					       (addrinfo-ai-protocol obj))))
			    (when reuse-address
			      (with-foreign-object (o :int)
				(setf (mem-ref o :int) 1)
				(%setsockopt sfd sol-socket so-reuseaddr o
					     (foreign-type-size :int))))
			    (with-slots (ai-addr ai-addrlen ai-next) obj
			      (if (not (eq sfd -1)) 
				  (if (eq 0 (%bind sfd ai-addr ai-addrlen))
				      (progn
					(%listen sfd backlog)
					sfd)
				      (progn
					(format t "Bind error: ~D" (errno))
					(%close sfd)))
				  (bind-any ai-next))))))))
	      (bind-any (mem-ref res :pointer))))))))

(defmethod accept (fd)
  (with-foreign-object (peer 'sockaddr)
    (bzero peer size-of-sockaddr)
    (with-foreign-object (len :int)
      (setf (mem-ref len :int) size-of-sockaddr)
      (%accept fd peer len))))

;; EPOLLUTION
(define-c-struct-wrapper epoll-event ())
(defvar +epoll-size+ 131072)
(defvar +epoll-max-events+ 100)

(defun epoll-create (size)
  (%epoll-create size))

(defun epoll-ctl (epfd op fd event)
  (%epoll-ctl epfd op fd event))

(defun epoll-wait (epfd events maxevents timeout)
  (%epoll-wait epfd events maxevents timeout))

;; actual interface
(defclass epoll-device ()
  ((fd :accessor fd :initarg :fd :initform nil)
   (max-events :accessor max-events :initarg :max-events :initform +epoll-max-events+)))

(defun make-epoll-device (&optional (size +epoll-size+))
  (make-instance 'epoll-device :fd (epoll-create size)))

(defmethod add ((self epoll-device) fd (modes list))
  (with-foreign-object (ev 'epoll-event)
    (bzero ev size-of-epoll-event)
    (with-foreign-slots ((events data) ev epoll-event) 
      (setf events (apply #'logior modes)
	    (foreign-slot-value (foreign-slot-value ev 'epoll-event 'data)
				'epoll-data 'fd) fd))
    (epoll-ctl (fd self) epoll-ctl-add fd ev)))

(defmethod del ((self epoll-device) fd)
  ;; Since Linux 2.6.9, event can be specified as NULL when using
  ;; EPOLL_CTL_DEL.
  (epoll-ctl (fd self) epoll-ctl-del fd (np))) 

(defmethod wait ((self epoll-device) timeout)
  (with-foreign-object (events 'epoll-event (max-events self))
    (bzero events (* (max-events self) size-of-epoll-event))
    (labels ((collect (i &optional acc)
	       (cond
		 ((< i 0) acc)
		 (t (let ((ee (make-instance 'epoll-event :pointer (mem-ref events 'epoll-event (- i 1)))))
		      (collect (1- i) (cons ee acc)))))))
      (let ((n (epoll-wait (fd self) events (max-events self) timeout)))
	(cond
	  ((eq n 0) nil)
	  (t (collect n)))))))
