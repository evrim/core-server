(in-package :tr.gen.core.ffi)

(defun np () (null-pointer))
(defun bzero (s n) (%bzero s n))

(defun print-range (ptr len &optional (type :uint8))
  (loop for i from 0 below len
     do (format t "~2,'0D " (or (and (= 0 (mem-ref ptr type i)) #\.)
				(mem-ref ptr type i)))
     do (if (= 0 (mod len 80)) (format t "~%"))))

(defvar epolloneshot #.(ash 1 30))
(defvar epollet #.(ash 1 31))

;; addrinfo helpers
(define-c-struct-wrapper addrinfo ())
(define-c-struct-wrapper sockaddr ())
(define-c-struct-wrapper sockaddr-in ())
(define-c-struct-wrapper linger ())

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
	    (error (format nil "getaddrinfo: ~A" (%gai_strerror r)))
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
			    (with-foreign-objects ((l 'linger) (o :int))
			      (setf (mem-ref o :int) 1)
			      (bzero l size-of-linger)
			      (%setsockopt sfd sol-socket so-reuseaddr o
					   (foreign-type-size :int))
			      (%setsockopt sfd sol-socket so-keepalive o
					   (foreign-type-size :int))
			      (%setsockopt sfd sol-socket so-linger l
					   (foreign-type-size 'linger))
			      (%setsockopt sfd ipproto-tcp tcp-nodelay o
					   (foreign-type-size :int)))
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
  (with-foreign-object (peer 'sockaddr-in)
    (bzero peer size-of-sockaddr-in)
    (with-foreign-object (len :int)
      (setf (mem-ref len :int) size-of-sockaddr)
      (let* ((newfd (%accept fd peer len))
	     (peeraddr (with-foreign-object (port :string 5)
			 (with-foreign-object (host :string 255) 
			   (bzero host 255) (bzero port 5) 
			   (%getnameinfo peer size-of-sockaddr-in host 255 port 5 (logior ni-numerichost ni-numericserv))
			   (list (foreign-string-to-lisp host) (foreign-string-to-lisp port)))))) 
	(values newfd peeraddr)))))

(defun set-nonblock (fd)
  (%fcntl fd setfd (logior nonblock (%fcntl fd getfd 0))))

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
(defclass epoll-device (core-server::local-unit)
  ((fd :accessor fd :initarg :fd :initform nil)
   (listenfd :accessor listenfd :initarg :listenfd)
   (handler :accessor handler :initarg :handler)
   (max-events :accessor max-events :initarg :max-events :initform +epoll-max-events+)
   (state :accessor state :initarg :state :initform (make-hash-table))))

(defun make-epoll-device (listen handler &optional (size +epoll-size+))
  (make-instance 'epoll-device :fd (epoll-create size) :listenfd listen :handler handler))

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
		 (t ;; (let ((ee (make-instance 'epoll-event :pointer (mem-ref events 'epoll-event (- i 1)))))
;; 		      (collect (1- i) (cons ee acc)))
		  (collect (1- i) (cons (mem-ref events 'epoll-event (- i 1)) acc))
		    ))))
      (let ((n (epoll-wait (fd self) events (max-events self) timeout)))
	(cond
	  ((eq n 0) nil)
	  (t (collect n)))))))

(defmethod core-server::run ((self epoll-device))
  (format t "Running on listenfd: ~D~%" (slot-value self 'listenfd))
  (loop
     (with-slots (listenfd handler-callback) self
       
       (labels ((ev-fd (e)
                  (foreign-slot-value (foreign-slot-value e 'epoll-event 'data) 'epoll-data 'fd)) 
                (newconn? (e)
                  (eq listenfd (ev-fd e))))
         (add self listenfd (list epollin #.(ash 1 31)))
         (let ((events (wait self 1000)))
           (with-foreign-object (ev 'epoll-event)
             (bzero ev size-of-epoll-event)
             (when (> (length events) 0) (format t "Wait returns: ~D~%" (length events)))
             (mapcar #'(lambda (e)
                         (format t "Event on fd: ~D~%" (ev-fd e))
                         (if (newconn? e)
                             (let ((client (accept (ev-fd e))))
                               (format t "accepted connection fd: ~D~%" client)
                               (if (< client 0)
                                   (format t "error: accept")
                                   (progn
                                     (set-nonblock (ev-fd e)) 
                                     (add self client (list epollin #.(ash 1 31))))))
                             (funcall #'handler-callback self (ev-fd e))))
                     events)))))))

(defmethod core-server::stop ((self epoll-device))
  (%close (slot-value self 'listenfd))
  (%close (slot-value self 'fd))
  (core-server::thread-kill (slot-value self 'core-server::%thread)))

(defparameter *read-bufsize* 1024)
(defparameter *response* "HTTP/1.1 200 OK
Content-Type: text/html;charset=UTF-8

<html>
<head><title>Core-Server Evloop Backend</title></head>
<body><p>Default worker answering your request.</p></body>
<p>aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa</p>
</html>
")

(defparameter *static-response* (foreign-alloc :string :count (length *response*)))
(lisp-string-to-foreign *response* *static-response* (length *response*))

;; read from fd, write to it, when EAGAIN occurs, save the state
(defmethod handler-callback ((self epoll-device) fd)
  (format t "handler-callback with fd: ~D~%" fd)
  (with-foreign-object (buf :string *read-bufsize*)
    (bzero buf *read-bufsize*)
    (format t "read: ~D~%" (%read fd buf *read-bufsize*))
    (format t "write: ~D~%" (%write fd *static-response* (length *response*)))
    (%close fd)
    (del self fd)))

(defun test (port) 
  (let* ((listen (bind "localhost" port))
         (dev (make-epoll-device listen #'handler-callback)))
    (core-server::start dev)
    dev))