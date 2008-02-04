(in-package :tr.gen.core.server)

;;;-----------------------------------------------------------------------------
;;; INDIVISIBLE WORKING UNITS
;;;-----------------------------------------------------------------------------
(defclass unit (server)
  ((name :accessor unit.name :initarg :name :initform "Core-serveR Unit")))

(defgeneric run (unit)
  (:documentation "Run method to make this unit work.")
  (:method ((self null)) nil)
  (:method ((self unit)) t))

(defgeneric unitp (unit)
  (:method ((self null)) nil)
  (:method ((self unit)) t))

(defvar +debug-units-on-error+ t "Debugging flag.")
;;;-----------------------------------------------------------------------------
;;; STANDARD UNIT
;;;-----------------------------------------------------------------------------
(defclass standard-unit (unit)
  ((%pid :reader get-pid :initarg :pid :initform -1)
   (%server :initarg :server :initform nil)
   (%continuations :initform (make-hash-table :test #'equal)
		   :documentation "Captured continuations.")))

(defmethod register-continuation ((self standard-unit) lambda &optional (k-id (current-thread)))
  (setf (gethash k-id (s-v '%continuations)) lambda))

(defmethod unregister-continuation ((self standard-unit) &optional (k-id (current-thread)))
  (remhash k-id (s-v '%continuations)))

(defmethod find-continuation ((self standard-unit) k-id)
  (gethash k-id (s-v '%continuations)))

(defmethod run ((self standard-unit))
  (flet ((control-loop-error (condition)
	   (if +debug-units-on-error+ 
	       (swank:swank-debugger-hook condition nil)
	       (invoke-restart 'ignore-error)))
	 (l00p ()
	   (let ((message (receive-message self)))
	     (cond
	       ((eq message 'shutdown)
;;		(format t "Shutting down unit:~A~%" self)
		(return-from run (values)))
	       ((functionp message)
		(funcall message self))
	       (t					  
		(format t "Got unknown message:~A~%" message))))))
  (loop
     (handler-bind ((error #'control-loop-error))
       (restart-case (l00p)
	 (ignore-error ()
	   :report "Ignore the error and continue processing."))))))

;;;-----------------------------------------------------------------------------
;;; LOCAL UNITS a.k.a. THREADS
;;;-----------------------------------------------------------------------------
(defclass local-unit (standard-unit)
  ((%thread :initform nil :documentation "Local thread that this unit runs."))
  (:default-initargs :name "Core-serveR Local Unit"))

(defmethod send-message ((self local-unit) message &optional (target nil))
  (thread-send (or target (s-v '%thread)) message))

(defmethod receive-message ((self local-unit))
  (thread-receive))

(defmethod start ((self local-unit))
  (if (not (local-unit.status self))      
      (prog1 t
	(setf (s-v '%thread)
	      (thread-spawn #'(lambda () (run self)) :name (unit.name self))))))

(defmethod stop ((self local-unit))
  (if (local-unit.status self) (send-message self 'shutdown))
  t)

(defmethod status ((self local-unit))
  (local-unit.status self))

(defmethod local-unit.status ((self local-unit))
  (and (threadp (s-v '%thread)) (thread-alive-p (s-v '%thread))))

(defmethod me-p ((self local-unit))
  (or (equal (current-thread) (s-v '%thread)) (not (status self))))

(defvar +ret+ nil)
(defmethod async-method-call-with-no-return ((self local-unit) method-name &rest args)
  (send-message self #'(lambda (unit)
			 (apply method-name unit args)))
  (values))

(defmethod async-method-call ((self local-unit) method-name &rest args)
  (let ((me (current-thread)))    
    (send-message self
		  #'(lambda (unit)		      
		      (block unit-execution 
			(let ((+ret+ #'(lambda (val)
					 (format t "funki +ret+~A~%" val)
					 (thread-send me (list val))
;; 					 (return-from unit-execution t)
					 (format t "before run~%")
					 (run unit)
					 )))
			  (let ((result (multiple-value-list
					 (apply method-name unit args))))
			    (thread-send me result)))))))
  #'(lambda ()
      (apply #'values (thread-receive))))

(defmacro defmethod/unit (name &rest args)
  (let* ((method-keyword (if (keywordp (car args))
			     (prog1 (car args) (setq args (cdr args)))))
	 (method-body (cdr args))
	 (method-args (car args))
	 (arg-names (extract-argument-names method-args :allow-specializers t))
	 (self (car arg-names)))
    `(progn
       (defmethod ,name ,method-args ,@method-body)
       ,(cond
	 ((eq method-keyword :async-no-return)
	  `(defmethod ,name :around ,method-args
	     (if (me-p ,self)
		 (call-next-method)
		 (async-method-call-with-no-return ,self ',name ,@(rest arg-names)))))
	 ((eq method-keyword :async)
	  `(defmethod ,name :around ,method-args
	     (if (me-p ,self)
		 (if (null +ret+)
		     (let ((+ret+ #'(lambda (val)
				      (return-from ,name val))))
		       (call-next-method))
		     (call-next-method))
		 (async-method-call ,self ',name ,@(rest arg-names)))))	 
	 ((or (null method-keyword) (eq method-keyword :sync))
	  `(defmethod ,name :around ,method-args
	     (if (me-p ,self)
		 (if (null +ret+)
		     (let ((+ret+ #'(lambda (val)
				      (return-from ,name val))))
		       (call-next-method))
		     (call-next-method))
		 (funcall		  
		  (async-method-call ,self ',name ,@(rest arg-names))))))
	 (t (error "Keyword you've entered is not a valid, try usual defmethod.")))
       (warn "defmethod/unit overrides :around method."))))


;; (defmethod async-method-call ((self local-unit) method-name &rest args)
;;   (let ((me (current-thread)))    
;;     (send-message self
;; 		  #'(lambda (unit)
;; 		      (let ((atomic nil))
;; 			(let* ((+unit-return+ #'(lambda  (val)
;; 						  (setf atomic t)
;; 						  (thread-send me (list val))))
;; 			       (result (multiple-value-list
;; 					(apply method-name unit args))))			  
;; 			  (unless atomic				
;; 			    (thread-send me result))))))
;;     #'(lambda ()
;; 	(prog1 (apply #'values (thread-receive))
;; 	  (cleanup-mailbox me)))))

;; (defmethod async-method-call-with-no-return ((self local-unit) method-name &rest args)
;;   (let ((me (current-thread)))
;;     (send-message self
;; 		  #'(lambda (unit)
;; 		      (apply method-name unit args))))
;;   (values))

;; (defmacro defmethod/unit (name &rest args)
;;   (let* (;;	 (method-name (intern (format nil "%~:@(~A~)" name)))
;; 	 (method-keyword (if (keywordp (car args))
;; 			     (prog1 (car args)
;; 			       (setq args (cdr args)))))
;; 	 (method-body (cdr args))
;; 	 (method-args (car args))
;; 	 (arg-names (extract-argument-names method-args :allow-specializers t))
;; 	 (self (car arg-names)))
;;     (cond
;;       ((eq method-keyword :async)
;;        `(defmethod ,name ,method-args
;; 	  (if (me-p ,self)
;; 	      (funcall #'(lambda ,arg-names
;; 			   (declare (ignorable ,(car arg-names)))
;; 			   (block ,name
;; 			     ,@method-body)) ,@arg-names)
;; 	      (async-method-call ,self ',name ,@(rest arg-names)))))
;;       ((eq method-keyword :async-no-return)
;;        `(defmethod ,name ,method-args
;; 	  (if (me-p ,self)
;; 	      (funcall #'(lambda ,arg-names
;; 			   (declare (ignorable ,(car arg-names)))
;; 			   (block ,name
;; 			     ,@method-body)) ,@arg-names)
;; 	      (async-method-call-with-no-return ,self ',name ,@(rest arg-names)))
;; 	  (values)))
;;       ((or (null method-keyword) (eq method-keyword :sync))
;;        `(defmethod ,name ,method-args
;; 	  (if (me-p ,self)
;; 	      (funcall #'(lambda ,arg-names
;; 			   (declare (ignorable ,(car arg-names)))
;; 			   (block ,name
;; 			     (let ((+unit-return+ #'(lambda (val)
;; 						     (return-from ,name val))))
;; 			       ,@method-body))) ,@arg-names)
;; 	      (funcall (apply #'async-method-call ,self ',name
;; 			      (list ,@(rest arg-names)))))))
;;       (t
;;        `(defmethod ,name ,method-keyword ,method-args ,@method-body)))))

;; (defclass u1 (local-unit)
;;   ())

;; (defvar *u1 (make-instance 'u1))

;; (defmethod/unit t1 :sync ((self u1))
;;   ;;		(format *standard-output* "gee:~A" self)
;; 		(error "gee")
;;   (values 88 99 100))

;; (defgeneric send-message (unit message)
;;   (:documentation "Send message to unit."))

;; (defgeneric receive-message (unit)
;;   (:documentation "Receive message from unit."))

;; ;;;-----------------------------------------------------------------------------
;; ;;; UNIT SERVER
;; ;;;-----------------------------------------------------------------------------
;; (defclass unit-server (server)
;;   ((host :initarg :host :initform "0.0.0.0")
;;    (port :initarg :port :initform 3999)
;;    (%units :initform (make-hash-table :test #'equal :weakness :value))))

;; (defmethod find-unit ((self unit-server) pid)
;;   (find pid (s-v '%units) :test #'equal :key #'get-pid))

;; ;;;-----------------------------------------------------------------------------
;; ;;; LOCAL UNIT SERVER
;; ;;;-----------------------------------------------------------------------------
;; (defclass local-unit-server (unit-server)
;;   ((%remote-unit-servers :initform '())
;;    %socket %thread))

;; (defmethod add-remote-server ((self local-unit-server) host port)
;;   (let ((server (make-instance 'remote-unit-server
;; 			       :host host
;; 			       :port port
;; 			       :local-unit-server self)))
;;     (start server)
;;     (setf (s-v '%remote-unit-servers)
;; 	  (cons server (s-v '%remote-unit-servers)))
;;     server))

;; (defmethod remove-remote-server ((self local-unit-server) server)
;;   (stop server)
;;   (setf (s-v '%remote-unit-servers) (remove server (s-v '%remote-unit-servers)))
;;   server)

;; (defmethod remote-servers ((self local-unit-server))
;;   (copy-list (s-v '%remote-unit-servers)))

;; (defmethod find-remote-server ((self local-unit-server) host port)
;;   (find-if #'(lambda (s) (if (and (equal (slot-value s 'host) host)
;; 				  (equal (slot-value s 'port) port))
;; 			     t))
;; 	   (s-v '%remote-unit-servers)))

;; (defmethod start ((self unit-server))
;;   (if (not (status self))
;;       (prog1 t
;; 	(setf (s-v '%socket) (make-server :host (s-v 'host)
;; 					  :port (s-v 'port)
;; 					  :reuse-address t
;; 					  :backlog 1
;; 					  :protocol :tcp)
;; 	      (s-v '%thread) (spawn #'(lambda () (run self))
;; 			       :name (format nil "Local Unit Server ~A:~A"
;; 					     (s-v 'host) (s-v 'port)))))
;;       nil))

;; (defmethod stop ((self unit-server))
;;   (if (status self)
;;       (let ((socket (s-v '%socket)))
;; 	(setf (s-v '%socket) nil)
;; 	(close-socket socket)
;; 	(kill-thread (s-v '%thread))
;; 	t)
;;       nil))

;; (defmethod status ((self unit-server))
;;   (and (s-v '%socket) (threadp (s-v '%thread)) (thread-alive-p (s-v '%thread))))

;; (defmethod run ((self unit-server))
;;   (loop (when (s-v '%socket))      
;;      (multiple-value-bind (stream host port) (accept (s-v '%socket))
;;        (let ((remote-server (make-instance 'remote-unit-server
;; 					   :host host
;; 					   :port port
;; 					   :stream stream
;; 					   :local-unit-server self)))
;; 	 (start remote-server)
;; 	 (setf (s-v '%remote-unit-servers)
;; 	       (cons remote-server (s-v '%remote-unit-servers)))
;; 	 (format t "Got remote unit server ~A:~A" host port)))))

;; ;;;-----------------------------------------------------------------------------
;; ;;; REMOTE UNIT SERVER
;; ;;;-----------------------------------------------------------------------------
;; (defclass remote-unit-server (unit-server)
;;   ((%local-unit-server :initarg :local-unit-server :initform nil)
;;    (%stream :initarg :stream :initform nil)
;;    %thread))

;; (defmethod start ((self remote-unit-server))
;;   (if (null (s-v '%stream))
;;       (setf (s-v '%stream) (connect (s-v 'host) (s-v 'port))))
  
;;   (setf (s-v '%thread) (spawn #'(lambda () (run self))
;; 			 :name (format nil "Remote unit server ~A:~A"
;; 				       (s-v 'host) (s-v 'port))))
;;   t)

;; (defmethod stop ((self remote-unit-server))
;;   (let ((stream (s-v '%stream))
;; 	(thread (s-v '%thread)))
;;     (setf (s-v '%stream) nil
;; 	  (s-v '%thread) nil)
;;     (close-stream stream)
;;     (kill-thread thread)))

;; (defmethod status ((self remote-unit-server))
;;   (and (s-v '%thread) (thread-alive-p (s-v '%thread)) (s-v '%stream)))

;; (defmethod run ((self remote-unit-server))
;;   (loop (when (s-v '%stream))
;;      (multiple-value-bind (type k-id args) (deserialize-unit-message (s-v '%stream))
;;        (cond
;; 	 ((eq 'unit-call type)
;; 	  (handle-unit-call self k-id args))
;; 	 ((eq 'unit-answer type)
;; 	  (handle-unit-answer self k-id args))
;; 	 (t
;; 	  (format t "Got unknown message:~A" type))))))

;; (defmethod handle-unit-call ((self remote-unit-server) k-id args)
;;   (let ((method-name (car args))
;; 	(unit (find-unit (s-v '%local-unit-server) (cadr args)))
;; 	(method-args (caddr args)))
;;     (send-message unit
;;      #'(lambda (unit)
;; 	 (when (slot-value self '%stream)
;; 	   (write-stream (slot-value self '%stream)
;; 	    (serialize-unit-message
;; 	     (list 'unit-answer k-id
;; 		   (multiple-value-list (apply method-name unit method-args))))))))))

;; (defmethod handle-unit-answer ((self remote-unit-server) k-id args)
;;   (let ((unit (find-unit self (car args)))
;; 	(retval (cadr args)))
;;     (send-message unit
;;      #'(lambda (unit)
;; 	 (let ((k (find-continuation unit k-id)))
;; 	   (funcall k retval))))))

;; ;;;-----------------------------------------------------------------------------
;; ;;; STANDARD UNIT
;; ;;;-----------------------------------------------------------------------------
;; (defclass standard-unit (unit)
;;   ((%pid :reader get-pid :initarg :pid :initform -1)
;;    (%server :initarg :server :initform nil)
;;    (%continuations :initform (make-hash-table :test #'equal)
;; 		   :documentation "Captured continuations.")))

;; (defmethod register-continuation ((self standard-unit) lambda &optional (k-id (current-thread)))
;;   (setf (gethash k-id (s-v '%continuations)) lambda))

;; (defmethod unregister-continuation ((self standard-unit) &optional (k-id (current-thread)))
;;   (remhash k-id (s-v '%continuations)))

;; (defmethod find-continuation ((self standard-unit) k-id)
;;   (gethash k-id (s-v '%continuations)))

;; (defmethod run ((self standard-unit))  
;;   (flet ((control-loop-error (condition)
;; 	   (if *debug-on-error*
;; 	       (swank:swank-debugger-hook condition nil)               
;; 	       (invoke-restart 'ignore-error)))
;; 	 (l00p ()
;; 	   (let ((message (receive-message self)))
;; 	     (cond
;; 	       ((eq message 'shutdown)
;; 		(format t "Shutting down unit:~A~%" self)
;; 		(return-from run (values)))
;; 	       ((functionp message)
;; 		(funcall message self))		   
;; 	       (t					  
;; 		(format t "Got unknown message:~A~%" message))))))
;;   (loop
;;      (handler-bind ((error #'control-loop-error))
;;        (restart-case	     
;; 	   (l00p)
;; 	 (ignore-error ()
;; 	   :report "Ignore the error and continue processing."))))))

;; ;;;-----------------------------------------------------------------------------
;; ;;; LOCAL UNITS a.k.a. THREADS
;; ;;;-----------------------------------------------------------------------------
;; (defclass local-unit (standard-unit)
;;   ((%thread :initform nil
;; 	    :documentation "Local thread that this unit runs.")))

;; (defmethod send-message ((self local-unit) message)
;;   (thread-send (s-v '%thread) message))

;; (defmethod receive-message ((self local-unit))
;;   (thread-receive))

;; (defmethod start ((self local-unit))
;;   (if (not (status self))      
;;       (prog1 t
;; 	(setf (s-v '%thread)
;; 	      (swank-backend:spawn (curry #'run self) :name "Local Unit")))
;;       nil))

;; (defmethod stop ((self local-unit))
;;   (if (status self) (send-message self 'shutdown))
;;   t)

;; (defmethod status ((self local-unit))
;;   (and (threadp (s-v '%thread)) (thread-alive-p (s-v '%thread))))

;; (defmethod sync-method-call ((self local-unit) method-name &rest args)
;;   (flet ((k (val)
;; 	   (return-from sync-method-call (apply 'values val)))
;; 	 (me (current-thread)))
;;     (send-message self
;;      #'(lambda (unit)
;; 	 (let ((result (apply method-name unit args)))
;; 	   (thread-send me
;; 			#'(lambda (unit)
;; 			    (funcall k result)))))))
;;   (run self))

;; (defclass remote-unit (standard-unit)
;;   ())

;; (defmethod start ((self remote-unit)) t)
;; (defmethod stop ((self remote-unit)) t)
;; (defmethod status ((self remote-unit)) t)

;; (defmethod sync-method-call ((self remote-unit) method-name &rest args)
;;   (flet ((k (val)
;; 	   (return-from sync-method-call (apply 'values val))))
;;     (let ((k-id (random-string)))
;;       (register-continuation self #'k k-id)
;;       (when (slot-value (slot-value self '%server) '%stream)
;; 	(write-stream (slot-value (slot-value self '%server) '%stream)
;;          (serialize-unit-message
;; 	  (list 'unit-call k-id (list method-name (s-v '%pid) args)))))))
;;   (run self))

;; (defmacro defmethod/unit (name &rest args)
;;   (let* ((method-name (intern (format nil "%~:@(~A~)" name)))
;; 	 (method-keyword (if (keywordp (car args))
;; 			     (prog1 (car args)
;; 			       (setf args (cdr args)))))
;; 	 (method-body (cdr args))
;; 	 (method-args (car args))
;; 	 (arg-names (extract-argument-names args :allow-specializers t))
;; 	 (self (car arg-names)))
;;     (cond
;;       ((eq method-keyword :async)
;;        `(progn
;; 	  (defmethod ,method-name ,method-args
;; 	    ,@method-body)
;; 	  (defmethod ,name ,method-args
;; 	    (if (me-p self)
;; 		(,method-name ,arg-names)
;; 		(async-method-call ,self ',method-name ,@(rest arg-names))))))
;;       ((or (null method-keyword) (eq method-keyword :sync))
;;        `(progn
;; 	  (defmethod ,method-name ,method-args
;; 	    ,@method-body)
;; 	  (defmethod ,name ,method-args
;; 	    (if (me-p self)
;; 		(,method-name ,arg-names)
;; 		(sync-method-call ,self ',method-name ,@(rest arg-names))))))
;;       (t
;;        `(defmethod ,method-name ,method-keyword ,method-args
;; 		   ,@method-body)))))
;; ;;;-----------------------------------------------------------------------------
;; ;;; UNIT CACHE
;; ;;;-----------------------------------------------------------------------------
;; (defvar *unit-cache* (make-hash-table :test #'equal))
;; (defun current-unit (&optional (cache *unit-cache*))
;;   (gethash (current-thread) cache))

;; (defmethod run :around ((self standard-unit))
;;   (if (equal (s-v '%thread) (current-thread))
;;       (setf (gethash (current-thread) *unit-cache*) self))
;;   (prog1 (call-next-method)
;;     (remhash (current-thread) *unit-cache*)))



;; (defmethod/unit t1 :around ((self local-unit))
;;   (list "abuzer" "mabuzer"))
