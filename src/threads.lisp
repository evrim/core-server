(in-package :tr.gen.core.server)

;;;-----------------------------------------------------------------------------
;;; POSIX THREADS COMPATIBILITY LAYER, got from slime/swank mostly.
;;;-----------------------------------------------------------------------------
#+sb-thread
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric threadp (thread)
    (:method ((self t)) nil)
    (:method ((self sb-thread:thread)) t))
  
  (defun current-thread ()
    sb-thread:*current-thread*)

  (defun all-threads ()
    (sb-thread:list-all-threads))

  (defun thread-alive-p (thread)
    (sb-thread:thread-alive-p thread))

  (defun make-lock (&key (name nil))
    (sb-thread:make-mutex :name name))

  (defmacro with-lock-held ((lock) &body body)
    `(sb-thread:with-mutex (,lock)
       ,@body))

  (defmacro with-recursive-lock-held ((lock) &body body)
    `(sb-thread:with-recursive-lock (,lock)
       ,@body))
  
  (defvar *thread-mailbox-lock* (make-lock :name "Thread Mailbox Lock"))
  (declaim (type hash-table *thread-mailboxes*))
  (defvar *thread-mailboxes* (make-hash-table :test #'eq :weakness :value))

  (defstruct (thread-mailbox (:conc-name mailbox.))
    thread
    (lock (make-lock))
    (waitqueue (sb-thread:make-waitqueue))
    (queue '() :type list))

  (defun find-thread-mailbox (thread)
    (gethash (sb-thread::thread-os-thread thread) *thread-mailboxes*))

  (defun thread-mailbox (thread)
    "Return THREAD's mailbox."
    (with-lock-held (*thread-mailbox-lock*)
      (or (find-thread-mailbox thread)	  
	  (setf
	   (gethash (sb-thread::thread-os-thread thread) *thread-mailboxes*)
	   (make-thread-mailbox :thread thread)))))

  (defun thread-send (thread message)
    "Send message to thread"
    (let* ((mbox (thread-mailbox thread))
	   (lock (mailbox.lock mbox)))
      (with-lock-held (lock)
	(setf (mailbox.queue mbox)
	      (nconc (mailbox.queue mbox) (list message)))
	(sb-thread:condition-broadcast (mailbox.waitqueue mbox)))))

  (defun thread-receive (&optional (thread (current-thread)))
    "Pop message from queue"
    (let* ((mbox (thread-mailbox thread))
	   (lock (mailbox.lock mbox)))
      (with-lock-held (lock)
	(loop
	   (let ((q (mailbox.queue mbox)))
	     (cond (q (return (pop (mailbox.queue mbox))))
		   (t (sb-thread:condition-wait (mailbox.waitqueue mbox)
						lock))))))))
  
  (defun cleanup-mailbox (&optional (thread (current-thread)))
    (with-lock-held (*thread-mailbox-lock*)
      (remhash (sb-thread::thread-os-thread thread) *thread-mailboxes*)))
  
  (defun thread-spawn (fn &key name)
    (sb-thread:make-thread #'(lambda () (funcall (the function fn)) (cleanup-mailbox))
			   :name name))
  
  (defun thread-interrupt (thread fn)
    (sb-thread:interrupt-thread thread fn))

  (defun thread-kill (thread)    
    (sb-thread:terminate-thread thread)
    (cleanup-mailbox thread)))

;; (defmethod threadp ((object sb-thread:thread))
;;   t)

;; (defmethod threadp ((self t))
;;   nil)

;; (defclass thread (server)
;;   ())

;; (defgeneric run (thread)
;;   (:documentation "Thread main loop."))

;; (defclass standard-thread (thread)
;;   ((local-thread :initform nil)
;;    (continuations :initform (make-hash-table :test #'equal)
;; 		  :documentation "Thread continuations.")))

;; (defmethod start ((self standard-thread))
;;   (if (not (status self))      
;;       (progn (setf (s-v 'local-thread)
;; 		   (swank-backend:spawn (curry #'run self)
;; 		     :name "CLOS Thread"))
;; 	     t)
;;       nil))

;; (defmethod stop ((self standard-thread))
;;   (if (status self)
;;       (swank-backend:send (s-v 'local-thread) 'shutdown))
;;   t)

;; (defmethod status ((self standard-thread))
;;   (and (sb-thread::thread-p (s-v 'local-thread))
;;        (thread-alive-p (s-v 'local-thread))))

;; (defparameter *debug-on-error* t)
;; (defmethod run ((self standard-thread))
;;   (let ((remote-thread nil))
;;     (flet ((control-loop-error (condition)
;; 	     (if *debug-on-error*
;; 		 (swank:swank-debugger-hook condition nil)               
;; 		 (invoke-restart 'ignore-error))))
;;       (loop
;; 	 (handler-bind ((error #'control-loop-error))
;; 	   (restart-case	     
;; 	       (let ((message (swank-backend:receive)))
;; 		 (cond
;; 		   ((eq message 'shutdown)
;; 		    (format t "Shutting down.~%")
;; 		    (return-from run (values)))
;; 		   ((atom message) nil)
;; 		   ((not (threadp (car message))) nil)
;; 		   (t (setq remote-thread (car message))
;; 		      (setq message (cdr message))
;; 		      (cond
;; 			((eq 'funcall (car message))
;; 			 (let ((result (funcall (cadr message))))
;; 			   (swank-backend:send remote-thread
;; 					       (list remote-thread
;; 						     'return result))))
;; 			((eq 'return (car message))
;; 			 (let ((k (gethash remote-thread (s-v 'continuations))))
;; 			   (when (functionp k)
;; 			     (funcall k (cadr message)))))
;; 			(t		  
;; 			 (format t "Got unknown message:~A~%" message))))))
;; 	     (ignore-error ()
;; 	       :report "Ignore the error and continue processing."
;; 	       (if remote-thread
;; 		   (swank-backend:send remote-thread (values))
;; 		   (setq remote-thread nil)))))))))

;; (defmacro defmethod/thread (name args &body body)
;;   (labels ((arg-names ()
;; 	     (extract-argument-names args :allow-specializers t))
;; 	   (self ()
;; 	     (car (arg-names))))
;;    `(progn
;;        (defmethod ,name ,args
;; 	 (flet ((method-lambda ,(arg-names)
;; 		  ,@body))
;; 	   (cond
;; 	     ((or (not (status ,(self)))
;; 		  (equal (swank-backend:current-thread)
;; 			 (slot-value ,(self) 'local-thread)))
;; 	      (format t "Executing real ~A.~%" ',name)
;; 	      (apply #'method-lambda (list ,@(arg-names))))
;; 	     (t
;; 	      (flet ((k (val)
;; 		       (return-from ,name val)))
;; 		(swank-backend:send
;; 		 (slot-value ,(self) 'local-thread)
;; 		 (list (swank-backend:current-thread)
;; 		       'funcall
;; 		       #'(lambda ()
;; 			   (format t "Executing in thread ~A.~%" ',name)
;; 			   (apply #'method-lambda (list ,@(arg-names))))))
;; 		(setf (gethash (swank-backend:current-thread) (slot-value ,(self) 'continuations))
;; 		      #'k)
;; 		(run self)))))))))

;; (defclass delayed-start ()
;;   ((delayed-started :initform nil)))

;; (defmethod start ((self delayed-start))
;;   (send (s-v 'local-thread) 'start))

;; (defmethod stop ((self delayed-start))
;;   (setf (s-v 'delayed-started) nil))

;; (defmethod run :before ((self delayed-start))
;;   (when (not (s-v 'delayed-started))
;;     (iter (for message = (swank-backend:receive))
;; 	  (until (eq 'start message)))
;;     (setf (s-v 'delayed-started) t)))

;; (defmethod/thread t1 ((self standard-thread) a b c)
;;   (list a b c))

;; (defmethod/thread t2 ((self standard-thread) gee)
;;   (format nil "gee is:~A" gee))

;; (defclass ring-unit (standard-thread)
;;   ((previous :accessor previous :initarg :previous :initform nil)
;;    (shared-state :accessor shared-state :initform 100 :allocation :class)))

;; (defmethod/thread send-message-to-previous ((self ring-unit) message)
;;   (format t "I'm thread:~A" (swank-backend:thread-id (current-thread)))
;;   (if (previous self)
;;       (send-message-to-previous (previous self) message)
;;       (format t "I've got the message:~A~%" message))
;;   (stop self))

;; (defmethod/thread counter ((self ring-unit))
;;   (if (and (> (shared-state self) 0)
;; 	   (previous self))
;;       (progn
;; 	(decf (shared-state self))
;; 	(format t "Current:~A, Thread:~S~%" (shared-state self) (thread-id (current-thread)))
;; 	(counter (previous self)))
;;       (format t "Done at:~A~%." (shared-state self)))
;;   (stop self))

;; (defun n-ring-units (how-many &aux (parent nil)) 
;;   (last1
;;    (mapcar #'(lambda (n)
;; 	       (let ((unit (make-instance 'ring-unit :previous parent)))
;; 		 (start unit)
;; 		 (setq parent unit)))
;; 	   (seq how-many))))

;; (defun cleanup ()
;;   (mapcar #'(lambda (thread)
;; 	      (if (equal (thread-name thread) "CLOS Thread")
;; 		  (kill-thread thread)))
;; 	  (all-threads)))

;; (defclass list-unit (standard-thread)
;;   ((shared-state :accessor shared-state :initform 100 :allocation :class)))

;; (defparameter *l2 (n-list-unit 20))
;; (defparameter *l1 (n-list-unit 20))

;; (defmethod run ((self list-unit))
;;   (mapcar #'(lambda (n)
;; 	      (if (member self *l2)
;; 		  (incf (shared-state self))
;; 		  (decf (shared-state self)))
;; 	      (format t "state:~A, Thread:~S~%" (shared-state self) (thread-id (current-thread))))
;; 	  (seq 50)))

;; (defmethod/thread counter+ ((self list-unit))
;;   (if (> (shared-state self) 0)
;;       (progn
;; ;;	(sleep 1)
;; 	(setf (shared-state self) (1+ (shared-state self)))
;; 	(format t "Current:~A, Thread:~S" (shared-state self) (thread-id (current-thread))))
;;       (format t "Done at:~A" (shared-state self)))
;;   (stop self))

;; (defmethod/thread counter- ((self list-unit))
;;   (if (> (shared-state self) 0)
;;       (progn
;; ;;	(sleep 1)
;; 	(setf (shared-state self) (1+ (shared-state self)))
;; 	(format t "Current:~A, Thread:~S" (shared-state self) (thread-id (current-thread))))
;;       (format t "Done at:~A" (shared-state self)))
;;   (stop self))

;; (defun n-list-units (how-many)
;;   (mapcar #'(lambda (n)
;; 	      (let ((unit (make-instance 'list-unit)))		
;; 		unit))
;; 	  (seq how-many)))

;; (defmethod/thread incf50 ((self list-unit))
;;   (incf (shared-state self) 50)
;;   (stop self))

;; (defmethod/thread incf20 ((self list-unit))
;;   ())


;; (defgeneric receive (thread)
;;   (:documentation "Receive messages and execute thunks."))

;; (defgeneric send (thread)
;;   (:documentation "Send messages to execute thunks on remote threads."))

;; (defmethod receive ((self standard-thread))
;;   )

;; (defmethod send ((self standard-thread))
;;   )

;; (defmethod shared-initialize :after ((self standard-thread) slot-names &key initargs
;; 				     &allow-other-keys)
;;   (declare (ignore slot-names initargs))
;;   ;; (when (s-v 'runningp)
;; ;;     (setf (s-v 'local-thread) (make-thread #'(lambda ()
;; ;; 					       (run self)))
;; ;; 	  (s-v 'runningp) t))
;;   )



