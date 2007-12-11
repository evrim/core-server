(in-package :tr.gen.core.server)

;;;-----------------------------------------------------------------------------
;;; POSIX THREADS COMPATIBILITY LAYER, got from slime/swank mostly.
;;;-----------------------------------------------------------------------------

(declaim (type hash-table *thread-mailboxes*))

(defvar *thread-mailbox-lock* (make-lock "Thread Mailbox Lock")
  "Lock for manipulating *thread-mailboxes*")
(defvar *thread-mailboxes* (make-hash-table :test #'eq :weakness :value)
  "A global place to store thread mailboxes")

;; (defmethod make-condition-variable ()
;;   (sb-thread:make-waitqueue))

;; (defmethod condition-wait ((condition-variable sb-thread:waitqueue)
;; 			   (lock sb-thread:mutex))
;;   (sb-thread:condition-wait condition-variable lock))

;; (defmethod condition-notify ((condition-variable sb-thread:waitqueue))
;;   (sb-thread:condition-notify condition-variable))

;; (defmethod thread-yield ()
;;   (sb-thread:release-foreground))

(defstruct (thread-mailbox (:conc-name mailbox.))
  thread
  (lock (make-lock "A Thread Mailbox Lock"))
  (waitqueue (make-condition-variable))
  (queue '() :type list))

;; TODO: not portable
(defun thread-alive-p (thread)
  (swank::thread-alive-p thread))

;; TODO: not portable
;; (defmethod threadp ((thread t))
;;   nil)

;; (defmethod threadp ((object sb-thread:thread))
;;   t)

;; (defun current-thread ()
;;   (swank::current-thread))

;; (defmethod make-thread (function &key name)
;;   (swank::spawn function :name name))

;; (defmethod destroy-thread ((thread sb-thread:thread))
;;   (sb-thread:terminate-thread thread))

;; TODO: not portable
(defun find-thread-mailbox (thread)
  (gethash (sb-thread::thread-os-thread thread) *thread-mailboxes*))

;; (defmacro with-lock-held ((lock) &body body)
;;   `(swank::call-with-lock-held ,lock
;; 			       (lambda ()
;; 				 ,@body)))

;; TODO: not portable
(defun thread-mailbox (thread)
  "Return thread's mailbox."
  (with-lock-held (*thread-mailbox-lock*)
    (or (find-thread-mailbox thread)	  
	(setf
	 (gethash (sb-thread::thread-os-thread thread) *thread-mailboxes*)
	 (make-thread-mailbox :thread thread)))))

(defun thread-send (thread message)
  "Send message to a thread."
  (let* ((mbox (thread-mailbox thread))
	 (lock (mailbox.lock mbox)))
    (with-lock-held (lock)
      (setf (mailbox.queue mbox)
	    (nconc (mailbox.queue mbox) (list message)))
      (condition-notify (mailbox.waitqueue mbox)))))

(defun thread-receive (&optional (thread (current-thread)))
  "Pop message from queue."
  (let* ((mbox (thread-mailbox thread))
	 (lock (mailbox.lock mbox)))
    (with-lock-held (lock)
      (loop
	 (let ((q (mailbox.queue mbox)))
	   (cond (q (return (pop (mailbox.queue mbox))))
		 (t (condition-wait (mailbox.waitqueue mbox)
				    lock))))))))

;; TODO: not portable
(defun cleanup-mailbox (&optional (thread (current-thread)))
  "Cleanup a threads mailbox. By default it cleans up current-thread's mailbox."
  (with-lock-held (*thread-mailbox-lock*)
    (remhash (sb-thread::thread-os-thread thread) *thread-mailboxes*)))

(defun thread-spawn (fn &key name)
  "Make a new thread with the given function and name. Returns a thread."
  (make-thread #'(lambda () (funcall (the function fn)) (cleanup-mailbox)) :name name))

(defun thread-kill (thread)
  "Destroy thread and cleanup it's mailbox"
  (destroy-thread thread)
  (cleanup-mailbox thread))
