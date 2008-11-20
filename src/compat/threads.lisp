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
;;| POSIX Threads Compatiblity Functions
;;+----------------------------------------------------------------------------
;;
;; This file contains compat layer for POSIX Threads

(declaim (type hash-table *thread-mailboxes*))

(defvar *thread-mailbox-lock* (make-lock "Thread Mailbox Lock")
  "Lock for manipulating *thread-mailboxes*")

(defvar *thread-mailboxes* (make-hash-table :test #'eq :weakness :value)
  "A global place to store thread mailboxes")

(defstruct (thread-mailbox (:conc-name mailbox.))
  thread
  (lock (make-lock "A Thread Mailbox Lock"))
  (waitqueue (make-condition-variable))
  (queue '() :type list))

;; TODO: not portable
(defun thread-alive-p (thread)
  "Return t if 'thread' is alive and running"
  (sb-thread:thread-alive-p thread))

;; TODO: not portable
(defun find-thread-mailbox (thread)
  "Return the mailbox for the 'thread'"
  (gethash (sb-thread::thread-os-thread thread) *thread-mailboxes*))

;; TODO: not portable
(defun thread-mailbox (thread)
  "Return thread's mailbox, creates if none exists"
  (with-lock-held (*thread-mailbox-lock*)
    (or (find-thread-mailbox thread)
	(setf
	 (gethash (sb-thread::thread-os-thread thread) *thread-mailboxes*)
	 (make-thread-mailbox :thread thread)))))

(defun thread-send (thread message)
  "Send message to a 'thread'"
  (let* ((mbox (thread-mailbox thread))
	 (lock (mailbox.lock mbox)))
    (with-lock-held (lock)
      (setf (mailbox.queue mbox)
	    (nconc (mailbox.queue mbox) (list message)))
      (condition-notify (mailbox.waitqueue mbox)))))

(defun thread-receive (&optional (thread (current-thread)) (non-block nil))
  "Waits until a message pops from the thread queue"
  (let* ((mbox (thread-mailbox thread))
	 (lock (mailbox.lock mbox)))
    (with-lock-held (lock)
      (loop
	 (let ((q (mailbox.queue mbox)))
	   (cond (q (return (pop (mailbox.queue mbox))))
		 (non-block (return nil))
		 (t (condition-wait (mailbox.waitqueue mbox)
				    lock))))))))

;; TODO: not portable
(defun cleanup-mailbox (&optional (thread (current-thread)))
  "Cleanup a threads mailbox. By default it cleans up current-thread's mailbox"
  (with-lock-held (*thread-mailbox-lock*)
    (remhash (sb-thread::thread-os-thread thread) *thread-mailboxes*)))

(defun thread-spawn (fn &key name)
  "Make a new thread with the given function and name. Returns a thread."
  (make-thread #'(lambda ()
		   (unwind-protect (funcall (the function fn))
		     (cleanup-mailbox)))
	       :name name))

(defun thread-kill (thread)
  "Destroy thread and cleanup it's mailbox"
  (destroy-thread thread)
  (cleanup-mailbox thread))

;; TODO: What to do with below? -evrim

;; (defmethod make-condition-variable ()
;;   (sb-thread:make-waitqueue))

;; (defmethod condition-wait ((condition-variable sb-thread:waitqueue)
;; 			   (lock sb-thread:mutex))
;;   (sb-thread:condition-wait condition-variable lock))

;; (defmethod condition-notify ((condition-variable sb-thread:waitqueue))
;;   (sb-thread:condition-notify condition-variable))

;; (defmethod thread-yield ()
;;   (sb-thread:release-foreground))

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

;; (defmacro with-lock-held ((lock) &body body)
;;   `(swank::call-with-lock-held ,lock
;; 			       (lambda ()
;; 				 ,@body)))

