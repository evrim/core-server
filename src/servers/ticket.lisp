(in-package :tr.gen.core.server)

(defun make-ticket-server (path &key (auto-start nil))
  (make-instance 'ticket-server :db (make-instance 'database-server :db-auto-start auto-start :model-class 'ticket-model :directory path)))

(defmethod start ((self ticket-server))
  (start (ticket-server.db self)))

(defmethod stop ((self ticket-server))
  (stop (ticket-server.db self)))

(defmethod status ((self ticket-server))
  (status (ticket-server.db self)))

(defmethod tickets ((self ticket-server))
  (ticket-model.tickets (model (ticket-server.db self))))

(defun tx-add-ticket (system hash type &optional (used nil))
  (let* ((model (model system))
	 (ticket (make-instance 'ticket :hash hash :type type :used used)))
    (setf (gethash hash (ticket-model.tickets model)) ticket)
    ticket))

(defmethod add-ticket ((server ticket-server) hash type &optional used) 
  (if (null (cadr (multiple-value-list (gethash hash (tickets server)))))
      (execute (ticket-server.db server) (make-transaction 'tx-add-ticket hash type used))
      (error "Ticket with this hash already exists!")))

(defmethod generate-tickets ((server ticket-server) amount type)
  (iter (for i from 1 to amount)
	(let ((hash (funcall (ticket-server.hash-fun server) (ticket-model.tickets (model (ticket-server.db server))))))
	  (execute (ticket-server.db server) (make-transaction 'tx-add-ticket hash type)))))
