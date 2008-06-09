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
;;| Ticker Server
;;+----------------------------------------------------------------------------
;;
;; This file implements ticket server.
;;

(defun make-ticket-server (path &key (auto-start nil))
  "Returns a new ticket server instance, if 'auto-start' is t, server
is started"
  (make-instance 'ticket-server
		 :db (make-instance 'database-server
				    :db-auto-start auto-start
				    :model-class 'ticket-model
				    :directory path)))


(defmethod tickets ((self ticket-server))
  "Returns list of tickets that this server owns"
  (ticket-model.tickets (model (ticket-server.db self))))

(defun tx-add-ticket (system hash type &optional (used nil))
  "Transactional function used to add tickets"
  (let* ((model (model system))
	 (ticket (make-instance 'ticket :hash hash :type type :used used)))
    (setf (gethash hash (ticket-model.tickets model)) ticket)
    ticket))

;;-----------------------------------------------------------------------------
;; Ticket Server Procotol Implementation
;;-----------------------------------------------------------------------------
(defmethod add-ticket ((server ticket-server) hash type &optional used)
  (if (null (cadr (multiple-value-list (gethash hash (tickets server)))))
      (execute (ticket-server.db server) (make-transaction 'tx-add-ticket hash type used))
      (error "Ticket with this hash already exists!")))

(defmethod generate-tickets ((server ticket-server) amount type)
  (dotimes (i amount)
    (let ((hash (funcall (ticket-server.hash-fun server)
			 (ticket-model.tickets (model (ticket-server.db server))))))
      (execute (ticket-server.db server) (make-transaction 'tx-add-ticket hash type)))))

;;-----------------------------------------------------------------------------
;; Server Protocol Implementation
;;-----------------------------------------------------------------------------
(defmethod start ((self ticket-server))
  (start (ticket-server.db self)))

(defmethod stop ((self ticket-server))
  (stop (ticket-server.db self)))

(defmethod ticket-server.status ((self ticket-server))
  (status (ticket-server.db self)))

(defmethod status ((self ticket-server))
  (ticket-server.status self))
