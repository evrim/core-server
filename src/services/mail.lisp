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
;;| Mail Service
;;+----------------------------------------------------------------------------
;;
;; This file implements mail service
;;

;;; TODO: use auth whenever needed.

;;;;
;;;; Usage:
;;;;
;;
;; (defparameter *s (make-instance 'mail-sender
;; 				:username "aycan@core.gen.tr"
;; 				:password "l00kman0h4ndz"
;; 				:server "mail.core.gen.tr"
;; 				:port 25))
;; (start *s)
;; (mapcar (curry #'enqueue (mail-sender.queue *s)) *test-mails*)
;; (process *s)


;; We define a class for the main service object called
;; "mail-sender". This object has mandatory slots like username and
;; password used to connect to a mail server. mail server hostname and
;; port also required.
;;
;; We're adding mails to this sender's queue and upon service start,
;; we schedule a queue processor.

(defclass mail-sender (logger-server)
  ((username :accessor mail-sender.username :initarg :mail-username :initform nil
	     :documentation "Username for connecting to mail server")
   (password :accessor mail-sender.password :initarg :mail-password :initform nil
	     :documentation "Password for connecting to mail server")
   (server :accessor mail-sender.server :initarg :mail-server
	   :initform (error "mail-sender server must be defined.")
	   :documentation "mail server hostname")
   (mail-port :accessor mail-sender.port :initarg :mail-port :initform 25
	      :documentation "mail server port")
#+ssl
   (ssl :accessor mail-sender.ssl :initarg :mail-ssl :initform nil
	:documentation "enabled ssl/tls")
   (queue :accessor mail-sender.queue :initarg :queue :initform (make-instance 'queue)
	  :documentation "mail queue which will be processed with intervals")
   (timer :accessor mail-sender.timer :initarg :timer :initform nil
	  :documentation "A timer is an object holding a scheduled function")
   (interval :accessor mail-sender.interval :initarg :interval :initform 8
	     :documentation "Scheduling interval"))
  (:default-initargs :name "mail sender"))

;; recursively dequeue an envelope and send it to the wire
(defmethod/unit %process-queue ((self mail-sender) socket)
  (aif (dequeue (mail-sender.queue self))
       (progn 
	 (%sendmail self it socket)
	 (%process-queue self socket))
       nil))

;; when queue has envelopes, process the queue
(defmethod/unit %process :async-no-return ((self mail-sender))
  (when (< 0 (queue-count (mail-sender.queue self)))
    (smsg self (format nil "Connecting ~A:~D." (mail-sender.server self) (mail-sender.port self)))
    (flet (#+ssl
	   (do-ssl ()
	     (aif (connect (mail-sender.server self) (mail-sender.port self)) 
		  (progn
		    (smsg self "Connected to SMTP Server.")
		    ;; 220 node2.core.gen.tr ESMTP
		    (smtp? it) ;; should be < 400
		    (smsg self "Service ready. Starting TLS")
		    (smtp! it "STARTTLS")
		    (smtp? it)
		    (let ((it (make-core-stream
			       (cl+ssl:make-ssl-client-stream (slot-value it '%stream)))))
		      (smsg self "TLS started.")
		      ;; Say hello
		      (smtp-ehlo :stream it) ;; should be < 400
		      (smsg self (format nil "Got HELO from ~A." (mail-sender.server self)))
		      ;; 235 2.0.0 OK Authenticated
		      (if (mail-sender.password self)
			  (smtp-auth-plain :username (mail-sender.username self)
					   :password (mail-sender.password self)
					   :stream it)) ;; shoudl be < 400
		      (%process-queue self it)
		      (smtp-quit :stream it)
		      ;; close conn
		      (close-stream it)
		      (smsg self "Connection closed.")
		      (close-stream it)))))
	   (do-plain ()
	     (aif (connect (mail-sender.server self) (mail-sender.port self)) 
		  (progn
		    (smsg self "Connected to SMTP Server.")
		    ;; 220 node2.core.gen.tr ESMTP
		    (smtp? it) ;; should be < 400
		    (smsg self "Service ready.")
		    ;; Say hello
		    (smtp-ehlo :stream it) ;; should be < 400
		    (smsg self (format nil "Got HELO from ~A." (mail-sender.server self)))
		    ;; 235 2.0.0 OK Authenticated
		    (if (mail-sender.password self)
			(smtp-auth-plain :username (mail-sender.username self)
					 :password (mail-sender.password self)
					 :stream it)) ;; shoudl be < 400
		    (%process-queue self it)
		    (smtp-quit :stream it)
		    ;; close conn
		    (close-stream it)
		    (smsg self "Connection closed."))
		  (close-stream it))))
      #+ssl (if (mail-sender.ssl self)
		(do-ssl)
		(do-plain))
      #-ssl (do-plain))))

;; write an envelope to the wire
(defmethod %sendmail ((self mail-sender) (e envelope) (s core-stream))
  (smsg self (format nil "Sending message: ~A" e))
  (smtp-send :envelope e :stream s))

;; Use user supplied timer, otherwise make a new timer and schedule
;; it.
(defmethod start ((self mail-sender))
  (unless (mail-sender.timer self)
    (setf (mail-sender.timer self)
	  (make-timer (lambda ()
			(%process self))
		      :name "mail-sender"
		      :thread (s-v '%thread))))
  (schedule-timer (mail-sender.timer self)
		  (mail-sender.interval self)
		  :repeat-interval (mail-sender.interval self)))

;; remove the scheduled timer and stop.
(defmethod stop ((self mail-sender))
  (when (mail-sender.timer self)
    (unschedule-timer (mail-sender.timer self))
    (setf (mail-sender.timer self) nil)))

;; we're also inheriting logger-server. So here we define a logging
;; function with a default tag 'smtp.
(defun smsg (msender text)
  (log-me msender 'smtp (format nil "~A: ~A" (unit.name msender) text)))

;; main interface to other programs
(defmethod/unit sendmail :async-no-return ((self mail-sender) from to subject text
					   &optional cc reply-to display-name)
  (enqueue (mail-sender.queue self)
	   (apply #'make-instance 'envelope
		  (list :from from :to (ensure-list to) :subject subject :text text
			:cc (ensure-list cc) :reply-to reply-to :display-name display-name))))

;; (defparameter *test-mails*
;;   (list
;;    (make-instance 'envelope
;; 		  :from "bilgi@core.gen.tr"
;; 		  :to '("aycan@core.gen.tr")
;; 		  :subject "first mail (1)"
;; 		  :text (with-core-stream (s "")
;; 			  (with-html-output s
;; 			    (<:html
;; 			     (<:head (<:title "taytl"))
;; 			     (<:body
;; 			      (<:h1 "Heading 1")
;; 			      (<:p "Lorem ipsum okuzum malim."))))))
;;    (make-instance 'envelope
;; 		  :from "bilgi@core.gen.tr"
;; 		  :to '("aycan@core.gen.tr")
;; 		  :subject "second mail (2)"
;; 		  :text "number two (2)")
;;    (make-instance 'envelope
;; 		  :from "bilgi@core.gen.tr"
;; 		  :to  '("aycan@core.gen.tr")
;; 		  :subject "third mail (3)"
;; 		  :text "number three (3)")))


;;; sample mail conversation
;;;
;;; aycan.irican@node2 ~ $ nc mail.core.gen.tr 25
;;; 220 node2.core.gen.tr ESMTP
;;; HELO core.gen.tr
;;; 250 node2.core.gen.tr
;;; mail from: aycan@core.gen.tr
;;; 250 2.1.0 Ok
;;; rcpt to: evrim@core.gen.tr
;;; 250 2.1.5 Ok
;;; data
;;; 354 End data with <CR><LF>.<CR><LF>
;;; deneme evrimcim deneme
;;; .
;;; 250 2.0.0 Ok: queued as 62E6818ABA

;; (let ((c (connect "mail.core.gen.tr" 25)))
;;   (smtp? c)
;;   (smtp! c "STARTTLS")
;;   (describe (smtp? c))
;;   (let ((c (make-core-stream
;; 	     (cl+ssl:make-ssl-client-stream (slot-value c '%stream)))))
;; ;;     (describe (smtp? c))
;;     (smtp-ehlo :stream c)
;;     (smtp-auth-plain :username ""
;; 		     :password ""
;; 		     :stream c)
;;     (smtp-send :envelope (make-instance
;; 			  'envelope
;; 			  :from "evrimulu@gmail.com" 
;; 			  :to (list "evrim@core.gen.tr")
;; 			  :subject "subject" :text "text")
;; 	       :stream c)
;;     (smtp? c)))