(in-package :tr.gen.core.server)

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


;; we store smtp codes for ease of remember
;; *smtp-codes* :: [ (ResponseCodes, Description) ]
(defparameter *smtp-codes*
  '((220 . "connect")
    (235 . "auth success")
    (250 . "mail from, rcpt to")
    (334 . "auth plain")
    (354 . "data")
    (554 . "fail")))

;; default smtp response parser
;; default-smtp-response? :: Socket () -> ResponseCode
(defrule default-smtp-response? (c)
  (:lwsp?)
  (:fixnum? c)
  (:zom (:not #\Newline)
	(:type (or space? octet?)))
  (:return c))

;; we define different response code parsers here
;; rc-parsers :: Socket () -> Boolean

(defun service-ready? (sock)
  (eq 220 (default-smtp-response? sock)))
(defun action-ok? (sock)
  (eq 250 (default-smtp-response? sock)))
(defun start-input? (sock)
  (eq 354 (default-smtp-response? sock)))
(defun auth-start? (sock)
  (eq 334 (default-smtp-response? sock)))
(defun auth-success? (sock)
  (eq 235 (default-smtp-response? sock)))

;; We define a class for the main service object called
;; "mail-sender". This object has mandatory slots like username and
;; password used to connect to a mail server. mail server hostname and
;; port also required.
;;
;; We're adding mails to this sender's queue and upon service start,
;; we schedule a queue processor.

(defclass mail-sender (logger-server)
  ((username :accessor mail-sender.username :initarg :mail-username
	     :initform (error "mail-sender username must be defined.")
	     :documentation "Username for connecting to mail server")
   (password :accessor mail-sender.password :initarg :mail-password :initform nil
	     :documentation "Password for connecting to mail server")
   (server :accessor mail-sender.server :initarg :mail-server
	   :initform (error "mail-sender server must be defined.")
	   :documentation "mail server hostname")
   (mail-port :accessor mail-sender.port :initarg :mail-port :initform 25
	      :documentation "mail server port")
   (queue :accessor mail-sender.queue :initarg :queue :initform (make-instance 'queue)
	  :documentation "mail queue which will be processed with intervals")
   (queue-lock :accessor mail-sender.queue-lock :initarg :queue-lock :initform (sb-thread::make-mutex)
	       :documentation "queue lock used to separate multiple worker threads")
   (timer :accessor mail-sender.timer :initarg :timer :initform nil
	  :documentation "A timer is an object holding a scheduled function")
   (interval :accessor mail-sender.interval :initarg :interval :initform 8
	     :documentation "Scheduling interval"))
  (:default-initargs :name "mail sender"))

;; This is the mail adt.
(defclass envelope ()
  ((recipient :accessor envelope.recipient :initarg :recipient
	      :documentation "Recipient of the mail")
   (subject :accessor envelope.subject :initarg :subject
	    :documentation "mail subject")
   (text :accessor envelope.text :initarg :text :initform nil
	 :documentation "mail body which is a string")))

;; recursively dequeue an envelope and send it to the wire
(defmethod/unit %process-queue ((self mail-sender) socket)
  (sb-thread::with-recursive-lock ((mail-sender.queue-lock self))
    (aif (dequeue (mail-sender.queue self))
	 (progn 
	   (%sendmail self it socket)
	   (%process-queue self socket))
	 nil)))

;; when queue has envelopes, process the queue
(defmethod/unit %process :async-no-return ((self mail-sender))
  (when (< 0 (queue-count (mail-sender.queue self)))
    (smsg self (format nil "Connecting ~A:~D." (mail-sender.server self) (mail-sender.port self)))
    (aif (connect (mail-sender.server self) (mail-sender.port self)) 
	 (progn
	   (smsg self "Connected to SMTP Server.")
	   ;; 220 node2.core.gen.tr ESMTP
	   (service-ready? it)
	   (smsg self "Service ready.")
	   (string! it "HELO core.gen.tr")
	   (char! it #\Newline)
	   (finish-output (slot-value it '%stream)) 
	   ;; 250 node2.core.gen.tr
	   (action-ok? it)
	   (smsg self (format nil "Got HELO from ~A." (mail-sender.server self)))
	   ;; 235 2.0.0 OK Authenticated
	   (if (mail-sender.password self)
	       (auth-plain self it))
	   (%process-queue self it)
	   ;; close conn
	   (close-stream it)
	   (smsg self "Connection closed."))
	 (close-stream it))))

;; write an envelope to the wire
(defmethod %sendmail ((self mail-sender) (e envelope) socket)
  (let ((stream (slot-value socket '%stream)))
    (flet ((purge ()
	     (finish-output stream)))
      ;; TODO: how to program atomically dependent steps in here?
      (and
       (progn
	 (string! socket (format nil "MAIL FROM: ~A" (mail-sender.username self)))
	 (char! socket #\Newline)
	 (purge)
	 ;; 250 2.1.0 Ok
	 (action-ok? socket))
       (progn 
	 (string! socket (format nil "RCPT TO: ~A" (envelope.recipient e)))
	 (char! socket #\Newline)
	 (purge)
	 ;; 250 2.1.5 Ok
	 (action-ok? socket))
       (progn
	 (string! socket (format nil "data"))
	 (char! socket #\Newline)
	 (purge)
	 ;; 354 End data with <CR><LF>.<CR><LF>
	 (start-input? socket))
       (progn 
	 (string! socket (build-message-stream e)) 
	 (char! socket #\Newline)
	 (char! socket #\.)
	 (char! socket #\Newline)
	 (purge)
	 ;; 250 2.0.0 Ok: queued as 62E6818ABA
	 (action-ok? socket))))))

;; cosmetics for the message body
;; build-message-stream :: Envelope -> String
(defmethod build-message-stream ((e envelope))
  (let ((s (make-core-stream "")))
    (string! s (format nil "Subject: ~A" (envelope.subject e)))
    (char! s #\Newline)
    (string! s (format nil "To: ~A" (envelope.recipient e)))
    (char! s #\Newline)
    (string! s "MIME-Version: 1.0")
    (char! s #\Newline)
    (string! s "Content-Type: text/plain; charset=UTF-8; format=flowed")
    (char! s #\Newline)
    (string! s "Content-Transfer-Encoding: 8bit") 
    (char! s #\Newline)
    (char! s #\Newline)
    (string! s (envelope.text e))
    (char! s #\Newline)
    (return-stream s)))

;; TODO: fix authentication
(defmethod/unit auth-plain ((self mail-sender) socket)
  (smsg self "Sending AUTH")
  (string! socket "AUTH PLAIN ")
  (and
   (auth-start? socket)
   (base64! socket (mail-sender.username self))
   (auth-start? socket)
   (base64! socket (mail-sender.password self)))
  (char! socket #\Newline)
  (let ((res (parse-line? socket)))
    (smsg self (format nil "got response \"~A\"" res))))

;; Use user supplied timer, otherwise make a new timer and schedule
;; it.
(defmethod start ((self mail-sender))
  (unless (mail-sender.timer self) 
    (setf (mail-sender.timer self)
	  (make-timer (lambda ()
			(%process self))
		      :name "mail-sender")))
  (schedule-timer (mail-sender.timer self)
		  (mail-sender.interval self)
		  :repeat-interval (mail-sender.interval self))
  t)

;; remove the scheduled timer and stop.
(defmethod stop ((self mail-sender))
  (when (mail-sender.timer self)
    (unschedule-timer (mail-sender.timer self))))

;; we're also inheriting logger-server. So here we define a logging
;; function with a default tag 'smtp.
(defun smsg (msender text)
  (log-me msender 'smtp (format nil "~A: ~A~%" (unit.name msender) text)))
 
(defparameter *test-mails*
  (list
   (make-instance 'envelope
		  :recipient "aycan@core.gen.tr"
		  :subject "first mail (1)"
		  :text "number one (1)")
   (make-instance 'envelope
		  :recipient "aycan@core.gen.tr"
		  :subject "second mail (2)"
		  :text "number two (2)")
   (make-instance 'envelope
		  :recipient "aycan@core.gen.tr"
		  :subject "third mail (3)"
		  :text "number three (3)")))

;; main interface to other programs
(defmethod/unit sendmail :async-no-return ((self mail-sender) recipient subject text)
  (enqueue (mail-sender.queue self)
	   (apply #'make-instance 'envelope
		  (list :recipient recipient :subject subject :text text))))

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

