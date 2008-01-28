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

(defparameter *smtp-codes*
  '((220 . "connect")
    (235 . "auth success")
    (250 . "mail from, rcpt to")
    (334 . "auth plain")
    (354 . "data")
    (554 . "fail")))

(defrule default-smtp-response? (c)
  (:lwsp?)
  (:fixnum? c)
  (:zom (:not #\Newline)
	(:type (or space? octet?)))
  (:return c))

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

(defclass mail-sender (logger-server)
  ((username :accessor mail-sender.username :initarg :username :initform (error "mail-sender username must be defined."))
   (password :accessor mail-sender.password :initarg :password :initform nil)
   (server :accessor mail-sender.server :initarg :server :initform (error "mail-sender server must be defined."))
   (mail-port :accessor mail-sender.port :initarg :mail-port :initform 25)
   (queue :accessor mail-sender.queue :initarg :queue :initform (make-instance 'queue))
   (queue-lock :accessor mail-sender.queue-lock :initarg :queue-lock :initform (sb-thread::make-mutex))
   (timer :accessor mail-sender.timer :initarg :timer :initform nil)
   (interval :accessor mail-sender.interval :initarg :interval :initform 8))
  (:default-initargs :name "mail sender"))

(defclass envelope ()
  ((recipient :accessor envelope.recipient :initarg :recipient)
   (subject :accessor envelope.subject :initarg :subject)
   (text :accessor envelope.text :initarg :text :initform nil)))

(defmethod/unit %process-queue ((self mail-sender) socket)
  (sb-thread::with-recursive-lock ((mail-sender.queue-lock self))
    (aif (dequeue (mail-sender.queue self))
	 (progn 
	   (%sendmail self it socket)
	   (%process-queue self socket))
	 nil)))

;; write a function that acquires a lock and starts sending mails
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

;;; build-message-stream :: Envelope -> String
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

;; TODO: fox authentication
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

(defmethod start ((self mail-sender))
  (unless (mail-sender.timer self) 
    (setf (mail-sender.timer self)
	  (make-timer (lambda ()
			(%process self))
		      :name "mail-sender")))
  (schedule-timer (mail-sender.timer self)
		  (mail-sender.interval self)
		  :repeat-interval (mail-sender.interval self)))

(defmethod stop ((self mail-sender))
  (unschedule-timer (mail-sender.timer self)))

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

(defmethod/unit sendmail :async-no-return ((self mail-sender) recipient subject text)
  (enqueue (mail-sender.queue self) (apply #'make-instance 'envelope (list :recipient recipient :subject subject :text text))))

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

