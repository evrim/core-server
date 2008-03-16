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

(defparameter *x-mailer* "core-server")

;; thanks to cl-smtp for get-email-date-string and get-timezone-from-integer
(defun get-email-date-string ()
  (multiple-value-bind (sec min h d m y wd)
      (get-decoded-time)
    (let* ((month (elt '("Jan" "Feb" "r" "Apr" "y" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec") (- m 1)))
	   (weekday (elt '("n" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") wd))
	   (timezone (get-timezone-from-integer
		      (- (encode-universal-time sec min h d m y 0)
			 (get-universal-time)))))
      (format nil "~A, ~2,'0d ~A ~d ~2,'0d:~2,'0d:~2,'0d ~D"
	      weekday d month y h min sec timezone))))

(defun get-timezone-from-integer (x)
  (let ((min (/ x 60))
	(hour (/ x 3600)))
    (if (integerp hour)
	(cond
	  ((>= hour 0) (format nil "+~2,'0d00" hour))
	  ((< hour 0) (format nil "-~2,'0d00" (* -1 hour))))
	(multiple-value-bind (h m)
	    (truncate min 60)
	  (cond
	    ((>= hour 0) (format nil "+~2,'0d~2,'0d" h (truncate m)))
	    ((< hour 0) (format nil "-~2,'0d~2,'0d" (* -1 h) (* -1 (truncate m)))))))))

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
   (timer :accessor mail-sender.timer :initarg :timer :initform nil
	  :documentation "A timer is an object holding a scheduled function")
   (interval :accessor mail-sender.interval :initarg :interval :initform 8
	     :documentation "Scheduling interval"))
  (:default-initargs :name "mail sender"))

;; This is the mail adt.
(defclass envelope ()
  ((from :accessor envelope.from :initarg :from :initform (error "from field required for envelope")
	 :documentation "envelope from field")
   (display-name :accessor envelope.display-name :initarg :display-name :initform nil
		 :documentation "Display name of the sender. Ex: john doe")
   (to :accessor envelope.to :initarg :to :initform (error "to field required for envelope")
	      :documentation "Recipient of the mail")
   (cc :accessor envelope.cc :initarg :cc :initform nil
       :documentation "carbon copy goes to")
   (relpy-to :accessor envelope.reply-to :initarg :reply-to :initform nil
	     :documentation "reply to address")
   (subject :accessor envelope.subject :initarg :subject :initform (error "subject field required for envelope")
	    :documentation "mail subject")
   (text :accessor envelope.text :initarg :text :initform ""
	 :documentation "mail body which is a string")
   (date :accessor envelope.date :initarg :date :initform (get-email-date-string)
	 :documentation "creation date of envelope")
   (extra-headers :accessor envelope.extra-headers :initarg :extra-headers :initform nil
		  :documentation "extra headers for this envelope. List of string tuples.")))

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
	 (string! socket (format nil "MAIL FROM: ~@[~A ~]<~A>" (envelope.display-name e) (envelope.from e)))
	 (char! socket #\Newline)
	 (purge)
	 ;; 250 2.1.0 Ok
	 (action-ok? socket))
       (progn 
	 (string! socket (format nil "RCPT TO: ~A" (car (envelope.to e))))
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
  (flet ((smtp! (sock string)
	   (string! sock string)
	   (char! sock #\Newline)))
    (let ((s (make-core-stream "")))
      (smtp! s (format nil "Date: ~A" (envelope.date e)))
      (smtp! s (format nil "From: ~@[~A <~]~A~@[>~]" (envelope.display-name e) (envelope.from e) (envelope.display-name e)))
      (smtp! s (format nil "To: ~{ ~a~^,~}" (envelope.to e)))
      (when (envelope.cc e)
	(smtp! s (format nil "Cc: ~{ ~a~^,~}" (envelope.cc e))))
      (when (envelope.reply-to e)
	(smtp! s (format nil "Reply-To: ~A" (envelope.reply-to e))))
      (smtp! s (format nil "Subject: ~A" (envelope.subject e)))
      (smtp! s (format nil "X-Mailer: ~A" *x-mailer*))
      (let ((hdrs (envelope.extra-headers e)))
	(when (and hdrs (listp hdrs))
	  (dolist (h hdrs)
	    (smtp! s (format nil "~A: ~A" (car h) (cdr h))))))
      (smtp! s "MIME-Version: 1.0")
      (smtp! s "Content-Type: text/html; charset=UTF-8; format=flowed")
      (smtp! s "Content-Transfer-Encoding: 8bit")
      (char! s #\Newline)
      (string! s (envelope.text e))
      (char! s #\Newline)
      (return-stream s))))

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
 
(defparameter *test-mails*
  (list
   (make-instance 'envelope
		  :from "bilgi@core.gen.tr"
		  :to '("aycan@core.gen.tr")
		  :subject "first mail (1)"
		  :text (with-core-stream (s "")
			  (with-html-output s
			    (<:html
			     (<:head (<:title "taytl"))
			     (<:body
			      (<:h1 "Heading 1")
			      (<:p "Lorem ipsum okuzum malim."))))))
   (make-instance 'envelope
		  :from "bilgi@core.gen.tr"
		  :to '("aycan@core.gen.tr")
		  :subject "second mail (2)"
		  :text "number two (2)")
   (make-instance 'envelope
		  :from "bilgi@core.gen.tr"
		  :to  '("aycan@core.gen.tr")
		  :subject "third mail (3)"
		  :text "number three (3)")))

;; main interface to other programs
(defmethod/unit sendmail :async-no-return ((self mail-sender) from to subject text &optional cc reply-to display-name)
  (enqueue (mail-sender.queue self)
	   (apply #'make-instance 'envelope
		  (list :from from :to (ensure-list to) :subject subject :text text
			:cc (ensure-list cc) :reply-to reply-to :display-name display-name))))

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

