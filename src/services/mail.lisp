(in-package :tr.gen.core.server)

;;; You can use whichever server you want. Just create a mail-sender and use it like that:

;;; (defparameter *mail-sender1* (make-instance 'mail-sender
;;; 					    :username "forexample123@gmail.com"
;;; 					    :password "secret"
;;; 					    :server "smtp.gmail.com"
;;; 					    :port 467))


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
	(:type (or space? visible-char?)))
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

(defclass mail-sender (local-unit)
  ((name :accessor mail-sender.name :initarg :name :initform "Mail Sender 1")
   (%socket :initform nil)
   (username :accessor mail-sender.username :initarg :username)
   (password :accessor mail-sender.password :initarg :password)
   (server :accessor mail-sender.server :initarg :server)
   (port :accessor mail-sender.port :initarg :port :initform 25)))

(defprint-object (self mail-sender)
  (format t "server: ~A, port: ~D" (mail-sender.server self) (mail-sender.port self)))

(defmethod/unit sendmail :async-no-return ((self mail-sender) rcpt message)
  (let* ((sock (s-v '%socket)))
    ;; TODO: how to program atomically dependent steps in here?
    (and     
     (prog1
	 (action-ok? sock) 
       (string! sock (format nil "MAIL FROM: ~A" (mail-sender.username self)))
       (char! sock #\Newline)
       (finish-output (slot-value sock '%stream)))
     (prog1
	 (action-ok? sock) 
       (string! sock (format nil "RCPT TO: ~A" rcpt))
       (char! sock #\Newline)
       (finish-output (slot-value sock '%stream)))
     (prog1
	 (action-ok? sock) 
       (string! sock (format nil "data"))
       (char! sock #\Newline)
       (finish-output (slot-value sock '%stream)))
     (prog1
	 (start-input? sock) 
       (string! sock message) 
       (char! sock #\Newline)
       (char! sock #\.)
       (char! sock #\Newline)
       (finish-output (slot-value sock '%stream)))
     (action-ok? sock))))


;;; make-mail :: String -> String -> String
(defun make-mail (subject message)
  (let ((s (make-core-stream "")))
    (string! s (format nil "Subject: ~A" (tr2en subject)))
    (char! s #\Newline)
    (string! s "MIME-Version: 1.0")
    (char! s #\Newline)
    (string! s "Content-Type: text/plain; charset=UTF-8; format=flowed")
    (char! s #\Newline)
    (string! s "Content-Transfer-Encoding: 8bit") 
    (char! s #\Newline)
    (char! s #\Newline)
    (string! s message)
    (char! s #\Newline)
    (return-stream s)))

(defmethod/unit auth-plain ((self mail-sender))
  (smsg self "Sending AUTH")
  (let ((s (s-v '%socket)))
    (string! s "AUTH PLAIN ")
    (and
     (auth-start? s)
     (base64! s (mail-sender.username self))
     (auth-start? s)
     (base64! s (mail-sender.password self))
     )
    
    (char! s #\Newline)
    (let ((res (parse-line? s)))
      (smsg self (format nil "got response \"~A\"" res)))))

(defun smsg (msender text)
  (log-me *logger* 'smtp (format nil "~A: ~A" (mail-sender.name msender) text)))

(defmethod start ((self mail-sender))
  (smsg self "Starting.")
  (aif (connect (mail-sender.server self) (mail-sender.port self)) 
       (progn
	 (smsg self "Connected to SMTP Server.")
	 (setf (s-v '%socket) it)
	 ;; connect
	 (service-ready? it)
	 (smsg self "Service ready.")
	 ;; hello
	 (string! it "HELO core.gen.tr")
	 (char! it #\Newline)
	 (finish-output (slot-value (s-v '%socket) '%stream))
	 ;; get greets
	 (action-ok? it)
	 (smsg self "Got helo."))
       (close-stream it)))

(defmethod stop ((self mail-sender))
  (when (s-v '%socket)
    (close-stream (s-v '%socket))
    (setf (s-v '%socket) nil)
    (smsg self "Connection closed.")))

(defmethod status ((self mail-sender))
  (slot-value self '%socket))

;;; Usage:
;;; (defparameter *s (make-instance 'mail-sender :from "aycan@core.gen.tr" :server "mail.core.gen.tr" :port 25))
;;; (send-message *s "evrim@core.gen.tr" "y00 mama check diz out")