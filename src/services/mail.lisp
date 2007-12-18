(in-package :tr.gen.core.server)

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
    (250 . "mail from, rcpt to")
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

(defclass mail-sender (local-unit)
  ((from :accessor mail-sender.from :initarg :from)
   (server :accessor mail-sender.server :initarg :server)
   (port :accessor mail-sender.port :initarg :port :initform 25)))

(defmethod/unit sendmail :async-no-return ((self mail-sender) rcpt message)
  (let* ((sock (connect (mail-sender.server self) (mail-sender.port self))))
    ;; TODO: how to program atomically dependent steps in here?
    (and
     (prog1
	 (service-ready? sock) 
       (string! sock "HELO core.gen.tr")
       (char! sock #\Newline)
       (finish-output (slot-value sock '%stream)))
     (prog1
	 (action-ok? sock) 
       (string! sock (format nil "MAIL FROM: ~A" (mail-sender.from self)))
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
     (action-ok? sock))
    (close-stream sock)))

;;; (defmethod start ((self mail-sender))
;;;   t)
;;; (defmethod stop ((self mail-sender))
;;;   t)
;;; (defmethod status ((self mail-sender))
;;;   )

;;; Usage:
;;; (defparameter *s (make-instance 'mail-sender :from "aycan@core.gen.tr" :server "mail.core.gen.tr" :port 25))
;;; (send-message *s "evrim@core.gen.tr" "y00 mama check diz out")