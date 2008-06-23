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
;;| RFC 2821 SMTP Protocol
;;+----------------------------------------------------------------------------
;;
;; This file implement only client side operations
;;

(defcommand smtp ()
  ((code :host remote :accessor smtp.code :initarg :code :initform nil)
   (message :host local :accessor smtp.message :initarg :message :initform nil)
   (stream :host local :accessor smtp.stream :initarg :stream :initform nil)))

;; EHLO localhost
;; 250-node2.core.gen.tr
;; 250-PIPELINING
;; 250-SIZE 100000000
;; 250-ETRN
;; 250-STARTTLS
;; 250-ENHANCEDSTATUSCODES
;; 250-8BIT
;; 250 DSN
(defparser smtp-ehlo? (c (acc (make-accumulator)) params)
  (:lwsp?)
  (:zom (:seq "250-")
	(:lwsp?)
	(:zom (:not #\Newline) (:not #\Return)
	      (:type octet? c) (:collect c acc))
	(:lwsp?)
	(:do (push acc params) (setf acc (make-accumulator))))
  (:seq "250 ")
  (:zom (:not #\Newline) (:not #\Return)
	(:type octet? c) (:collect c acc))
  (:return (values 250 acc (nreverse params))))

(defrule smtp? (c temp (acc (make-accumulator)))
  (:lwsp?) (:fixnum? c) (:lwsp?)
  (:zom (:not #\Newline) (:not #\Return)
	(:type octet? temp) (:collect temp acc))
  (:return (values c acc)))

(defrender smtp! (message)
  (:string! message) (:char! #\Newline))

(defparser line? (c (acc (make-accumulator)))
  (:zom (:not #\Newline)
	(:type octet? c) (:collect c acc))
  (:return acc))

(defmethod run ((self smtp))
  (if (smtp.message self)
      (smtp! (smtp.stream self) (smtp.message self)))
  (or (smtp-ehlo? (smtp.stream self))
      (smtp? (smtp.stream self))))

(defcommand smtp-ehlo (smtp)
  ()
  (:default-initargs :message "EHLO localhost"))

(defcommand smtp-quit (smtp)
  ()
  (:default-initargs :message "QUIT"))

(defcommand smtp-auth-plain (smtp)
  ((username :host local :initarg :username :initform nil)
   (password :host local :initarg :password :initform nil)))

(defmethod run ((self smtp-auth-plain))
  (when (and (s-v 'username) (s-v 'password))    
    (smtp! (s-v 'stream)
	   (with-core-stream (s "")
	     (string! s "AUTH PLAIN ")
	     (base64! s (with-core-stream (s "")
			  (write-stream s 0)
			  (string! s (s-v 'username))
			  (write-stream s 0)
			  (string! s (s-v 'password))
			  (return-stream s)))
	     (return-stream s)))
    (smtp? (s-v 'stream))))

(defcommand smtp-auth-login (smtp-auth-plain)
  ())

(defmethod run ((self smtp-auth-login))
  (when (and (s-v 'username) (s-v 'password))
    (smtp! (s-v 'stream) "AUTH LOGIN")
    (let ((code (smtp? (s-v 'stream))))
      (if (> code 400)
	  (error "Remote mail server raises condition: ~D" code))
      (smtp! (s-v 'stream)
	     (with-core-stream (s "")
	       (base64! s (with-core-stream (s "")
			    (write-stream s 0)
			    (string! s (s-v 'username))
			    (write-stream s 0)
			    (string! s (s-v 'password))
			    (return-stream s)))
	       (return-stream s)))
      (smtp? (s-v 'stream)))))

(defcommand smtp-mail-from (smtp)
  ((email :host local :initarg :email))
  (:default-initargs :message "MAIL FROM:"))

(defmethod run ((self smtp-mail-from))
  (when (s-v 'email) 
    (smtp! (s-v 'stream) (format nil "~A<~A>" (smtp.message self) (s-v 'email)))
    (smtp? (s-v 'stream))))

(defcommand smtp-rcpt-to (smtp)
  ((email :host local :initarg :email))
  (:default-initargs :message "RCPT TO:"))

(defmethod run ((self smtp-rcpt-to))
  (when (s-v 'email) 
    (smtp! (s-v 'stream) (format nil "~A<~A>" (smtp.message self) (s-v 'email)))
    (smtp? (s-v 'stream))))

;; This is the mail adt.
(defclass envelope ()
  ((from :accessor envelope.from :initarg :from
    :initform (error "from field required for envelope")
    :documentation "envelope from field")
   (display-name :accessor envelope.display-name :initarg :display-name
		 :initform nil
		 :documentation "Display name of the sender. Ex: john doe")
   (to :accessor envelope.to :initarg :to
       :initform (error "to field required for envelope")
       :documentation "Recipient of the mail")
   (cc :accessor envelope.cc :initarg :cc
       :initform nil :documentation "carbon copy goes to")
   (relpy-to :accessor envelope.reply-to :initarg :reply-to :initform nil
	     :documentation "reply to address")
   (subject :accessor envelope.subject :initarg :subject
	    :initform (error "subject field required for envelope")
	    :documentation "mail subject")
   (text :accessor envelope.text :initarg :text :initform ""
	 :documentation "mail body which is a string")
   (date :accessor envelope.date :initarg :date :initform (get-email-date-string)
	 :documentation "creation date of envelope")
   (extra-headers :accessor envelope.extra-headers :initarg :extra-headers :initform nil
		  :documentation "extra headers for this envelope. List of string tuples.")))

(defmethod envelope! ((s core-stream) (e envelope))
  (prog1 s
    (checkpoint-stream s)
    (smtp! s (format nil "Date: ~A" (envelope.date e)))
    (smtp! s (format nil "From: ~@[~A <~]~A~@[>~]" (envelope.display-name e) (envelope.from e) (envelope.display-name e)))
    (smtp! s (format nil "To: ~{ ~a~^,~}" (ensure-list (envelope.to e))))
    (when (envelope.cc e)
      (smtp! s (format nil "Cc: ~{ ~a~^,~}" (ensure-list (envelope.cc e)))))
    (when (envelope.reply-to e)
      (smtp! s (format nil "Reply-To: ~A" (envelope.reply-to e))))
    (smtp! s (format nil "Subject: ~A" (envelope.subject e)))
    (smtp! s (format nil "X-Mailer: ~A" +x-mailer+))
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
    (commit-stream s)))

(defcommand smtp-send (smtp)
  ((envelope :host local :initarg :envelope))
  (:default-initargs :message "DATA"))

(defmethod run ((self smtp-send))
  (with-accessors ((from envelope.from) (to envelope.to)) (s-v 'envelope)
    (smtp-mail-from :email (format nil " <~A>" from) :stream (s-v 'stream))
    (mapcar (lambda (rcpt)
	      (smtp-rcpt-to :email (format nil " <~A>" rcpt) :stream (s-v 'stream)))
	    (ensure-list to))
    (smtp! (s-v 'stream) "DATA")
    (envelope! (s-v 'stream) (s-v 'envelope))
    (smtp! (s-v 'stream) ".")
    (smtp? (s-v 'stream))))

(deftrace smtp
    '(smtp! smtp? smtp-ehlo? smtp smtp-auth-plain smtp-auth-login
      smtp-rcpt-to smtp-mail-from smtp-send))

;; 4.2.2 Reply Codes by Function Groups

;; 500 Syntax error, command unrecognized
;; (This may include errors such as command line too long)

;; 501 Syntax error in parameters or arguments
;; 502 Command not implemented  (see section 4.2.4)

;; 503 Bad sequence of commands
;; 504 Command parameter not implemented

;; 211 System status, or system help reply
;; 214 Help message
;; (Information on how to use the receiver or the meaning of a
;; 	     particular non-standard command; this reply is useful only
;; 	     to the human user)


;; 220 <domain> Service ready
;; 221 <domain> Service closing transmission channel
;; 421 <domain> Service not available, closing transmission channel
;; (This may be a reply to any command if the service knows it
;;       must shut down)


;; 250 Requested mail action okay, completed
;; 251 User not local; will forward to <forward-path>
;; (See section 3.4)

;; 252 Cannot VRFY user, but will accept message and attempt
;; delivery
;; (See section 3.5.3)

;; 450 Requested mail action not taken: mailbox unavailable
;; (e.g., mailbox busy)

;; 550 Requested action not taken: mailbox unavailable
;; (e.g., mailbox not found, no access, or command rejected
;;        for policy reasons)

;; 451 Requested action aborted: error in processing
;; 551 User not local; please try <forward-path>
;; (See section 3.4)

;; 452 Requested action not taken: insufficient system storage
;; 552 Requested mail action aborted: exceeded storage allocation
;; 553 Requested action not taken: mailbox name not allowed
;; (e.g., mailbox syntax incorrect)

;; 354 Start mail input; end with <CRLF>.<CRLF>
;; 554 Transaction failed (Or, in the case of a connection-opening
;; 			    response, "No SP service here")

(defun test-smtp ()
  (let ((s (connect "node2.core.gen.tr" 25))
	(en (make-instance 'envelope
			   :from "evrim@core.gen.tr"
			   :to "aycan@core.gen.tr"
			   :display-name "zebedisplayname"
			   :text "zoooo"
			   :subject "keh keh al sana subject")))
    (list (smtp? *s)
	  (smtp-ehlo :stream s)
	  (smtp-send :envelope en :stream s)
	  (smtp-quit :stream s))))