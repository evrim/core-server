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
;; This file implements client side operations
;;

;; 4.1 SMTP Commands
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

(defmethod run ((self smtp))
  (if (smtp.message self)
      (smtp! (smtp.stream self) (smtp.message self)))
  (or (smtp-ehlo? (smtp.stream self))
      (smtp? (smtp.stream self))))

;; 4.1.1.1  Extended HELLO (EHLO) or HELLO (HELO)
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
   (date :accessor envelope.date :initarg :date
	 :initform (get-universal-time)
	 :documentation "creation date of envelope")
   (message-id :accessor envelope.message-id :initarg :message-id
	       :initform (random-string 32)
	       :documentation "message-id")
   (extra-headers :accessor envelope.extra-headers :initarg :extra-headers :initform nil
		  :documentation "extra headers for this envelope. List of string tuples.")))

(defprint-object (self envelope :identity t :type t)
  (format t "~A->~A:~A" (slot-value self 'from) (slot-value self 'to)
	  (slot-value self 'subject)))

(defparser email? (c (username (make-accumulator))
		     (domain (make-accumulator)))
  (:oom (:type unreserved? c)
	(:collect c username))
  #\@
  (:oom (:type unreserved? c)
	(:collect c domain))
  (:return (cons username domain)))

(defmethod envelope! ((s core-stream) (e envelope))
  (flet ((write-address (name address)
	   (cond
	     (name
	      (let ((s (make-quoted-printable-stream s)))
		(string! s name)
		(close-stream s))
	      (string! s " <")
	      (string! s address)
	      (char! s #\>))
	     (t
	      (string! s (envelope.from e))))))
    (prog1 s
      (checkpoint-stream s)
      (when (envelope.date e)
	(string! s "Date: ")
	(http-date! s (envelope.date e))
	(char! s #\Newline))

      (string! s "From:")
      (write-address (envelope.display-name e) (envelope.from e))
      (char! s #\Newline)
    
      (string! s "To:")
      (string! s (format nil "~{ ~a~^,~}" (ensure-list (envelope.to e))))
      (char! s #\Newline)
    
      (when (envelope.cc e)
	(string! s "Cc:")
	(string! s
		 (format nil "~{ ~a~^,~}" (ensure-list (envelope.cc e))))
	(char! s #\Newline))
    
      (when (envelope.reply-to e)
	(string! s "Reply-To: ")
	(string! s (envelope.reply-to e))
	(char! s #\Newline))
    
      (string! s "Subject:")
      (let ((s (make-quoted-printable-stream s)))
      	(string! s (envelope.subject e))
      	(close-stream s))
      (char! s #\Newline)

      (string! s "Message-Id: <")
      (string! s (envelope.message-id e))
      (char! s #\@)
      (string! s (cdr (email? (make-core-stream (envelope.from e)))))
      (char! s #\>)
      (char! s #\Newline)
      
      (smtp! s (format nil "X-Mailer: ~A" +x-mailer+))
      (let ((hdrs (envelope.extra-headers e)))
	(when (and hdrs (listp hdrs))
	  (dolist (h hdrs)
	    (smtp! s (format nil "~A: ~A" (car h) (cdr h))))))
      (smtp! s "MIME-Version: 1.0")
      (smtp! s "Content-Type: text/html; charset=UTF-8; format=flowed")
      (smtp! s "Content-Transfer-Encoding: 8bit")
      (char! s #\Newline)
      (cond
	((typep (envelope.text e) 'dom-element)
	 (write-stream (make-xml-stream s) (envelope.text e)))
	(t
	 (string! s (envelope.text e))))
      (char! s #\Newline)
      (commit-stream s))))

;; SERVER> (let ((s (make-core-file-output-stream #P"/tmp/foo"))
;; 		 (e (make-instance 'core-server::envelope 
;; 		 		   :to (list "evrim@core.gen.tr"
;;                                           "aycan@core.gen.tr")
;; 				   :from "evrim@core.gen.tr"
;; 				   :subject "[Coretal.net] ĞÜLŞÖÇİIÜĞŞİÇÇ:ÖÖMÖIOIOIğülşölöşğü"
;; 				   :text (<:div "text123")
;; 				   :display-name "ĞÜŞİÇÖI")))
;; 	    (core-server::envelope! s e)
;; 	    (close-stream s)
;; 	    (read-string-from-file #P"/tmp/foo" :external-format :utf-8))

;; "Date: Fri, 22 Aug 2011 22:14:23 GMT
;; From: =?UTF-8?Q?=C4=9E=C3=9C=C5=9E=C4=B0=C3=87=C3=96I?= <evrim@core.gen.tr>
;; To: evrim@core.gen.tr, aycan@core.gen.tr
;; Subject: =?UTF-8?Q?[Coretal.net]=20=C4=9E=C3=9CL=C5=9E=C3=96=C3=87=C4=B0I=C3=9C=C4=9E?=
;;  =?UTF-8?Q?=C5=9E=C4=B0=C3=87=C3=87:=C3=96=C3=96M=C3=96IOIOI=C4=9F=C3=BCl=C5?=
;;  =?UTF-8?Q?=9F=C3=B6l=C3=B6=C5=9F=C4=9F=C3=BC?=
;; X-Mailer: [Core-serveR] (http://labs.core.gen.tr)
;; MIME-Version: 1.0
;; Content-Type: text/html; charset=UTF-8; format=flowed
;; Content-Transfer-Encoding: 8bit

;; <div>text123</div>
;; "

(defcommand smtp-send (smtp)
  ((envelope :host local :initarg :envelope))
  (:default-initargs :message "DATA"))

(defmethod run ((self smtp-send))
  (with-accessors ((from envelope.from) (to envelope.to)) (s-v 'envelope)
    (smtp-mail-from :email from :stream (s-v 'stream))
    (mapcar (lambda (rcpt)
	      (smtp-rcpt-to :email rcpt :stream (s-v 'stream)))
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
  (let ((s (connect "localhost" 25))
	(en (make-instance 'envelope
			   :from "aycan@core.gen.tr"
			   :to "evrim@core.gen.tr"
			   :display-name "zebedisplayname"
			   :text "zoooo"
			   :subject "keh keh al sana subject")))
    (list (smtp? s)
	  (smtp-ehlo :stream s)
	  (smtp-send :envelope en :stream s)
	  (smtp-quit :stream s))))