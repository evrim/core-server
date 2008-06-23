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
;;| RFC 3501 IMAP Protocol
;;+----------------------------------------------------------------------------
;;
;; This file implement only client side operations
;;

(defparser imap-newline? ()
  #\Return #\Newline (:return t))

(defatom imap-valid-char? ()
  (or (space? c) (visible-char? c)))

(defparser imap-eat-until-newline? ()
  (:zom (:type imap-valid-char?))
  (:return t))

(defparser imap-greeting? (c (acc (make-accumulator)))
  (:lwsp?)
  #\*
  (:zom (:not (:imap-newline?))
	(:type imap-valid-char? c)
	(:collect c acc))
  (:return acc))

(defrender imap! (cont message)
  (:string! cont)
  (:char! #\Space)
  (:string! message)
  (:char! #\Newline))

(defparser imap-status-line? (cont status)
  (:string? cont)
  (:lwsp?)
  (:or (:and (:seq "OK") (:do (setf status 'OK)))
       (:and (:seq "BAD") (:do (setf status 'BAD))))
  (:zom (:not (:imap-newline?))
	(:type octet?))
  (:return (values cont status)))

(defparser imap? (cont status)
  (:imap-status-line? cont status)
  (:return (list cont status)))

(defcommand imap ()
  ((stream :host local :accessor imap.stream :initarg :stream
	   :initform (error "Please specify stream"))
   (message :host local :accessor imap.message :initarg :message 
	    :initform (error "Please specify message"))
   (continuation :host local :accessor imap.continuation
		 :initarg :continuation
		 :initform (random-string 4))
   (parser :accessor imap.parser
	   :initform #'imap?
	   :initarg :parser)))

(defmethod render ((self imap))
  (imap! (imap.stream self) (imap.continuation self) (imap.message self)))

(defmethod parser ((self imap))
  (funcall (imap.parser self) (imap.stream self)))

(defmethod validate-continuation ((self imap) cont)
  (if (equal cont (imap.continuation self))
      t
      (error "Got response for other cont, current:~A, got:~A"
	     (imap.continuation self) cont)))

(defmethod validate-result ((self imap) result)
  (if (eq result 'OK)
      t
      (error "Got BAD response for ~A" self)))

(defmethod run ((self imap))  
  (render self)
  (let ((result (parser self)))
    (validate-continuation self (car result))
    (validate-result self (cadr result))
    (cddr result)))

;; 6.1.    Client Commands - Any State
;; 6.1.1.  CAPABILITY Command

;;    Arguments:  none
;;    Responses:  REQUIRED untagged response: CAPABILITY
;;    Result:     OK - capability completed
;;                BAD - command unknown or arguments invalid
(defparser imap-capability? (c (acc (make-accumulator))
			       params cont status)
  (:seq "* CAPABILITY ")
  (:zom (:zom (:type visible-char? c)
	      (:collect c acc))
	(:do (push acc params)
	     (setf acc (make-accumulator)))
	(:type space?))
  (:lwsp?)
  (:imap-status-line? cont status)
  (:return (cons cont (cons status (nreverse params)))))

(defcommand imap-capability (imap)
  ()
  (:default-initargs :message "CAPABILITY"
    :parser #'imap-capability?))

;; 6.1.2.  NOOP Command

;;    Arguments:  none
;;    Responses:  no specific responses for this command (but see below)
;;    Result:     OK - noop completed
;;                BAD - command unknown or arguments invalid
(defparser imap-noop? (cont status num value params)
  (:zom #\*
	(:fixnum? num)
	(:string? value)
	(:do (push (cons num value) params)))
  (:imap-status-line? cont status)
  (:return (cons cont (cons status params))))

(defcommand imap-noop (imap)
  ()
  (:default-initargs :message "NOOP" :parser #'imap-noop?))

;; 6.1.3.  LOGOUT Command

;;    Arguments:  none
;;    Responses:  REQUIRED untagged response: BYE
;;    Result:     OK - logout completed
;;                BAD - command unknown or arguments invalid
(defparser imap-logout? (cont status)
  #\* (:lwsp?) (:seq "BYE")
  (:zom (:not #\Newline)
	(:type octet?))
  (:imap-status-line? cont status)
  (:return (cons cont (cons status t))))

(defcommand imap-logout (imap)
  ()
  (:default-initargs :message "LOGOUT" :parser #'imap-logout?))

(defmethod run ((self imap-logout))
  (prog1 (call-next-method)
    (close-stream (imap.stream self))))

;; 6.2.    Client Commands - Not Authenticated State

;; 6.2.1.  STARTTLS Command

;;    Arguments:  none
;;    Responses:  no specific response for this command
;;    Result:     OK - starttls completed, begin TLS negotiation
;;                BAD - command unknown or arguments invalid
(defcommand imap-starttls (imap)
  ()
  (:default-initargs :message "STARTTLS" :parser #'imap?))

;; 6.2.2.  AUTHENTICATE Command

;;    Arguments:  authentication mechanism name
;;    Responses:  continuation data can be requested
;;    Result:     OK - authenticate completed, now in authenticated state
;;                NO - authenticate failure: unsupported authentication
;;                     mechanism, credentials rejected
;;                BAD - command unknown or arguments invalid,
;;                     authentication exchange cancelled
(defcommand imap-authenticate (imap)
  ((mechanism :accessor imap.mechanism :initarg :mechanism
	      :initform "LOGIN"))
  (:default-initargs :message "AUTHENTICATE"))

;; 6.2.3.  LOGIN Command

;;    Arguments:  user name
;;                password
;;    Responses:  no specific responses for this command
;;    Result:     OK - login completed, now in authenticated state
;;                NO - login failure: user name or password rejected
;;                BAD - command unknown or arguments invalid
(defcommand imap-login (imap)
  ((username :host local :initarg :username :accessor imap.username
	     :initform (error "Please specify username"))
   (password :host local :initarg :password :accessor imap.password
	     :initform (error "Please specify password")))
  (:default-initargs :message "LOGIN"))

(defmethod run ((self imap-login))
  (prog1 t
    (setf (imap.message self)
	  (format nil "~A ~A ~A" (imap.message self)
		  (imap.username self) (imap.password self)))
    (call-next-method)))

;; 6.3.    Client Commands - Authenticated State
(defcommand imap-mailbox-command (imap)
  ((mailbox :accessor imap.mailbox :initform (error "Please specify mailbox")
	    :host local)))

(defmethod render ((self imap-mailbox-command))
  (imap! (imap.stream self) (imap.continuation self)
	 (format nil "~A ~A" (imap.message self) (imap.mailbox self))))

;; 6.3.1.  SELECT Command

;;    Arguments:  mailbox name
;;    Responses:  REQUIRED untagged responses: FLAGS, EXISTS, RECENT
;;                REQUIRED OK untagged responses:  UNSEEN,  PERMANENTFLAGS,
;;                UIDNEXT, UIDVALIDITY
;;    Result:     OK - select completed, now in selected state
;;                NO - select failure, now in authenticated state: no
;;                     such mailbox, can't access mailbox
;;                BAD - command unknown or arguments invalid
(defatom imap-flag? ()
  (and (not (eq c #.(char-code #\)))) (visible-char? c)))

(defparser imap-select-flags? (flags c (acc (make-accumulator)))
  #\(
  (:zom (:not #\))
	(:or (:and (:seq "\\Seen") (:do (push 'seen flags)))
	     (:and (:seq "\\Answered") (:do (push 'answered flags)))
	     (:and (:seq "\\Flagged") (:do (push 'flagged flags)))
	     (:and (:seq "\\Deleted") (:do (push 'deleted flags)))
	     (:and (:seq "\\Draft") (:do (push 'draft flags)))
	     (:and (:seq "\\Recent") (:do (push 'recent flags)))
	     (:and (:seq "\\Noselect") (:do (push 'noselect flags)))
	     (:and (:seq "\\*") (:do (push '* flags)))
	     (:and (:zom (:type imap-flag? c)
			 (:collect c acc))
		   (:do (push acc flags)
			(setf acc (make-accumulator)))))
	(:lwsp?))
  (:return (if (null flags)
	       'non-existent-imap-flag
	       (nreverse flags))))

(defparser imap-select? (cont status flags exists recent unseen
			      uidvalidity uidnext permanent-flags)
  (:zom #\* (:lwsp?)
	(:or (:and (:seq "FLAGS") (:lwsp?)
		   (:imap-select-flags? flags)
		   (:if (eq flags 'non-existent-imap-flag)
			(:do (setf flags nil))))
	     (:checkpoint (:fixnum? exists) (:lwsp?) (:seq "EXISTS") (:commit))
	     (:checkpoint (:fixnum? recent) (:lwsp?) (:seq "RECENT") (:commit))
	     (:and (:seq "OK") (:lwsp?)
		   (:or (:and (:seq "[UNSEEN ")
			      (:fixnum? unseen)
			      (:imap-eat-until-newline?))
			(:and (:seq "[UIDVALIDITY ")
			      (:fixnum? uidvalidity)
			      (:imap-eat-until-newline?))
			(:and (:seq "[UIDNEXT ")
			      (:fixnum? uidnext)
			      (:imap-eat-until-newline?))
			(:and (:seq "[PERMANENTFLAGS ")
			      (:imap-select-flags? permanent-flags)
			      (:if (eq permanent-flags 'non-existent-imap-flag)
				   (:do (setf permanent-flags nil)))
			      (:imap-eat-until-newline?)))))
	(:lwsp?))
  (:imap-status-line? cont status)
  (:return (cons cont
		 (cons status (list (cons 'exists exists)
				    (cons 'recent recent)
				    (cons 'unseen unseen)
				    (cons 'uidvalidity uidvalidity)
				    (cons 'uidnext uidnext)
				    (cons 'permanent-flags permanent-flags))))))

(defcommand imap-select (imap-mailbox-command)
  ()
  (:default-initargs :message "SELECT" :parser #'imap-select?))

;; 6.3.2.  EXAMINE Command

;;    Arguments:  mailbox name
;;    Responses:  REQUIRED untagged responses: FLAGS, EXISTS, RECENT
;;                REQUIRED OK untagged responses:  UNSEEN,  PERMANENTFLAGS,
;;                UIDNEXT, UIDVALIDITY
;;    Result:     OK - examine completed, now in selected state
;;                NO - examine failure, now in authenticated state: no
;;                     such mailbox, can't access mailbox
;;                BAD - command unknown or arguments invalid
(defcommand imap-examine (imap-select)
  ()
  (:default-initargs :message "EXAMINE"))

;; 6.3.3.  CREATE Command

;;    Arguments:  mailbox name
;;    Responses:  no specific responses for this command
;;    Result:     OK - create completed
;;                NO - create failure: can't create mailbox with that name
;;                BAD - command unknown or arguments invalid
(defcommand imap-create (imap-mailbox-command)
  ()
  (:default-initargs :message "CREATE"))

;; 6.3.4.  DELETE Command

;;    Arguments:  mailbox name
;;    Responses:  no specific responses for this command
;;    Result:     OK - delete completed
;;                NO - delete failure: can't delete mailbox with that name
;;                BAD - command unknown or arguments invalid
(defcommand imap-delete (imap-mailbox-command)
  ()
  (:default-initargs :message "DELETE"))

;; 6.3.5.  RENAME Command

;;    Arguments:  existing mailbox name
;;                new mailbox name
;;    Responses:  no specific responses for this command
;;    Result:     OK - rename completed
;;                NO - rename failure: can't rename mailbox with that name,
;;                     can't rename to mailbox with that name
;;                BAD - command unknown or arguments invalid
(defcommand imap-rename (imap-mailbox-command)
  ((new-mailbox :accessor imap.new-mailbox
		:initform (error "Please specify new mailbox")
		:initarg :new-mailbox
		:host local))
  (:default-initargs :message "RENAME"))

(defmethod render ((self imap-rename))
  (imap! (imap.stream self) (imap.continuation self)
	 (format nil "~A ~A ~A" (imap.message self)
		 (imap.mailbox self) (imap.new-mailbox self))))

;; 6.3.6.  SUBSCRIBE Command

;;    Arguments:  mailbox
;;    Responses:  no specific responses for this command
;;    Result:     OK - subscribe completed
;;                NO - subscribe failure: can't subscribe to that name
;;                BAD - command unknown or arguments invalid
(defcommand imap-subscribe (imap-mailbox-command)
  ()
  (:default-initargs :message "SUBSCRIBE"))

;; 6.3.7.  UNSUBSCRIBE Command

;;    Arguments:  mailbox name
;;    Responses:  no specific responses for this command
;;    Result:     OK - unsubscribe completed
;;                NO - unsubscribe failure: can't unsubscribe that name
;;                BAD - command unknown or arguments invalid
(defcommand imap-unsubscribe (imap-mailbox-command)
  ()
  (:default-initargs :message "UNSUBSCRIBE"))

;; 6.3.8.  LIST Command

;;    Arguments:  reference name
;;                mailbox name with possible wildcards
;;    Responses:  untagged responses: LIST
;;    Result:     OK - list completed
;;                NO - list failure: can't list that reference or name
;;                BAD - command unknown or arguments invalid
(defparser imap-list? (cont status flags c reference mailbox result)
  (:zom #\* (:lwsp?) (:or (:seq "LSUB") (:seq "LIST"))
	(:lwsp?) (:imap-select-flags? flags) (:lwsp?)
	(:or (:quoted? reference)
	     (:and (:do (setf reference (make-accumulator)))
		   (:zom (:type visible-char? c)
			 (:collect c reference))))
	(:or (:quoted? mailbox)
	     (:and (:do (setf mailbox (make-accumulator)))
		   (:zom (:type visible-char? c)
			 (:collect c mailbox))))
	(:do (push (list (if (eq 'non-existent-imap-flag flags)
			     nil
			     flags) reference mailbox) result)))
  (:lwsp?)
  (:imap-status-line? cont status)
  (:return (cons cont (cons status (nreverse result)))))

(defcommand imap-list (imap-mailbox-command)
  ((reference :accessor imap.reference :initarg :reference
	      :initform "" :host local))
  (:default-initargs :mailbox "" :message "LIST" :parser #'imap-list?))

(defrender imap-list! (cont message reference mailbox)
  (:string! cont)
  (:char! #\Space)
  (:string! message)
  (:char! #\Space)
  (:quoted! reference)
  (:char! #\Space)
  (:quoted! mailbox)
  (:char! #\Newline))

(defmethod render ((self imap-list))
  (imap-list! (imap.stream self) (imap.continuation self)
	      (imap.message self) (imap.reference self) (imap.mailbox self)))

;; 6.3.9.  LSUB Command

;;    Arguments:  reference name
;;                mailbox name with possible wildcards
;;    Responses:  untagged responses: LSUB
;;    Result:     OK - lsub completed
;;                NO - lsub failure: can't list that reference or name
;;                BAD - command unknown or arguments invalid
(defcommand imap-lsub (imap-list)
  ()
  (:default-initargs :message "LSUB"))

;; 6.3.10. STATUS Command

;;    Arguments:  mailbox name
;;                status data item names
;;    Responses:  untagged responses: STATUS
;;    Result:     OK - status completed
;;                NO - status failure: no status for that name
;;                BAD - command unknown or arguments invalid
(defparser imap-status? (c mbox
			   messages uidnext recent uidvalidity unseen
			   cont status)
  #\* (:lwsp?) (:seq "STATUS") (:lwsp?)
  (:or (:quoted? mbox)
       (:and (:do (setf mbox (make-accumulator)))
	     (:zom (:type visible-char? c) (:collect c mbox))))
  (:lwsp?)
  #\(
  (:zom (:or (:and (:seq "MESSAGES") (:lwsp?) (:fixnum? messages))
	     (:and (:seq "UIDNEXT") (:lwsp?) (:fixnum? uidnext))
	     (:and (:seq "RECENT") (:lwsp?) (:fixnum? recent))
	     (:and (:seq "UIDVALIDITY") (:lwsp?) (:fixnum? uidvalidity))
	     (:and (:seq "UNSEEN") (:lwsp?) (:fixnum? unseen)))
	(:lwsp?))
  #\) (:lwsp?)
  (:imap-status-line? cont status)
  (:return (cons cont
		 (cons status
		       (list (cons 'messages messages)
			     (cons 'uidnext uidnext)
			     (cons 'recent recent)
			     (cons 'uidvalidity uidvalidity)
			     (cons 'unseen unseen))))))

(defcommand imap-status (imap-mailbox-command)
  ((messages :initform t :initarg :messages :host local :accessor imap.messages)
   (recent :initform t :initarg :recent :host local :accessor imap.recent)
   (uidnext :initform t :initarg :uidnext :host local :accessor imap.uidnext)
   (uidvalidity :initform t :initarg :uidvalidity :host local :accessor imap.uidvalidity)
   (unseen :initform t :initarg :unseen :host local :accessor imap.unseen))
  (:default-initargs :message "STATUS" :parser #'imap-status?))

(defmethod render ((self imap-status))
  (let ((data (reduce (lambda (acc atom)
			(if (slot-value self atom) (cons atom acc) acc))
		      '(messages recent uidnext uidvalidity unseen) :initial-value nil)))
    (imap! (imap.stream self) (imap.continuation self)
	   (format nil "~A ~A ~(~A~)"
		   (imap.message self) (imap.mailbox self) data))))

;; 6.3.11. APPEND Command

;;    Arguments:  mailbox name
;;                OPTIONAL flag parenthesized list
;;                OPTIONAL date/time string
;;                message literal
;;    Responses:  no specific responses for this command
;;    Result:     OK - append completed
;;                NO - append error: can't append to that mailbox, error
;;                     in flags or date/time or message text
;;                BAD - command unknown or arguments invalid
;; TODO: Fixme
(defcommand imap-append (imap-mailbox-command)
  ()
  (:default-initargs :message "APPEND"))

;; 6.4.    Client Commands - Selected State

;; 6.4.1.  CHECK Command

;;    Arguments:  none
;;    Responses:  no specific responses for this command
;;    Result:     OK - check completed
;;                BAD - command unknown or arguments invalid
(defcommand imap-check (imap)
  ()
  (:default-initargs :message "CHECK"))

;; 6.4.2.  CLOSE Command

;;    Arguments:  none
;;    Responses:  no specific responses for this command
;;    Result:     OK - close completed, now in authenticated state
;;                BAD - command unknown or arguments invalid
(defcommand imap-close (imap)
  ()
  (:default-initargs :message "CLOSE"))

;; 6.4.3.  EXPUNGE Command

;;    Arguments:  none
;;    Responses:  untagged responses: EXPUNGE
;;    Result:     OK - expunge completed
;;                NO - expunge failure: can't expunge (e.g., permission
;;                     denied)
;;                BAD - command unknown or arguments invalid
(defparser imap-expunge? (cont status id messages)
  (:zom #\* (:lwsp?)
	(:fixnum? id)
	(:do (push id messages))
	(:seq "EXPUNGE")
	(:lwsp?))
  (:imap-status-line? cont status)
  (:return (cons cont
		 (cons status (nreverse messages)))))

(defcommand imap-expunge (imap)
  ()
  (:default-initargs :message "EXPUNGE" :parser #'imap-expunge?))

;; 6.4.4.  SEARCH Command

;;    Arguments:  OPTIONAL [CHARSET] specification
;;                searching criteria (one or more)
;;    Responses:  REQUIRED untagged response: SEARCH
;;    Result:     OK - search completed
;;                NO - search error: can't search that [CHARSET] or
;;                     criteria
;;                BAD - command unknown or arguments invalid
(defparser imap-search? (num result cont status)
  #\* (:lwsp?) (:seq "SEARCH") (:lwsp?)
  (:zom (:fixnum? num)
	(:do (push num result))
	(:type space?))
  (:lwsp?)
  (:imap-status-line? cont status)
  (:return (cons cont (cons status (nreverse result)))))

(defcommand imap-search (imap)
  ((charset :accessor imap.charset :initarg :charset :host local
	    :initform nil)
   (query :accessor imap.query :initarg :query :host local
	  :initform (error "Please specify query")))
  (:default-initargs :message "SEARCH" :parser #'imap-search?))

(defmethod render ((self imap-search))
  (imap! (imap.stream self) (imap.continuation self)
	 (format nil "~A ~A" (imap.message self)
		 (if (imap.charset self)
		     (format nil "~A ~A" (imap.charset self) (imap.query self))
		     (imap.query self)))))

;; 6.4.5.  FETCH Command

;;    Arguments:  sequence set
;;                message data item names or macro
;;    Responses:  untagged responses: FETCH
;;    Result:     OK - fetch completed
;;                NO - fetch error: can't fetch that data
;;                BAD - command unknown or arguments invalid

;; TODO: Handle BODY replies, parse result
;; "(FLAGS (\\Seen) INTERNALDATE \"20-Jun-2008 14:06:51 +0300\" RFC822.SIZE 7143 ENVELOPE (\"Fri, 20 Jun 2008 13:47:02 +0300\" \"=?windows-1254?Q?ileri_Excel_ve_=DDnternet_Pazarlama?=\" ((\"Bilisim Seminer\" NIL \"news\" \"zeru-news.com\")) ((\"Bilisim Seminer\" NIL \"news\" \"zeru-news.com\")) ((NIL NIL \"news\" \"zerumail.com\")) ((NIL NIL \"evrim\" \"core.gen.tr\")) NIL NIL NIL \"<d258cd436b112a116f6b88e50012cbd1@zeru-news.com>\"))"
(defparser imap-fetch? (id result c (acc (make-accumulator))
			   cont status)
  (:zom #\* (:lwsp?)
	(:fixnum? id) (:lwsp?) (:seq "FETCH") (:lwsp?)
	(:zom (:type (or space? visible-char?) c)
	      (:collect c acc))
	(:do (push (cons id acc) result))
	(:lwsp?))
  (:imap-status-line? cont status)
  (:return (cons cont (cons status (nreverse result)))))

(defcommand imap-fetch (imap)
  ((sequence :initarg :sequence :accessor imap.sequence :host local
	     :initform (error "Please specify sequence"))
   (macro :initarg :macro :accessor imap.macro :host local
	  :initform (error "Please specify data item names or macro")))
  (:default-initargs :message "FETCH" :parser #'imap-fetch?))

(defmethod render ((self imap-fetch))
  (imap! (imap.stream self) (imap.continuation self)
	 (format nil "~A ~A ~A"
		 (imap.message self)
		 (if (consp (imap.sequence self))
		     (format nil "~D:~D"
			     (car (imap.sequence self))
			     (cdr (imap.sequence self)))
		     (imap.sequence self))
		 (imap.macro self))))

;; 6.4.6.  STORE Command

;;    Arguments:  sequence set
;;                message data item name
;;                value for message data item
;;    Responses:  untagged responses: FETCH
;;    Result:     OK - store completed
;;                NO - store error: can't store that data
;;                BAD - command unknown or arguments invalid
;; TODO: get flags as symbols
(defparameter +imap-store-commands+
  '((set "FLAGS")
    (silent-set "FLAGS.SILENT")
    (append "+FLAGS")
    (silent-append "+FLAGS.SILENT")
    (remove "-FLAGS")
    (silent-remove "-FLAGS.SILENT")))

(defparser imap-store? (id flags result cont status)
  (:zom #\* (:lwsp?)
	(:fixnum? id) (:lwsp?)
	(:seq "FETCH") (:lwsp?)
	(:seq "(FLAGS ")
	(:imap-select-flags? flags)
	(:do (push (cons id flags) result))
	#\)
	(:lwsp?))
  (:imap-status-line? cont status)
  (:return (cons cont (cons status (nreverse result)))))

(defcommand imap-store (imap)
  ((sequence :host local :initform (error "Please specify :sequence")
	     :accessor imap.sequence :initarg :sequence)
   (command :host local :initform (error "Please specify :comand")
	    :accessor imap.command :initarg :command)
   (flags :host local :initform (error "Please specify :flags")
	  :accessor imap.flags :initarg :flags))
  (:default-initargs :message "STORE" :parser #'imap-store?))

(defrender imap-store! (cont message sequence command flags)
  (:string! cont)
  (:char! #\Space)
  (:string! message)
  (:char! #\Space)
  (:string! sequence)
  (:char! #\Space)
  (:string! command)
  (:char! #\Space)
  (:char! #\()
  (:sep " " flags)
  (:char! #\))
  (:char! #\Newline))

(defmethod render ((self imap-store))
  (assert (member (imap.command self) +imap-store-commands+ :key #'car))
  (imap-store! (imap.stream self) (imap.continuation self)
	       (imap.message self) (if (consp (imap.sequence self))
				       (format nil "~D:~D"
					       (car (imap.sequence self))
					       (cdr (imap.sequence self)))
				       (format nil "~D" (imap.sequence self)))
	       (cadr (assoc (imap.command self) +imap-store-commands+))
	       (imap.flags self)))

;; 6.4.7.  COPY Command

;;    Arguments:  sequence set
;;                mailbox name
;;    Responses:  no specific responses for this command
;;    Result:     OK - copy completed
;;                NO - copy error: can't copy those messages or to that
;;                     name
;;                BAD - command unknown or arguments invalid
(defcommand imap-copy (imap)
  ((sequence :accessor imap.sequence :host local :initarg :sequence
	     :initform (error "Please specify :sequence"))
   (mailbox :accessor imap.mailbox :host local :initarg :mailbox
	    :initform (error "Please specify :mailbox")))
  (:default-initargs :message "COPY"))

(defmethod render ((self imap-copy))
  (imap! (imap.stream self) (imap.continuation self)
	 (format nil "~A ~A ~A"
		 (imap.message self)
		 (if (consp (imap.sequence self))
		     (format nil "~D:~D"
			     (car (imap.sequence self))
			     (cdr (imap.sequence self)))
		     (imap.sequence self))
		 (imap.mailbox self))))

;; 6.4.8.  UID Command

;;    Arguments:  command name
;;                command arguments
;;    Responses:  untagged responses: FETCH, SEARCH
;;    Result:     OK - UID command completed
;;                NO - UID command error
;;                BAD - command unknown or arguments invalid



(defun test-imap ()
  (let ((s (make-core-stream
	    (cl+ssl::make-ssl-client-stream 
	     (slot-value (connect "imap.core.gen.tr" 993) '%stream)))))
    (list (imap-greeting? s)
	  (imap-capability :stream s)
	  (imap-noop :stream s)
;;	  (imap-starttls :stream s)
	  (imap-login :stream s :username "evrim.ulu" :password "d0m1n0z")
	  (imap-select :stream s :mailbox "INBOX")
	  (imap-examine :stream s :mailbox "INBOX")
	  (let ((folder (random-string 5))
		(folder2 (random-string 5)))
	    (imap-create :stream s :mailbox folder)
	    (imap-rename :stream s :mailbox folder :new-mailbox folder2)
	    (imap-select :stream s :mailbox folder2)
	    (imap-delete :stream s :mailbox folder2))
	  (imap-select :stream s :mailbox "INBOX")
	  (imap-status :stream s :mailbox "INBOX")
	  (imap-list :stream s)
	  (imap-lsub :stream s)
	  (imap-check :stream s)
	  (imap-search :stream s :query "FROM \"evrim\"")
	  (imap-fetch :stream s :sequence 2457 :macro "ALL")
	  (imap-store :stream s :sequence 2457 :command 'set :flags '("\\Seen"))
	  (imap-copy :stream s :sequence 2457 :mailbox "Sent")
	  (imap-expunge :stream s)
	  (imap-logout :stream s))))

;; openssl s_client -host mail.core.gen.tr -port 993