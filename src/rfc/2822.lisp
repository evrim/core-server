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

;;;;
;;;; THIS IS HARD TO IMPLEMENT, SO PLEASE BE KIND!
;;;;

;; TODO: How to optimize? well you should first get a category of
;; messages. For each parser, you'll extract the subcategory and make
;; a frequency analysis. Then you'll change your OR compositions so
;; that they'll match the most frequent occurences first. -aycan

;; Utils

; (cons 'leftval 'rightval)
(defun fork (x l r)
  (cond
    ((left? x)
     (funcall l (car x)))
    ((right? x)
     (funcall r (cdr x)))
    (t (error 'notafork))))

(defun right? (x)
  (null (car x)))

(defun left? (x)
  (null (cdr x)))

(defun right (x)
  (cons nil x))

(defun left (x)
  (cons x nil))

;; 3.2.1. Primitive Tokens
;; NO-WS-CTL       =       %d1-8 /         ; US-ASCII control characters
;;                         %d11 /          ;  that do not include the
;;                         %d12 /          ;  carriage return, line feed,
;;                         %d14-31 /       ;  and white space characters
;;                         %d127

;; text            =       %d1-9 /         ; Characters excluding CR and LF
;;                         %d11 /
;;                         %d12 /
;;                         %d14-127 /
;;                         obs-text

;; specials        =       "(" / ")" /     ; Special characters used in
;;                         "<" / ">" /     ;  other parts of the syntax
;;                         "[" / "]" /
;;                         ":" / ";" /
;;                         "@" / "\" /
;;                         "," / "." /
;;                         DQUOTE

(defparser no-ws-ctl? (c)
  (:satisfy #'(lambda (x)
		(and (or (and (> x 0) (< x 32))
			 (= x 127))
		     (not (or (= x 9) (= x 10) (= x 13)))))
	    c)
  (:return c))

;; (eq (text? (make-core-stream "asd")) 97)
(defparser text? (c)
  (:or (:satisfy #'(lambda (x) (and (not (or (= x 10) (= x 13))) (< x 128))) c)
       (:obs-text? c))
  (:return c))

;; (specials? (char-code #\())

(defparser specials? (c)
  (:oneof "()<>[]:;@,.\\\"" c)
  (:return c))

;; 3.2.2. Quoted characters
;; quoted-pair     =       ("\" text) / obs-qp

;; (eq (quoted-pair? (make-core-stream "\\w")) 119)
(defparser quoted-pair? (c)
  (:or (:and #\\
	 (:text? c))
       (:obs-qp? c)) 
  (:return c))

;; 3.2.3. Folding white space and comments
;; FWS             =  ([*WSP CRLF] 1*WSP) /   ; Folding white space
;;                    obs-FWS
;; ctext           =  NO-WS-CTL /     ; Non white space controls
;;                    %d33-39 /       ; The rest of the US-ASCII
;;                    %d42-91 /       ;  characters not including "(",
;;                    %d93-126        ;  ")", or "\"

;; (fws? (make-core-stream (format nil "   ~% ")))
(defparser fws? ()
  (:or (:and (:optional
	      (:zom (:type white-space?))
	      (:crlf?))
	     (:oom (:type white-space?)))
       (:obs-fws?))
  (:return t))

;; ctext           =  <any CHAR excluding "(",     ; => may be folded
;;                    ")", "\" & CR, & including
;;                    linear-white-space>
(defparser ctext? (c)
  (:or (:no-ws-ctl?)
       (:noneof "()\\" c))
  (:return c))

;; ccontent        =  ctext / quoted-pair / comment
(defparser ccontent? (c text)
  (:or (:ctext? c)
       (:quoted-pair? c)
       (:comment? text))
  (:return (cons c text)))

;; comment         =  "(" *([FWS] ccontent) [FWS] ")"
(defparser comment? (c (acc (make-accumulator)))
  #\(
;;  (:collect #\( acc) ;; do not collect lexer
  (:zom (:optional (:fws?))
	(:ccontent? c)
	(:if (left? c)
	     (:collect (car c) acc)
	     (:do (vector-push-extend #\  acc)
		  (reduce #'(lambda (a item)
			      (declare (ignore a))
			      (vector-push-extend item acc))
			  (cdr c) :initial-value nil))))
  (:optional (:fws?))
  #\)
;;  (:collect #\) acc) ;; do not collect lexer
  (:return acc))

(defun comment! (stream text)
  (char! stream #\()
  (string! stream text)
  (char! stream #\)))

;; CFWS            =  *([FWS] comment) (([FWS] comment) / FWS)
;; TODO: collect comments.
(defparser cfws? ()
  (:oom (:or (:and (:optional (:fws?))
		   (:comment?))
	     (:fws?)))
  (:return t))

;; 3.2.4. Atom
;; atext           =       ALPHA / DIGIT / ; Any character except controls,
;;                         "!" / "#" /     ;  SP, and specials.
;;                         "$" / "%" /     ;  Used for atoms
;;                         "&" / "'" /
;;                         "*" / "+" /
;;                         "-" / "/" /
;;                         "=" / "?" /
;;                         "^" / "_" /
;;                         "`" / "{" /
;;                         "|" / "}" /
;;                         "~"
;; atom            =       [CFWS] 1*atext [CFWS]
;; dot-atom        =       [CFWS] dot-atom-text [CFWS]
;; dot-atom-text   =       1*atext *("." 1*atext)

(defparser atext? (c)
  (:or (:type alphanum? c)
       (:oneof "!#$%&'*+-/=?^_`{|}~" c))
  (:return c))

(defparser atom? (c (acc (make-accumulator)))
  (:optional (:cfws?))
  (:oom (:atext? c)
	(:collect c acc))
  (:optional (:cfws?))
  (:return acc))

(defparser dot-atom? (c)
  (:optional (:cfws?))
  (:dot-atom-text? c)
  (:optional (:cfws?))
  (:return c))

(defparser dot-atom-text? (c (acc (make-accumulator)))
  (:oom (:atext? c)
	(:collect c acc))
  (:zom #\.
	(:collect #\. acc)
	(:oom (:atext? c)
	      (:collect c acc)))
  (:return acc))

;; 3.2.5. Quoted strings
;; qtext           =       NO-WS-CTL /     ; Non white space controls
;;                         %d33 /          ; The rest of the US-ASCII
;;                         %d35-91 /       ;  characters not including "\"
;;                         %d93-126        ;  or the quote character
;; qcontent        =       qtext / quoted-pair
;; quoted-string   =       [CFWS]
;;                         DQUOTE *([FWS] qcontent) [FWS] DQUOTE
;;                         [CFWS]

(defparser qtext? (c)
  (:or (:no-ws-ctl? c)
       (:satisfy #'(lambda (x)
		     (and (or (> x 32) (< x 127))
			  (not (or (= x #.(char-code #\"))
				   (= x #.(char-code #\\))))))
		 c))
  (:return c))

(defparser qcontent? (c)
  (:or (:qtext? c)
       (:quoted-pair? c))
  (:return c))

(defparser quoted-string? (c (acc (make-accumulator)))
  (:optional (:cfws?))
  #\"
  (:zom (:optional (:fws?))
	(:qcontent? c)
	(:collect c acc))
  (:optional (:fws?))
  #\"
  (:optional (:cfws?))
  (:return acc))

;; 3.2.6. Miscellaneous tokens
;; word            =       atom / quoted-string
;; phrase          =       1*word / obs-phrase
;; utext           =       NO-WS-CTL /     ; Non white space controls
;;                         %d33-126 /      ; The rest of US-ASCII
;;                         obs-utext
;; unstructured    =       *([FWS] utext) [FWS]

(defparser word? (c)
  (:or (:atom? c)
       (:quoted-string? c))
  (:return c))

(defparser phrase? (c phrase)
  (:or (:oom (:word? c)
	     (:do (push c phrase)))
       (:obs-phrase? phrase))
  (:return (nreverse phrase)))

(defparser utext? (c)
  (:or (:no-ws-ctl? c)
       (:satisfy #'(lambda (x) (and (> x 32) (< x 127))) c)
       (:obs-utext? c))
  (:return c))

(defparser unstructured? (c (acc (make-accumulator)))
  (:zom (:optional (:fws?))
	(:utext? c)
	(:collect c acc))
  (:optional (:fws?))
  (:return acc))

;; 3.3. Date and Time Specification
;; date-time       =       [ day-of-week "," ] date FWS time [CFWS]
;; day-of-week     =       ([FWS] day-name) / obs-day-of-week
;; day-name        =       "Mon" / "Tue" / "Wed" / "Thu" /
;;                         "Fri" / "Sat" / "Sun"
;; date            =       day month year
;; year            =       4*DIGIT / obs-year
;; month           =       (FWS month-name FWS) / obs-month
;; month-name      =       "Jan" / "Feb" / "Mar" / "Apr" /
;;                         "May" / "Jun" / "Jul" / "Aug" /
;;                         "Sep" / "Oct" / "Nov" / "Dec"
;; day             =       ([FWS] 1*2DIGIT) / obs-day
;; time            =       time-of-day FWS zone
;; time-of-day     =       hour ":" minute [ ":" second ]
;; hour            =       2DIGIT / obs-hour
;; minute          =       2DIGIT / obs-minute
;; second          =       2DIGIT / obs-second
;; zone            =       (( "+" / "-" ) 4DIGIT) / obs-zone

;; helper
(defparser twodigitint? (d1 d2)
  (:type digit? d1)
  (:type digit? d2)
  (:return (unsafe-digits2int (list d1 d2))))

(defparser fourdigitint? (d1 d2 d3 d4)
  (:type digit? d1)
  (:type digit? d2)
  (:type digit? d3)
  (:type digit? d4)
  (:return (unsafe-digits2int (list d1 d2 d3 d4))))

;; (eq (rfc2822-date-time? (make-core-stream "Mon, 21 Sep 1980 10:01:02 +0230")) 2547376262)
(defparser rfc2822-date-time? (dow d time)
  (:optional (:rfc2822-day-of-week? dow) #\,)
  (:rfc2822-date? d)
  (:fws?)
  (:rfc2822-time? time)
  (:optional (:cfws?))
  (:return (apply #'encode-universal-time `(,@(car time) ,@d ,(cdr time)))))

(defparser rfc2822-day-of-week? (dow)
  (:or (:and (:optional (:fws?))
	     (:rfc2822-day-name? dow))
       (:obs-day-of-week? dow))
  (:return dow))

(defparser rfc2822-day-name? ()
  (:or (:and (:seq "Mon") (:return 0))
       (:and (:seq "Tue") (:return 1))
       (:and (:seq "Wed") (:return 2))
       (:and (:seq "Thu") (:return 3))
       (:and (:seq "Fri") (:return 4))
       (:and (:seq "Sat") (:return 5))
       (:and (:seq "Sun") (:return 6))))

(defparser rfc2822-date? (d m y)
  (:rfc2822-day? d)
  (:rfc2822-month? m)
  (:rfc2822-year? y)
  (:return (list d m y)))

;; fold with pair
;; (eq (unsafe-digits2int (mapcar #'char-code '(#\1 #\9 #\9 #\0))) 1990)
(defun unsafe-digits2int (dlist)
  (let ((rlist (reverse (mapcar #'(lambda (x) (- x 48)) dlist))))
    (cdr (reduce #'(lambda (acc item)
		     (let ((pwr (* 10 (car acc))))
		       (cons pwr (+ (cdr acc) (* pwr item))))) 
		 (cdr rlist)
		 :initial-value (cons 1 (car rlist))))))

;; (eq (unsafe-digits2string (list 49 56 54 52)) "1864")
(defun unsafe-digits2string (dlist)
  (format nil "~{~D~}" (mapcar #'code-char dlist)))

(defparser rfc2822-year? (c)
  (:or (:fourdigitint? c)
       (:obs-year? c))
  (:return c))

(defparser rfc2822-month? (m)
  (:or (:and (:fws?)
	     (:rfc2822-month-name? m)
	     (:fws?))
       (:obs-month? m)) 
  (:return m))

(defparser rfc2822-month-name? ()
  (:or (:and (:seq "Jan") (:return 1))
       (:and (:seq "Feb") (:return 2))
       (:and (:seq "Mar") (:return 3))
       (:and (:seq "Apr") (:return 4))
       (:and (:seq "May") (:return 5))
       (:and (:seq "Jun") (:return 6))
       (:and (:seq "Jul") (:return 7))
       (:and (:seq "Aug") (:return 8))
       (:and (:seq "Sep") (:return 9))
       (:and (:seq "Oct") (:return 10))
       (:and (:seq "Nov") (:return 11))
       (:and (:seq "Dec") (:return 12))))

(defparser rfc2822-day? (c)
  (:or (:and (:optional (:fws?))
	     (:twodigitint? c))
       (:obs-day? c))
  (:return c))

(defparser rfc2822-time? (tod z)
  (:rfc2822-time-of-day? tod)
  (:fws?)
  (:rfc2822-zone? z)
  (:return (cons tod z)))

(defparser rfc2822-time-of-day? (h m s)
  (:rfc2822-hour? h)
  #\:
  (:rfc2822-minute? m)
  (:optional #\: (:rfc2822-second? s))
  (:return (list (or s 0) m h)))

;; TODO: put constraint 0 < c < 24
(defparser rfc2822-hour? (c)
  (:or (:twodigitint? c)
       (:obs-hour? c))
  (:return c))

;; TODO: put constraint 0 < c < 60
(defparser rfc2822-minute? (c)
  (:or (:twodigitint? c)
       (:obs-minute? c))
  (:return c))

;; TODO: put constraint 0 < c < 60
(defparser rfc2822-second? (c)
  (:or (:twodigitint? c)
       (:obs-second? c))
  (:return c))

(defparser rfc2822-zone? (h1 h2 m1 m2 sign oz)
  (:or (:and (:or (:and #\+ (:do (setq sign 'succ)))
		  (:and #\- (:do (setq sign 'decc))))
	     (:type digit? h1)
	     (:type digit? h2)
	     (:type digit? m1)
	     (:type digit? m2)
	     (:return (let ((tmp (+ (/ (unsafe-digits2int (list m1 m2)) 60)
				    (unsafe-digits2int (list h1 h2)))))
			(if (eq sign 'decc)
			    (* -1 tmp)
			    tmp))))
       (:and (:obs-zone? oz) (:return oz))))

;; 3.4. Address Specification
;; address         =       mailbox / group
;; mailbox         =       name-addr / addr-spec
;; name-addr       =       [display-name] angle-addr
;; angle-addr      =       [CFWS] "<" addr-spec ">" [CFWS] / obs-angle-addr
;; group           =       display-name ":" [mailbox-list / CFWS] ";"
;;                         [CFWS]
;; display-name    =       phrase
;; mailbox-list    =       (mailbox *("," mailbox)) / obs-mbox-list
;; address-list    =       (address *("," address)) / obs-addr-list

(defclass rfc2822-nameaddr ()
  ((name :accessor rfc2822-nameaddr.name :initarg :name)
   (addr :accessor rfc2822-nameaddr.addr :initarg :addr)))

(defun make-rfc2822-nameaddr (&optional name addr)
  (make-instance 'rfc2822-nameaddr
		 :name name :addr addr))

(defparser address? (c)
  (:or (:mailbox? c)
       (:group? c))
  (:return c))

(defparser mailbox? (m)
  (:or (:name-addr? m)
       (:addr-spec? m))
  (:return m))

(defparser name-addr? (name addr)
  (:optional (:display-name? name))
  (:angle-addr? addr)
  (:return (make-rfc2822-nameaddr name addr)))

(defparser angle-addr? (r)
  (:or (:and (:optional (:cfws?))
	     #\<
	     (:addr-spec? r)
	     #\>
	     (:optional (:cfws?)))
       (:obs-angle-addr? r))
  (:return r))

(defparser group? (dname mls)
  (:display-name? dname)
  #\:
  (:optional (:or (:mailbox-list? mls)
		  (:cfws?)))
  #\;
  (:optional (:cfws?))
  (:return mls))

(defparser display-name? (dn)
  (:phrase? dn)
  (:return dn))

(defparser mailbox-list? (c mboxs)
  (:or (:and (:mailbox? c)
	     (:do (push c mboxs))
	     (:zom #\,
		   (:mailbox? c)
		   (:do (push c mboxs)))
	     (:return (nreverse mboxs)))
       (:and (:obs-mbox-list? c) (:return c))))

(defparser address-list? (c axs)
  (:or (:and (:address? c)
	     (:do (push c axs))
	     (:zom #\,
		   (:address? c)
		   (:do (push c axs)))
	     (:return (nreverse axs)))
       (:and (:obs-addr-list? c) (:return c))))

;; 3.4.1. Addr-spec specification
;; addr-spec       =       local-part "@" domain
;; local-part      =       dot-atom / quoted-string / obs-local-part
;; domain          =       dot-atom / domain-literal / obs-domain
;; domain-literal  =       [CFWS] "[" *([FWS] dcontent) [FWS] "]" [CFWS]
;; dcontent        =       dtext / quoted-pair
;; dtext           =       NO-WS-CTL /     ; Non white space controls
;;                         %d33-90 /       ; The rest of the US-ASCII
;;                         %d94-126        ;  characters not including "[",
;;                                         ;  "]", or "\"

(defparser addr-spec? (l d)
  (:local-part? l)
  #\@
  (:domain? d)
  (:return (concatenate 'string l "@" d)))

(defparser local-part? (lp)
  (:or (:dot-atom? lp)
       (:quoted-string? lp)
       (:obs-local-part? lp))
  (:return lp))

(defparser domain? (d)
  (:or (:dot-atom? d)
       (:domain-literal? d)
       (:obs-domain? d))
  (:return d))

(defparser domain-literal? (dc)
  (:optional (:cfws?))
  #\[
  (:zom (:optional (:fws?))
	(:dcontent? dc)
	(:optional (:fws?)))
  #\]
  (:optional (:cfws?)))

(defparser dcontent? (d)
  (:or (:dtext? d)
       (:quoted-pair? d))
  (:return d))

(defparser dtext? (c)
  (:no-ws-ctl?)
  (:satisfy #'(lambda (x) (or (and (> x 32) (< x 91))
			      (and (> x 93) (< x 127))))
	    c)
  (:return c))

;; 3.5 Overall message syntax
;; message         =       (fields / obs-fields)
;;                         [CRLF body]
;; body            =       *(*998text CRLF) *998text



(defclass rfc2822-message ()
  ((fields :accessor rfc2822-message.fields :initarg :fields)
   (body :accessor rfc2822-message.body :initarg :body)))

(defun make-rfc2822-message (&optional fields body)
  (make-instance 'rfc2822-message :fields fields :body body))

(defparser rfc-2822-message? (fxs body)
  (:or (:rfc2822-fields? fxs)
       (:obs-fields? fxs))
  (:optional
   (:crlf?)
   (:rfc-2822-body? body))
  (:return (make-rfc2822-message fxs body)))

(defparser rfc-2822-body? (txt body (acc (make-accumulator)))
  (:zom (:and (:zom (:998text? txt)
		    (:do (setq body (concatenate 'string body (unless body (format nil "~%")) txt))))
	      (:crlf?)))
  (:zom (:998text txt)
	(:do (setq body (concatenate 'string body (unless body (format nil "~%")) txt))))
  (:return acc))

;; 3.6. Field definitions
;; fields          =       *(trace
;;                           *(resent-date /
;;                            resent-from /
;;                            resent-sender /
;;                            resent-to /
;;                            resent-cc /
;;                            resent-bcc /
;;                            resent-msg-id))
;;                         *(orig-date /
;;                         from /
;;                         sender /
;;                         reply-to /
;;                         to /
;;                         cc /
;;                         bcc /
;;                         message-id /
;;                         in-reply-to /
;;                         references /
;;                         subject /
;;                         comments /
;;                         keywords /
;;                         optional-field)

(defmacro rfc2822-header (name var txt &body body)
  `(defparser ,(intern (format nil "RFC2822-FIELDS.~A?" name)) ,var
     (:seq ,txt)
     ,@body
     (:crlf?)
     (:return (cons ',name ,(car var)))))

(defparser rfc2822-fields? (x xs)
  (:and (:zom (:and
	       (:and (:rfc2822-fields.trace? x) (:do (push x xs)))
	       (:zom (:or (:and (:rfc2822-fields.resent-date? x) (:do (push x xs)))
			  (:and (:rfc2822-fields.resent-from? x) (:do (push x xs)))
			  (:and (:rfc2822-fields.resent-sender? x) (:do (push x xs)))
			  (:and (:rfc2822-fields.resent-to? x) (:do (push x xs)))
			  (:and (:rfc2822-fields.resent-cc? x) (:do (push x xs)))
			  (:and (:rfc2822-fields.resent-bcc? x) (:do (push x xs)))
			  (:and (:rfc2822-fields.resent-msg-id? x) (:do (push x xs)))))))
	(:zom (:or (:and (:rfc2822-fields.orig-date? x) (:do (push x xs)))
		   (:and (:rfc2822-fields.from? x) (:do (push x xs)))
		   (:and (:rfc2822-fields.sender? x) (:do (push x xs)))
		   (:and (:rfc2822-fields.reply-to? x) (:do (push x xs)))
		   (:and (:rfc2822-fields.to? x) (:do (push x xs)))
		   (:and (:rfc2822-fields.cc? x) (:do (push x xs)))
		   (:and (:rfc2822-fields.bcc? x) (:do (push x xs)))
		   (:and (:rfc2822-fields.message-id? x) (:do (push x xs)))
		   (:and (:rfc2822-fields.in-reply-to? x) (:do (push x xs)))
		   (:and (:rfc2822-fields.references? x) (:do (push x xs)))
		   (:and (:rfc2822-fields.subject? x) (:do (push x xs)))
		   (:and (:rfc2822-fields.comments? x) (:do (push x xs)))
		   (:and (:rfc2822-fields.keywords? x) (:do (push x xs)))
		   (:and (:rfc2822-fields.optional-field? x) (:do (push x xs))))))
  (:return xs))

;; 3.6.1. The origination date field
;; orig-date       =       "Date:" date-time CRLF

;; (eq (rfc2822-fields.orig-date? (make-core-stream (format nil "Date: Mon, 21 Sep 1980 10:01:02 +0230~%"))) 2547376262)
(rfc2822-header orig-date (d) "Date:" (:rfc2822-date-time? d))

;; 3.6.2. Originator fields
;; from            =       "From:" mailbox-list CRLF
;; sender          =       "Sender:" mailbox CRLF
;; reply-to        =       "Reply-To:" address-list CRLF

;; (eq (rfc2822-fields.from? (make-core-stream (format nil "From: aycan@core.gen.tr, evrim@core.gen.tr~%")))
;;     '("aycan@core.gen.tr" "evrim@core.gen.tr"))
(rfc2822-header from (xs) "From:" (:mailbox-list? xs))
(rfc2822-header sender (x) "Sender:" (:mailbox? x))
(rfc2822-header reply-to (xs) "Reply-To:" (:address-list? xs))

;; 3.6.3. Destination address fields
;; to              =       "To:" address-list CRLF
;; cc              =       "Cc:" address-list CRLF
;; bcc             =       "Bcc:" (address-list / [CFWS]) CRLF

(rfc2822-header to (xs) "To:" (:address-list? xs))
(rfc2822-header cc (xs) "Cc:" (:address-list? xs))
(rfc2822-header bcc (xs) "Bcc:" (:or (:address-list? xs) (:optional (:cfws?))))

;; 3.6.4. Identification fields
;; message-id      =       "Message-ID:" msg-id CRLF
;; in-reply-to     =       "In-Reply-To:" 1*msg-id CRLF
;; references      =       "References:" 1*msg-id CRLF
;; msg-id          =       [CFWS] "<" id-left "@" id-right ">" [CFWS]
;; id-left         =       dot-atom-text / no-fold-quote / obs-id-left
;; id-right        =       dot-atom-text / no-fold-literal / obs-id-right
;; no-fold-quote   =       DQUOTE *(qtext / quoted-pair) DQUOTE
;; no-fold-literal =       "[" *(dtext / quoted-pair) "]"

(rfc2822-header message-id (x) "Message-ID:" (:rfc2822-msg-id? x))

(DEFPARSER RFC2822-FIELDS.IN-REPLY-TO? (XS C)
  (:SEQ "In-Reply-To:")
  (:OOM (:AND (:RFC2822-MSG-ID? C) (:DO (PUSH C XS))))
  (:CRLF?)
  (:RETURN (list 'IN-REPLY-TO (NREVERSE XS))))

;; (rfc2822-header references (xs c) "References:" (:oom (:and (:rfc2822-msg-id? c) (:do (push c xs)))))

(DEFPARSER RFC2822-FIELDS.REFERENCES? (XS C)
  (:SEQ "References:")
  (:OOM (:AND (:RFC2822-MSG-ID? C) (:DO (PUSH C XS))))
  (:CRLF?)
  (:RETURN (list 'REFERENCES (NREVERSE XS))))

(defparser rfc2822-msg-id? (l r)
  (:optional (:cfws?))
  #\<
  (:rfc2822-id-left? l)
  #\@
  (:rfc2822-id-right? r)
  #\>
  (:optional (:cfws?))
  (:return (concatenate 'string "<" l "@" r ">")))

(defparser rfc2822-id-left? (x)
  (:or (:dot-atom-text? x)
       (:rfc2822-no-fold-literal? x)
       (:obs-id-left? x))
  (:return x))

(defparser rfc2822-id-right? (x)
  (:or (:dot-atom-text? x)
       (:rfc2822-no-fold-literal? x)
       (:obs-id-right? x))
  (:return x))

(defparser rfc2822-no-fold-quote? (x (acc (make-accumulator)))
  #\"
  (:zom (:or (:and (:qtext? x) (:collect x acc))
	     (:and (:quoted-pair? x) (:collect x acc))))
  #\"
  (:return acc))

(defparser rfc2822-no-fold-literal? (x (acc (make-accumulator)))
  #\[
  (:zom (:or (:and (:dtext? x) (:collect x acc))
	     (:and (:quoted-pair? x) (:collect x acc))))
  #\]
  (:return acc))

;; 3.6.5. Informational fields
;; subject         =       "Subject:" unstructured CRLF
;; comments        =       "Comments:" unstructured CRLF
;; keywords        =       "Keywords:" phrase *("," phrase) CRLF

(rfc2822-header subject (x) "Subject:" (:unstructured? x))
(rfc2822-header comments (x) "Comments:" (:unstructured? x))

(defparser RFC2822-FIELDS.KEYWORDS? (XS C)
  (:SEQ "Keywords:")
  (:phrase? C)
  (:DO (PUSH C XS))
  (:zom #\,
	(:phrase? c)
	(:do (push c xs)))
  (:CRLF?)
  (:RETURN (list 'KEYWORDS (NREVERSE XS))))

;; 3.6.6. Resent fields
;; resent-date     =       "Resent-Date:" date-time CRLF
;; resent-from     =       "Resent-From:" mailbox-list CRLF
;; resent-sender   =       "Resent-Sender:" mailbox CRLF
;; resent-to       =       "Resent-To:" address-list CRLF
;; resent-cc       =       "Resent-Cc:" address-list CRLF
;; resent-bcc      =       "Resent-Bcc:" (address-list / [CFWS]) CRLF
;; resent-msg-id   =       "Resent-Message-ID:" msg-id CRLF

(rfc2822-header resent-date (x) "Resent-Date:" (:rfc2822-date-time? x))
(rfc2822-header resent-from (x) "Resent-From:" (:mailbox-list? x))
(rfc2822-header resent-sender (x) "Resent-Sender:" (:mailbox? x))
(rfc2822-header resent-to (x) "Resent-To:" (:address-list? x))
(rfc2822-header resent-cc (x) "Resent-Cc:" (:address-list? x))
(rfc2822-header resent-bcc (x) "Resent-Bcc:" (:or (:address-list? x) (:optional (:cfws?))))
(rfc2822-header resent-msg-id (x) "Resent-Message-ID:" (:rfc2822-msg-id? x))

;; 3.6.7. Trace fields
;; trace           =       [return]
;;                         1*received
;; return          =       "Return-Path:" path CRLF
;; path            =       ([CFWS] "<" ([CFWS] / addr-spec) ">" [CFWS]) /
;;                         obs-path
;; received        =       "Received:" name-val-list ";" date-time CRLF
;; name-val-list   =       [CFWS] [name-val-pair *(CFWS name-val-pair)]
;; name-val-pair   =       item-name CFWS item-value
;; item-name       =       ALPHA *(["-"] (ALPHA / DIGIT))
;; item-value      =       1*angle-addr / addr-spec /
;;                          atom / domain / msg-id

;; Received: from zpam.core.gen.tr ([127.0.0.1]) by localhost (zpam.core.gen.tr [127.0.0.1]) (amavisd-new, port 10024) with ESMTP id 65gk-VHxskhp for <aycan.irican@core.gen.tr>; Sat, 28 Jun 2008 00:14:15 +0300 (EEST)
;; Received: from lists-outbound.sourceforge.net (lists-outbound.sourceforge.net [66.35.250.225]) by zpam.core.gen.tr (Postfix) with ESMTP id 09E87434F0 for <aycan.irican@core.gen.tr>; Sat, 28 Jun 2008 00:13:58 +0300 (EEST)

(defparser rfc2822-fields.trace? (x xs)
  (:optional (:return?))
  (:oom (:and (:rfc2822-fields.received? x) (:do (push x xs))))
  (:return xs))

(defparser rfc2822-fields.return? (x)
  (:seq "Return-Path:")
  (:rfc2822-fields.path? x)
  (:crlf?)
  (:return (list 'Return-Path x)))

(defparser rfc2822-fields.path? (x)
  (:or (:and (:optional (:cfws?))
	     #\<
	     (:and (:optional (:cfws?))
		   (:addr-spec? x))
	     #\>
	     (:optional (:cfws?)))
       (:obs-path? x))
  (:return x))

(defparser rfc2822-fields.received? (x dt)
  (:seq "Received:")
  (:rfc2822-fields.name-val-list? x)
  #\;
  (:rfc2822-date-time? dt)
  (:return (list 'Received x dt)))

(defparser rfc2822-fields.name-val-list? (x xs)
  (:optional (:cfws?))
  (:optional (:rfc2822-fields.name-val-pair? x)
	     (:do (push x xs))
	     (:zom (:cfws?)
		   (:rfc2822-fields.name-val-pair? x)
		   (:do (push x xs))))
  (:return xs))

(defparser rfc2822-fields.name-val-pair? (n v)
  (:rfc2822-fields.item-name? n)
  (:cfws?)
  (:rfc2822-fields.item-value? v)
  (:return (cons n v)))

(defparser rfc2822-fields.item-name? (x st1 (acc (make-accumulator)))
  (:type alpha? x)
  (:collect x acc)
  (:zom (:and (:optional #\- (:do (setq st1 t)))
	      (:or (:type alpha? x)
		   (:type digit? x))
	      (:if st1 (:and (:collect #\- acc) (:do (setq st1 nil))))
	      (:collect x acc)))
  (:return acc))

(defparser rfc2822-fields.item-value? (x aadr)
  (:or (:oom (:and (:angle-addr? x) (:do (concatenate 'string aadr x))))
       (:addr-spec? x)
       (:atom? x)
       (:domain? x)
       (:rfc2822-msg-id? x))
  (:return (or aadr x)))

;; 3.6.8. Optional fields
;; optional-field  =       field-name ":" unstructured CRLF
;; field-name      =       1*ftext
;; ftext           =       %d33-57 /               ; Any character except
;;                         %d59-126                ;  controls, SP, and
;;                                                 ;  ":".

(defparser rfc2822-fields.optional-field? (fname val)
  (:rfc2822-fields.field-name? fname)
  #\:
  (:unstructured? val)
  (:return (cons fname val)))

(defparser rfc2822-fields.field-name? (x (xs (make-accumulator)))
  (:oom (:rfc2822-fields.ftext? x)
	(:collect x xs))
  (:return xs))

(defparser rfc2822-fields.ftext? (c)
  (:satisfy #'(lambda (x)
		(and (or (> x 32) (< x 127))
		     (not (= x 58))))
	    c)
  (:return c))

;; 4.1. scellaneous obsolete tokens
;;
;; obs-qp          =       "\" (%d0-127)
;; obs-text        =       *LF *CR *(obs-char *LF *CR)
;; obs-char        =       %d0-9 / %d11 /          ; %d0-127 except CR and
;;                         %d12 / %d14-127         ;  LF
;; obs-utext       =       obs-text
;; obs-phrase      =       word *(word / ". " / CFWS)
;; obs-phrase-list =       phrase / 1*([phrase] [CFWS] "," [CFWS]) [phrase]

(defparser obs-qp? (c (acc (make-accumulator)))
  #\\
  (:satisfy #'(lambda (x) (> x -1) (< x 128)) c)
  (:collect #\\ acc) (:collect c acc)
  (:return acc))

(defparser obs-text? (c)
  (:zom (:type linefeed?))
  (:zom (:type carriage-return?))
  (:zom (:obs-char? c)
	(:zom (:type linefeed?))
	(:zom (:type carriage-return?)))
  (:return c))

(defparser obs-char? (c)
  (:satisfy #'(lambda (x)
		(and (or (> x -1) (< x 128))
		     (not (or (= x 10) (= x 13)))))
	    c)
  (:return c))

(defparser obs-utext? (c)
  (:obs-text? c)
  (:return c))

(defparser obs-phrase? (c cs)
  (:word? c)
  (:do (push c cs))
  (:zom (:or (:and (:word? c)
		   (:do (push c cs)))
	     #\.
	     (:cfws?)))
  (:return cs))

(defparser obs-phrase-list? (c cs)
  (:or (:and (:phrase? c) (:return c))
       (:oom (:optional (:phrase? c)
			(:do (nconc c cs)))
	     (:optional (:cfws?))
	     #\,
	     (:optional (:cfws?)))
       (:optional (:phrase? c)
		  (:do (nconc c cs))))
  (:return cs))

;; 4.2. Obsolete folding white space
;; obs-FWS         =       1*WSP *(CRLF 1*WSP)

(defparser obs-fws? ()
  (:oom (:type white-space?))
  (:zom (:crlf?)
	(:oom (:type white-space?)))
  (:return t))

;; 4.3. Obsolete Date and Time
;; obs-day-of-week =       [CFWS] day-name [CFWS]
;; obs-year        =       [CFWS] 2*DIGIT [CFWS]
;; obs-month       =       CFWS month-name CFWS
;; obs-day         =       [CFWS] 1*2DIGIT [CFWS]
;; obs-hour        =       [CFWS] 2DIGIT [CFWS]
;; obs-minute      =       [CFWS] 2DIGIT [CFWS]
;; obs-second      =       [CFWS] 2DIGIT [CFWS]
;; obs-zone        =       "UT" / "GMT" /          ; Universal Time
;;                                                 ; North American UT
;;                                                 ; offsets
;;                         "EST" / "EDT" /         ; Eastern:  - 5/ - 4
;;                         "CST" / "CDT" /         ; Central:  - 6/ - 5
;;                         "MST" / "MDT" /         ; Mountain: - 7/ - 6
;;                         "PST" / "PDT" /         ; Pacific:  - 8/ - 7
;;                         %d65-73 /               ; Military zones - "A"
;;                         %d75-90 /               ; through "I" and "K"
;;                         %d97-105 /              ; through "Z", both
;;                         %d107-122               ; upper and lower case

;; (eq (rfc2822-date-time? (make-core-stream "Mon, 21 Sep 1980 10:01:02 +0230")) 2547376262)

(defparser obs-day-of-week? (dow)
  (:and (:optional (:cfws?))
	(:rfc2822-day-name? dow)
	(:optional (:cfws?)))
  (:return dow))

(defparser obs-year? (c)
  (:optional (:cfws?))
  (:twodigitint? c)
  (:optional (:cfws?))
  (:return c))

(defparser obs-month? (m)
  (:cfws?)
  (:rfc2822-month-name? m)
  (:cfws?) 
  (:return m))

;; TODO: fix it
(defparser obs-day? (c)
  (:optional (:cfws?))
  (:twodigitint? c)
  (:optional (:cfws?))
  (:return c))

;; TODO: put constraint 0 < c < 24
(defparser obs-hour? (c)
  (:optional (:cfws?))
  (:twodigitint? c)
  (:optional (:cfws?))
  (:return c))

;; TODO: put constraint 0 < c < 60
(defparser obs-minute? (c)
  (:optional (:cfws?))
  (:twodigitint? c)
  (:optional (:cfws?))
  (:return c))

;; TODO: put constraint 0 < c < 60
(defparser obs-second? (c)
  (:optional (:cfws?))
  (:twodigitint? c)
  (:optional (:cfws?))
  (:return c))

(defun mkzone (offset)
  offset)

;; TODO: complete it (military zones, EEST etc.)
(defparser obs-zone? ()
  (:or (:and (:or (:seq "UT") (:seq "GMT") #\Z)
	     (:return (mkzone 0))) 
       (:and (:or (:seq "EST") (:seq "CDT"))
	     (:return (mkzone -5)))
       (:and (:seq "EDT") (:return (mkzone -4)))
       (:and (:or (:seq "CST") (:seq "MDT"))
	     (:return (mkzone -6)))
       (:and (:or (:seq "MST") (:seq "PDT"))
	     (:return (mkzone -7)))
       (:and (:seq "PST") (:return (mkzone -8)))))

;; 4.4. Obsolete Addressing
;; obs-angle-addr  =       [CFWS] "<" [obs-route] addr-spec ">" [CFWS]
;; obs-route       =       [CFWS] obs-domain-list ":" [CFWS]
;; obs-domain-list =       "@" domain *(*(CFWS / "," ) [CFWS] "@" domain)
;; obs-local-part  =       word *("." word)
;; obs-domain      =       atom *("." atom)
;; obs-mbox-list   =       1*([mailbox] [CFWS] "," [CFWS]) [mailbox]
;; obs-addr-list   =       1*([address] [CFWS] "," [CFWS]) [address]

(defparser obs-angle-addr? (r addr)
  (:optional (:cfws?))
  #\<
  (:optional (:obs-route? r))
  (:addr-spec? addr)
  #\>
  (:optional (:cfws?))
  (:return (cons r addr)))

(defparser obs-route? (ds)
  (:optional (:cfws?))
  (:obs-domain-list? ds)
  #\:
  (:optional (:cfws?))
  (:return ds))

(defparser obs-domain-list? (d ds)
  #\@
  (:domain? d)
  (:do (push d ds))
  (:zom (:zom (:or (:cfws?) #\,))
	(:optional (:cfws?))
	#\@
	(:domain? d)
	(:do (push d ds)))
  (:return (nreverse ds)))

(defparser obs-local-part? (w ws)
  (:word? w)
  (:do (push w ws))
  (:zom (:word? w)
	(:do (push w ws)))
  (:return (nreverse ws)))

(defparser obs-domain? (a as)
  (:atom? a)
  (:do (push a as))
  (:zom #\.
	(:atom? a)
	(:do (push a as)))
  (:return (nreverse as)))

(defparser obs-mbox-list? (r axs)
  (:oom (:optional (:mailbox? r)
		   (:do (push r axs)))
	(:optional (:cfws?))
	#\,
	(:optional (:cfws?)))
  (:optional (:mailbox? r)
	     (:do (push r axs)))
  (:return (nreverse axs)))

(defparser obs-addr-list? (r axs)
  (:oom (:optional (:address? r)
		   (:do (push r axs)))
	(:optional (:cfws?))
	#\,
	(:optional (:cfws?)))
  (:optional (:address? r)
	     (:do (push r axs)))
  (:return (nreverse axs)))

;; 4.5. Obsolete header fields
;; obs-fields      =       *(obs-return /
;;                         obs-received /
;;                         obs-orig-date /
;;                         obs-from /
;;                         obs-sender /
;;                         obs-reply-to /
;;                         obs-to /
;;                         obs-cc /
;;                         obs-bcc /
;;                         obs-message-id /
;;                         obs-in-reply-to /
;;                         obs-references /
;;                         obs-subject /
;;                         obs-comments /
;;                         obs-keywords /
;;                         obs-resent-date /
;;                         obs-resent-from /
;;                         obs-resent-send /
;;                         obs-resent-rply /
;;                         obs-resent-to /
;;                         obs-resent-cc /
;;                         obs-resent-bcc /
;;                         obs-resent-mid /
;;                         obs-optional)

(defparser obs-fields? (x xs)
  (:zom (:and (:obs-return? x) (:do (push x xs)))
	(:and (:obs-received? x) (:do (push x xs)))
	(:and (:obs-orig-date? x) (:do (push x xs)))
	(:and (:obs-from? x) (:do (push x xs)))
	(:and (:obs-sender? x) (:do (push x xs)))
	(:and (:obs-reply-to? x) (:do (push x xs)))
	(:and (:obs-to? x) (:do (push x xs)))
	(:and (:obs-cc? x) (:do (push x xs)))
	(:and (:obs-bcc? x) (:do (push x xs)))
	(:and (:obs-message-id? x) (:do (push x xs)))
	(:and (:obs-in-reply-to? x) (:do (push x xs)))
	(:and (:obs-references? x) (:do (push x xs)))
	(:and (:obs-subject? x) (:do (push x xs)))
	(:and (:obs-comments? x) (:do (push x xs)))
	(:and (:obs-keywords? x) (:do (push x xs)))
	(:and (:obs-resent-date? x) (:do (push x xs)))
	(:and (:obs-resent-from? x) (:do (push x xs)))
	(:and (:obs-resent-send? x) (:do (push x xs)))
	(:and (:obs-resent-rply? x) (:do (push x xs)))
	(:and (:obs-resent-to? x) (:do (push x xs)))
	(:and (:obs-resent-cc? x) (:do (push x xs)))
	(:and (:obs-resent-bcc? x) (:do (push x xs)))
	(:and (:obs-resent-mid? x) (:do (push x xs)))
	(:and (:obs-optional? x) (:do (push x xs)))))

(defmacro rfc2822-header-obs (name var txt &body body)
  `(defparser ,(intern (format nil "~A?" name)) ,var
     (:seq ,txt)
     (:zom (:type white-space?))
     #\:
     ,@body
     (:crlf?)
     (:return ,(car var))))

;; 4.5.1. Obsolete origination date field
;; obs-orig-date   =       "Date" *WSP ":" date-time CRLF

(rfc2822-header-obs obs-orig-date (x) "Date" (:rfc2822-date-time? x))

;; 4.5.2. Obsolete originator fields
;; obs-from        =       "From" *WSP ":" mailbox-list CRLF
;; obs-sender      =       "Sender" *WSP ":" mailbox CRLF
;; obs-reply-to    =       "Reply-To" *WSP ":" mailbox-list CRLF

(rfc2822-header-obs obs-from (x) "From" (:mailbox-list? x))
(rfc2822-header-obs obs-sender (x) "Sender" (:mailbox? x))
(rfc2822-header-obs obs-reply-to (x) "Reply-To" (:mailbox-list? x))

;; 4.5.3. Obsolete destination address fields
;; obs-to          =       "To" *WSP ":" address-list CRLF
;; obs-cc          =       "Cc" *WSP ":" address-list CRLF
;; obs-bcc         =       "Bcc" *WSP ":" (address-list / [CFWS]) CRLF

(rfc2822-header-obs obs-to (x) "To" (:address-list? x))
(rfc2822-header-obs obs-cc (x) "Cc" (:address-list? x))
(rfc2822-header-obs obs-bcc (x) "Bcc" (:or (:address-list? x) (:optional (:cfws?))))

;; 4.5.4. Obsolete identification fields
;; obs-message-id  =       "Message-ID" *WSP ":" msg-id CRLF
;; obs-in-reply-to =       "In-Reply-To" *WSP ":" *(phrase / msg-id) CRLF
;; obs-references  =       "References" *WSP ":" *(phrase / msg-id) CRLF
;; obs-id-left     =       local-part
;; obs-id-right    =       domain

(rfc2822-header-obs obs-message-id (x) "Message-ID" (:rfc2822-msg-id? x))
(rfc2822-header-obs obs-in-reply-to (x) "In-Reply-To" (:zom (:or (:phrase? x) (:rfc2822-msg-id? x))))
(rfc2822-header-obs obs-references (x) "References" (:zom (:or (:phrase? x) (:rfc2822-msg-id? x))))
(defparser obs-id-left? (x)
  (:local-part? x)
  (:return x))
(defparser obs-id-right? (x)
  (:domain? x)
  (:return x))

;; 4.5.5. Obsolete informational fields
;; obs-subject     =       "Subject" *WSP ":" unstructured CRLF
;; obs-comments    =       "Comments" *WSP ":" unstructured CRLF
;; obs-keywords    =       "Keywords" *WSP ":" obs-phrase-list CRLF

(rfc2822-header-obs obs-subject (x) "Subject" (:unstructured? x))
(rfc2822-header-obs obs-comments (x) "Comments" (:unstructured? x))
(rfc2822-header-obs obs-keywords (x) "Keywords" (:obs-phrase-list? x))

;; 4.5.6. Obsolete resent fields
;; obs-resent-from =       "Resent-From" *WSP ":" mailbox-list CRLF
;; obs-resent-send =       "Resent-Sender" *WSP ":" mailbox CRLF
;; obs-resent-date =       "Resent-Date" *WSP ":" date-time CRLF
;; obs-resent-to   =       "Resent-To" *WSP ":" address-list CRLF
;; obs-resent-cc   =       "Resent-Cc" *WSP ":" address-list CRLF
;; obs-resent-bcc  =       "Resent-Bcc" *WSP ":"
;;                          (address-list / [CFWS]) CRLF
;; obs-resent-mid  =       "Resent-Message-ID" *WSP ":" msg-id CRLF
;; obs-resent-rply =       "Resent-Reply-To" *WSP ":" address-list CRLF

(rfc2822-header-obs obs-resent-from (x) "Resent-From" (:mailbox-list? x))
(rfc2822-header-obs obs-resent-send (x) "Resent-Sender" (:mailbox? x))
(rfc2822-header-obs obs-resent-date (x) "Resent-Date" (:rfc2822-date-time? x))
(rfc2822-header-obs obs-resent-to (x) "Resent-To" (:address-list? x))
(rfc2822-header-obs obs-resent-cc (x) "Resent-Cc" (:address-list? x))
(rfc2822-header-obs obs-resent-bcc (x) "Resent-Bcc" (:or (:address-list? x) (:optional (:cfws?))))
(rfc2822-header-obs obs-resent-mid (x) "Resent-Message-ID" (:rfc2822-msg-id? x))
(rfc2822-header-obs obs-resent-rply (x) "Resent-Reply-To" (:address-list? x))

;; 4.5.7. Obsolete trace fields
;; obs-return      =       "Return-Path" *WSP ":" path CRLF
;; obs-received    =       "Received" *WSP ":" name-val-list CRLF
;; obs-path        =       obs-angle-addr
(rfc2822-header-obs obs-return (x) "Return-Path" (:path? x))
(rfc2822-header-obs obs-received (x) "Received" (:rfc2822-fields.name-val-list? x))
(defparser obs-path? (x)
  (:obs-angle-addr? x)
  (:return x))

;; 4.5.8. Obsolete optional fields
;; obs-optional    =       field-name *WSP ":" unstructured CRLF

(defparser obs-optional? (fn val)
  (:rfc2822-fields.field-name? fn)
  (:zom (:type white-space?))
  #\:
  (:unstructured? val)
  (:return (list fn val)))

(deftrace rfc2822-parsers
    '(fws? ctext? ccontent? comment? cfws? atext? atom? dot-atom? dot-atom-text?
      qtext? qcontent? quoted-string? word? phrase? utext? unstructured? twodigitint?
      fourdigitint? rfc2822-date-time? rfc2822-day-of-week? rfc2822-day-name?
      rfc2822-date? rfc2822-year? rfc2822-month? rfc2822-month-name?
      rfc2822-day? rfc2822-time? rfc2822-time-of-day? rfc2822-hour? rfc2822-minute?))