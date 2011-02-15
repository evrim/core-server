;;;-----------------------------------------------------------------------------
;;; RFC 2616 - Hypertext Transfer Protocol -- HTTP/1.1
;;;-----------------------------------------------------------------------------
(in-package :tr.gen.core.server)

;;;-----------------------------------------------------------------------------
;;; HTTP METHOD/PROTOCOL
;;;-----------------------------------------------------------------------------
(defrule http-protocol? (version)
  (:seq "HTTP/") (:version? version)
  (:return (list 'HTTP version)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar +http-request-methods+
    '(options get head post put delete trace connect)))

(defrule http-method? (c (val (make-accumulator)))
  (:zom (:type alpha? c) (:collect c val))  
  (:return (car (member (string-upcase val) +http-request-methods+
			:test #'string=))))

(defrender http-method! (method)
  (:symbol! method))

;;;-----------------------------------------------------------------------------
;;; HTTP HEADER TYPES
;;;-----------------------------------------------------------------------------
(defatom http-header-name? ()
  (and (visible-char? c) (not (eq c #.(char-code #\:)))))

(defatom http-header-value? ()
  (or (visible-char? c) (space? c)))

(defatom separator? ()
  (if (member c '#.(cons 9 (mapcar #'char-code '(#\( #\) #\< #\> #\@
						 #\, #\; #\: #\\ #\"
						 #\/ #\[ #\] #\? #\=
						 #\{ #\} #\ ))))
      t))

(defatom tokenatom? ()
  (and (not (separator? c))
       (not (control? c))))

(defrule token? (c (acc (make-accumulator)))
  (:type tokenatom? c)
  (:collect c acc)
  (:zom (:type tokenatom? c)
	(:collect c acc))
  (:return acc))

(defrule http-field-name? (c)
  (:token? c)
  (:return c))

;; Ex: ;asd=asd or ;asd="asd"
;; header-parameter? :: stream -> (attr . val)
(defrule header-parameter? (attr val)
  (:and #\; (:lwsp?)
	(:token? attr)
	#\=
	(:or (:token? val)
	     (:quoted? val)))
  (:return (cons attr val)))

(defrender header-parameter! (hp)
  #\; (car hp) #\= (:quoted! (cdr hp)))

;; 3.8 product tokens
;; product         = token [ "/" product-version]
;; product-version = token
(defrule product-version? (c)
  (:token? c)
  (:return c))

(defrule product? (prod ver)
  (:token? prod)
  (:zom #\/
	(:product-version? ver))
  (:return (cons prod ver)))

;;;-----------------------------------------------------------------------------
;;; 4.5 HTTP GENERAL HEADERS
;;;-----------------------------------------------------------------------------
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defvar +http-general-headers+
    '(CACHE-CONTROL CONNECTION DATE PRAGMA TRAILER TRANSFER-ENCODING UPGRADE VIA 
      WARNING))) ;; len=9

;; 14.9 Cache-Control
;;     Cache-Control   = "Cache-Control" ":" 1#cache-directive
;;     cache-directive = cache-request-directive | cache-response-directive
;;     cache-request-directive =
;;            "no-cache"                          ; Section 14.9.1
;;          | "no-store"                          ; Section 14.9.2
;;          | "max-age" "=" delta-seconds         ; Section 14.9.3, 14.9.4
;;          | "max-stale" [ "=" delta-seconds ]   ; Section 14.9.3
;;          | "min-fresh" "=" delta-seconds       ; Section 14.9.3
;;          | "no-transform"                      ; Section 14.9.5
;;          | "only-if-cached"                    ; Section 14.9.4
;;          | cache-extension                     ; Section 14.9.6
;;      cache-response-directive =
;;            "public"                               ; Section 14.9.1
;;          | "private" [ "=" <"> 1#field-name <"> ] ; Section 14.9.1
;;          | "no-cache" [ "=" <"> 1#field-name <"> ]; Section 14.9.1
;;          | "no-store"                             ; Section 14.9.2
;;          | "no-transform"                         ; Section 14.9.5
;;          | "must-revalidate"                      ; Section 14.9.4
;;          | "proxy-revalidate"                     ; Section 14.9.4
;;          | "max-age" "=" delta-seconds            ; Section 14.9.3
;;          | "s-maxage" "=" delta-seconds           ; Section 14.9.3
;;          | cache-extension                        ; Section 14.9.6
;;     cache-extension = token [ "=" ( token | quoted-string ) ]
;; FIXmE: Implement cache-extension
(defvar +http-cache-request-directives+
  '(no-cache no-store max-age max-stale min-fresh no-transform only-if-cached))

(defrule http-cache-control? (result val)
  (:oom (:or (:and (:seq "no-cache") (:do (push (cons 'no-cache nil) result)))
	     (:and (:seq "no-store") (:do (push (cons 'no-store nil) result)))
	     (:and (:seq "max-age") #\= (:fixnum? val) (:do (push (cons 'max-age val) result)))
	     (:and (:seq "max-stale") #\= (:fixnum? val) (:do (push (cons 'max-stale val) result)))
	     (:and (:seq "min-fresh") #\= (:fixnum? val) (:do (push (cons 'min-fresh val) result)))
	     (:and (:seq "no-transform") (:do (push (cons 'no-transform nil) result)))
	     (:and (:seq "only-if-cached") (:do (push (cons 'only-if-cached nil) result))))
	(:zom (:type (or tab? space?)))
	(:optional #\,)
	(:zom (:type (or tab? space?))))
  (:return (nreverse result)))

(defvar +http-cache-response-directives+
  '(public private no-cache no-store no-transform must-revalidate
    proxy-revalidate max-age s-maxage))

(defrender %http-cache-control! (cache-control-cons)   
  (:cond
    ((atom cache-control-cons)
     cache-control-cons)
    ((typep (cdr cache-control-cons) 'fixnum)
     (car cache-control-cons) #\= (cdr cache-control-cons))
    ((typep (cdr cache-control-cons) 'cons)
     (car cache-control-cons) #\, (cadr cache-control-cons) #\=
     (cddr cache-control-cons))
    ((typep (cdr cache-control-cons) 'string)
     (car cache-control-cons) #\= (:quoted! (cdr cache-control-cons)))
    (t
     (:do (error "Invalid Cache-Control declaration.")))))

(defun http-cache-control! (stream cache-controls)
  (let ((cache-controls (ensure-list cache-controls)))
    (flet ((cache-control! (cache-control-cons)
	     (if (atom cache-control-cons)
		 (typecase cache-control-cons
		   (null t)
		   (symbol (symbol! stream cache-control-cons))
		   (string (string! stream cache-control-cons)))      
		 (typecase (cdr cache-control-cons)
		   (fixnum (symbol! stream (car cache-control-cons))
			   (char! stream #\=)
			   (fixnum! stream (cdr cache-control-cons)))
		   (cons (symbol! stream (car cache-control-cons))
			 (char! stream #\,)
			 (string! stream (cadr cache-control-cons))
			 (char! stream #\=)
			 (quoted! stream (cddr cache-control-cons)))
		   (string (symbol! stream (car cache-control-cons))
			   (char! stream #\=)
			   (quoted! stream (cdr cache-control-cons)))
		   (t (error "Invalid Cache-Control declaration!"))))))
      (cache-control! (car cache-controls))
      (mapcar (lambda (cache-control-cons)
		(string! stream ", ")
		(cache-control! cache-control-cons))
	      (cdr cache-controls)))))

;; 14.10 Connection
;; Connection = "Connection" ":" 1#(connection-token)
;; connection-token  = token
(defrule http-connection? (tokens c)
  (:token? c)
  (:do (push c tokens))
  (:zom #\, (:lwsp?)
	(:token? c)
	(:do (push c tokens)))
  (:return tokens))

(defun/cc2 http-connection! (stream connection)
  (typecase connection
    (symbol (symbol! stream connection))
    (string (string! stream connection))))

;; 14.18 Date
;; Date  = "Date" ":" HTTP-date
;;;-----------------------------------------------------------------------------
;;; DATE TImE FORmATS (see 3.3.1)
;;;-----------------------------------------------------------------------------
;;
;;        Sun, 06 Nov 1994 08:49:37 GmT  ; RFC 822, updated by RFC 1123
;;        Sunday, 06-Nov-94 08:49:37 GmT ; RFC 850, obsoleted by RFC 1036
;;        Sun Nov  6 08:49:37 1994       ; ANSI C's asctime() format
;;
;;        HTTP-date    = rfc1123-date | rfc850-date | asctime-date
;;        rfc1123-date = wkday "," SP date1 SP time SP "GMT"
;;        rfc850-date  = weekday "," SP date2 SP time SP "GMT"
;;        asctime-date = wkday SP date3 SP time SP 4DIGIT
;;        date1        = 2DIGIT SP month SP 4DIGIT
;;                       ; day month year (e.g., 02 Jun 1982)
;;        date2        = 2DIGIT "-" month "-" 2DIGIT
;;                       ; day-month-year (e.g., 02-Jun-82)
;;        date3        = month SP ( 2DIGIT | ( SP 1DIGIT ))
;;                       ; month day (e.g., Jun  2)
;;        time         = 2DIGIT ":" 2DIGIT ":" 2DIGIT
;;                       ; 00:00:00 - 23:59:59
;;        wkday        = "Mon" | "Tue" | "Wed"
;;                     | "Thu" | "Fri" | "Sat" | "Sun"
;;        weekday      = "Monday" | "Tuesday" | "Wednesday"
;;                     | "Thursday" | "Friday" | "Saturday" | "Sunday"
;;        month        = "Jan" | "Feb" | "Mar" | "Apr"
;;                     | "May" | "Jun" | "Jul" | "Aug"
;;                     | "Sep" | "Oct" | "Nov" | "Dec"
(defun find-rfc1123-month (str)
  (aif (position (string-downcase str) '("jan" "feb" "mar" "apr" "may" "jun" "jul"
					"aug" "sep" "oct" "nov" "dec")
		:test #'equal)
       (1+ it)))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defvar +rfc1123-day-names+ '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
  (defvar +rfc1123-month-names+ '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul"
				  "Aug" "Sep" "Oct" "Nov" "Dec")))

;; Sun, 06 Nov 1994 08:49:37 GmT  ; RFC 822, updated by RFC 1123
(defrule rfc1123-date? ((acc (make-accumulator)) c
			day month year hour minute second)
  (:zom (:type visible-char?)) (:lwsp?)
  (:fixnum? day) (:lwsp?)
  (:zom (:type alpha? c)
	(:collect c acc))
  (:do (setq month (find-rfc1123-month acc)))
  (:lwsp?) (:fixnum? year) (:lwsp?)
  (:fixnum? hour) #\: (:fixnum? minute) #\: (:fixnum? second) 
  (:lwsp?) (:seq "GMT") (:lwsp?)
  (:return (encode-universal-time second minute hour day month year 0)))

;; Sunday, 06-Nov-94 08:49:37 GmT ; RFC 850, obsoleted by RFC 1036
(defrule rfc850-date? (day month year hour minute second
			   (acc (make-accumulator)) c)
  (:zom (:type visible-char?))
  (:lwsp?)
  (:fixnum? day) #\-
  (:zom (:type alpha? c) (:collect c acc))
  (:do (setq month (find-rfc1123-month acc)))
  #\- (:fixnum? year)
  (:do (setq year (+ 1900 year)))    
  (:lwsp?)
  (:fixnum? hour) #\: (:fixnum? minute) #\: (:fixnum? second)
  (:lwsp?) (:seq "GMT") (:lwsp?)
  (:return (encode-universal-time second minute hour day month year 0)))

;;        Sun Nov  6 08:49:37 1994       ; ANSI C's asctime() format
(defrule asctime-date? ((acc (make-accumulator)) c
			day month year hour minute second)
  (:zom (:type visible-char?))
  (:lwsp?)
  (:zom (:type alpha? c) (:collect c acc))
  (:do (setq month (find-rfc1123-month acc)))
  (:lwsp?) (:fixnum? day) (:lwsp?)
  (:fixnum? hour) #\: (:fixnum? minute) #\: (:fixnum? second)
  (:lwsp?)
  (:fixnum? year)
  (:return (encode-universal-time second minute hour day month year 0)))

(defrule http-date? (timestamp)
  (:or (:rfc1123-date? timestamp)   
       (:rfc850-date? timestamp)
       (:asctime-date? timestamp))
  (:return timestamp))

;;Sun, 06 Nov 1994 08:49:37 GmT
(defun/cc2 http-date! (stream timestamp)
  (multiple-value-bind (second minute hour day month year day-of-week)
      (decode-universal-time timestamp 0)
    (string! stream
	     (format nil "~a, ~2,'0d ~a ~d ~2,'0d:~2,'0d:~2,'0d GMT"
		     (nth day-of-week +rfc1123-day-names+)
		     day (nth month +rfc1123-month-names+) year
		     hour minute second))))

;; 14.32 Pragma
;;
;; Pragma            = "Pragma" ":" 1#pragma-directive
;; pragma-directive  = "no-cache" | extension-pragma
;; extension-pragma  = token [ "=" ( token | quoted-string ) ]
(defrule extension-pragma? ((attr (make-accumulator)) val)
  (:lwsp?)
  (:http-field-name? attr)
  #\=
  (:quoted? val)
  (:return (cons attr val)))

(defrule http-pragma? (pragma)
  (:or (:and (:seq "no-cache") (:do (setq pragma (cons 'no-cache nil))))
       (:extension-pragma? pragma))
  (:return pragma))

(defun http-pragma! (stream pragma-cons)
  (if (atom pragma-cons)
      (symbol! stream pragma-cons)
      (typecase (car pragma-cons)
	(symbol (symbol! stream (car pragma-cons)))
	(string (string! stream (car pragma-cons))
		(char! stream #\=)
		(string! stream (cdr pragma-cons))))))

;; 14.40 Trailer
;;
;; Trailer  = "Trailer" ":" 1#field-name
(defrule http-trailer? (fields c)
  (:http-field-name? c)
  (:do (push c fields))
  (:zom #\, (:lwsp?)
	(:http-field-name? c)
	(:do (push c fields)))
  (:return fields))

(defun http-trailer! (stream fields)
  (if (car fields) 
      (progn
	(string! stream (car fields))
	(reduce #'(lambda (acc item)
		    (declare (ignore acc))
		    (char! stream #\,)
		    (string! stream item))
		(cdr fields) :initial-value nil))))

;; 14.41 Transfer-Encoding
;;
;; Transfer-Encoding       = "Transfer-Encoding" ":" 1#transfer-coding
;; Transfer-Encoding: chunked (see 3.6)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defrule http-transfer-extension? (tok param params)
    (:token? tok)
    (:zom (:header-parameter? param)
	  (:do (push param params)))
    (:return (cons tok params))))

(defun http-transfer-extension! (stream te)
  (typecase (car te)
    (string (string! stream (car te)))
    (symbol (symbol! stream (car te))))
  (when (cdr te)
    (reduce #'(lambda (acc item)
		(declare (ignore acc))
		(header-parameter! stream item))
	    (cdr te) :initial-value nil)))

;; http-transfer-encoding? :: stream -> (token . (attr . val))
(defrule http-transfer-encoding? (encs c)
  (:http-transfer-extension? c)
  (:do (push c encs))
  (:zom #\, (:lwsp?)
	(:http-transfer-extension? c)
	(:do (push c encs)))
  (:return encs))

;; http-transfer-encoding! :: stream -> ((token . (attr . val)) ...) -> nil
(defun http-transfer-encoding! (stream encodings)
  (with-separator (i encodings #\, stream)
    (http-transfer-extension! stream i)))

;; 14.42 Upgrade
;; Upgrade        = "Upgrade" ":" 1#product
;; Upgrade: HTTP/2.0, SHTTP/1.3, IRC/6.9, RTA/x11
(defrule http-upgrade? (c products)
  (:product? c)
  (:do (push c products))
  (:zom #\, (:lwsp?)
	(:product? c)
	(:do (push c products)))
  (:return products))

(defun http-upgrade! (stream products)
  (with-separator (i products #\, stream)
    (string! stream (car i))
    (char! stream #\/)
    (string! stream (cdr i))))

;; 14.45 Via
;; Via =  "Via" ":" 1#( received-protocol received-by [ comment ] )
;;       received-protocol = [ protocol-name "/" ] protocol-version
;;       protocol-name     = token
;;       protocol-version  = token
;;       received-by       = ( host [ ":" port ] ) | pseudonym
;;       pseudonym         = token
;; Via: 1.0 fred, 1.1 nowhere.com (Apache/1.1)
;; Via: 1.0 ricky, 1.1 ethel, 1.1 fred, 1.0 lucy
;; Via: 1.0 ricky, 1.1 mertz, 1.0 lucy
(defrule via-received-by? (c)
  (:or (:hostport? c)
       (:token? c))
  (:return c))

(defrule via-received-protocol? (pname pver)
  (:token? pname)
  (:or (:and #\/
	     (:token? pver)
	     (:return (cons pname pver)))
       (:return (cons nil pname))))

;; ex: (("HTTP" . "1.1") ("core.gen.tr" . 80) "core server site")
(defrule http-via? (prot by comment vias)
  (:via-received-protocol? prot)
  (:lwsp?)
  (:via-received-by? by)
  (:lwsp?)
  (:optional (:comment? comment))
  (:do (push (list prot by comment) vias))
  (:zom #\, (:lwsp?)
	(:via-received-protocol? prot)
	(:lwsp?)
	(:via-received-by? by)
	(:lwsp?)
	(:optional (:comment? comment))
	(:do (push (list prot by comment) vias)))
  (:return (nreverse vias)))

(defun http-via! (stream vias)
  (with-separator (i vias #\, stream)
    (when (car i)
      (if (caar i) 
	  (progn
	    (string! stream (caar i))
	    (char! stream #\/)
	    (string! stream (cdar i)))
	  (string! stream (cdar i))))
    (char! stream #\ )
    (when (cadr i)
      (hostport! stream (cadr i)))
    (char! stream #\ )
    (when (caddr i)
      (comment! stream (caddr i)))))

;; 14.46 Warning
;; Warning    = "Warning" ":" 1#warning-value
;; warning-value = warn-code SP warn-agent SP warn-text [SP warn-date]
;; warn-code  = 3DIGIT
;; warn-agent = ( host [ ":" port ] ) | pseudonym
;;           ; the name or pseudonym of the server adding
;;           ; the Warning header, for use in debugging
;; warn-text  = quoted-string
;; warn-date  = <"> HTTP-date <">
(defrule warn-agent? (agent)
  (:or (:hostport? agent)
       (:token? agent))
  (:return agent))

(defrule warn-code? (c)
  (:fixnum? c)
  (:return c))

(defrule warn-date? (d)
  #\" (:http-date? d) #\"
  (:return d))

(defrule warning-value? (code agent text date)
  (:warn-code? code)
  (:lwsp?)
  (:warn-agent? agent)
  (:lwsp?)
  (:quoted? text)
  (:lwsp?)
  (:warn-date? date)
  (:return (list code agent text date)))

(defrule http-warning? (c acc)
  (:warning-value? c)
  (:do (push c acc))
  (:zom (:warning-value? c)
	(:do (push c acc)))
  (:return acc))

;; warning :: '((199 ("www.core.gen.tr" . 80) "warn text" 3408142800))
(defun http-warning! (stream warnings)
  (with-separator (i warnings #\, stream)
    (fixnum! stream (car i))
    (char! stream #\ )
    (if (cdr (cadr i))
	(hostport! stream (cadr i))
	(string! stream (car (cadr i))))
    (char! stream #\ )
    (quoted! stream (caddr i))
    (char! stream #\ )
    (char! stream #\")
    (http-date! stream (cadddr i))
    (char! stream #\")))

;;;-----------------------------------------------------------------------------
;;; 5.3 HTTP REQUEST HEADERS
;;;-----------------------------------------------------------------------------
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defvar +http-request-headers+
    '(ACCEPT ACCEPT-CHARSET ACCEPT-ENCODING ACCEPT-LANGUAGE AUTHORIZATION
      EXPECT FROM HOST IF-MATCH IF-MODIFIED-SINCE IF-NONE-MATCH IF-RANGE
      IF-UNMODIFIED-SINCE MAX-FORWARDS PROXY-AUTHORIZATION RANGE REFERER
      TE USER-AGENT COOKIE))) ;; len=19

;; 14.1 Accept
;; Accept           = "Accept" ":"
;;                    #( media-range [ accept-params ] )
;; media-range      = ( "*/*"
;;                    | ( type "/" "*" )
;;                    | ( type "/" subtype )
;;                    ) *( ";" parameter )
;; accept-params    = ";" "q" "=" qvalue *( accept-extension )
;; accept-extension = ";" token [ "=" ( token | quoted-string ) ]
;; text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5
(defatom http-media-type? ()
  (and (not (eq c #.(char-code #\,)))
       (not (eq c #.(char-code #\;)))
       (not (eq c #.(char-code #\/)))
       (http-header-name? c)))

;; quality-value? :: stream -> float
;; 3.9 Quality Values
;; qvalue         = ( "0" [ "." 0*3DIGIT  ])
;;                | ( "1" [ "." 0*3("0") ] )
(defrule quality-value? ((val (make-accumulator)) c)
  (:or (:and #\0 (:collect #\0 val)
	     (:or (:and #\. (:collect #\. val)
			(:zom (:type digit? c) (:collect c val)))
		  (:return (float 0))))
       (:and #\1 (:collect #\1 val)
	     (:zom (:type digit? c))))
  (:return (parse-float val)))

;; quality-parameter? :: stream -> ("q" . float)
(defrule quality-parameter? (val)
  (:and #\; (:lwsp?) #\q #\=
	(:quality-value? val))
  (:return (cons "q" val)))

;; http-media-range? :: stream -> (values type subtype ((attr .val) ...))
(defrule http-media-range? ((type (make-accumulator))
			    (subtype (make-accumulator))
			    param params c)
  (:zom (:type http-media-type? c) (:collect c type))
  #\/
  (:zom (:type http-media-type? c) (:collect c subtype))
  (:zom (:or (:quality-parameter? param)
	     ;; accept-extension
	     (:header-parameter? param))
	(:do
	 (push param params)))
  (:return (values type subtype params)))

;; http-accept? :: stream -> ((type subtype ((attr . val) ...)) ...)
(defrule http-accept? (type subtype params accept)
  (:zom (:and (:http-media-range? type subtype params)
	      (:do (push (list type subtype params) accept))
	      (:zom (:not #\,) (:type http-header-name?))) 
	(:zom (:type space?)))
  (:return accept))

;; Http Language
(defatom http-language-type? ()
  (and (not (eq c #.(char-code #\,)))
       (not (eq c #.(char-code #\;)))
       (visible-char? c)))

;; http-language? :: stream -> (language . quality)
(defrule http-language? (c (lang (make-accumulator)) quality)
  (:type http-language-type? c) (:collect c lang)
  (:zom (:type http-language-type? c) (:collect c lang))
  (:or (:and (:quality-parameter? quality)
	     (:return (cons lang (cdr quality))))
       (:return (cons lang 1.0))))

;; 14.2 Accept Charset
;; Accept-Charset = "Accept-Charset" ":" 1#( ( charset | "*" )[ ";" "q" "=" qvalue ] )
;; Accept-Charset: iso-8859-5, unicode-1-1;q=0.8
(defrule http-accept-charset? (e e*)
  (:zom (:and (:http-language? e)
	      (:do (push e e*))
	      (:zom (:not #\,) (:type http-header-name?)))
	(:zom (:type space?)))
  (:return (nreverse e*)))

;; 14.3 Accept Encoding
;; Accept-Encoding  = "Accept-Encoding" ":" 1#( codings [ ";" "q" "=" qvalue ])
;; codings          = ( content-coding | "*" )
(defrule http-accept-encoding? (e e*)
  (:zom (:and (:http-language? e)
	      (:do (push e e*))
	      (:zom (:not #\,) (:type http-header-name?)))
	(:zom (:type space?)))
  (:return (nreverse e*)))

;; 14.4 Accept Language
;; Accept-Language = "Accept-Language" ":" 1#( language-range [ ";" "q" "=" qvalue ])
;; language-range  = ( ( 1*8ALPHA * ( "-" 1*8ALPHA)) | "*" )
;; Accept-Language: da, en-gb;q=0.8, en;q=0.7
(defrule http-accept-language? (langs lang)
  (:zom (:and (:http-language? lang)
	      (:do (push lang langs))
	      (:zom (:not #\,) (:type http-header-name?)))
	(:zom (:type space?)))
  (:return (nreverse langs)))

;; 14.8 Authorization
;; Authorization  = "Authorization" ":" credentials
(defrule http-authorization? (challenge)
  (:http-challenge? challenge)
  (:return challenge))

(defrule expectation-extension? ((attr (make-accumulator))
				 (val (make-accumulator)) c)
  (:and (:lwsp?)
	(:zom (:type alphanum? c) (:collect c attr))
	#\=
	(:zom (:type http-media-type? c)
	      (:collect c val)))
  (:return (cons attr val)))

;; 14.20 Expect
;; Expect       =  "Expect" ":" 1#expectation
;; expectation  =  "100-continue" | expectation-extension
;; expectation-extension =  token [ "=" ( token | quoted-string )
;;                         *expect-params ]
;; expect-params =  ";" token [ "=" ( token | quoted-string ) ]
;; FIXmE: implement extensions.
(defrule http-expect? (expectation param params)
  (:or (:and (:seq "100-continue")
	     (:return '100-continue))
       (:and (:expectation-extension? expectation)
	     (:zom (:and (:header-parameter? param)
			 (:do (push param params))))))
  (:return (cons expectation params)))

;; 14.22 From
;; From   = "From" ":" mailbox
(defrule http-from? (mbox)
  (:mailbox? mbox)
  (:return mbox))

;; 14.23 Host
;; Host = "Host" ":" host [ ":" port ] ; Section 3.2.2
(defrule http-host? (hp)
  (:hostport? hp) (:return hp))

;; 14.19 Etag
;; ETag = "ETag" ":" entity-tag
;; 3.11 Entity Tags
;; entity-tag = [ weak ] opaque-tag
;; weak       = "W/"
;; opaque-tag = quoted-string
;;
;; http-etag? :: stream -> (tagstr . weak?)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defrule http-etag? (tagstr weak?)
    (:checkpoint
     #\W #\/ (:do (setq weak? t)) (:commit))
    (:quoted? tagstr)
    (:return (cons tagstr weak?))))

;; 14.24 If-Match
;; If-Match = "If-Match" ":" ( "*" | 1#entity-tag )
;; If-Match: "xyzzy"
;; If-Match: "xyzzy", "r2d2xxxx", "c3piozzzz"
;; If-Match: *
(defrule http-if-match? (tag (etags '()))
  (:or (:and #\* (:return (cons '* nil)))
       (:and (:http-etag? tag)
	     (:do (push tag etags))
	     (:zom #\, (:lwsp?) (:http-etag? tag)
		   (:do (push tag etags)))))
  (:return (nreverse etags)))

;; 14.25 If-Modified-Since
;; If-Modified-Since = "If-Modified-Since" ":" HTTP-date
;; If-Modified-Since: Sat, 29 Oct 1994 19:43:31 GMT
(defrule http-if-modified-since? (date)
  (:http-date? date)
  (:return date))

;; 14.26 If-None-Match
;; If-None-Match = "If-None-Match" ":" ( "*" | 1#entity-tag )
;; If-None-Match: "xyzzy"
;; If-None-Match: W/"xyzzy"
;; If-None-Match: "xyzzy", "r2d2xxxx", "c3piozzzz"
;; If-None-Match: W/"xyzzy", W/"r2d2xxxx", W/"c3piozzzz"
;; If-None-Match: *
(defrule http-if-none-match? (res)
  (:http-if-match? res)
  (:return res))

;; 14.27 If-Range
;; If-Range = "If-Range" ":" ( entity-tag | HTTP-date )
;; FIXmE: Implement me.
(defrule http-if-range? (range)
  (:or (:http-date? range)
       (:http-etag? range))
  (:return range))

;; 14.28 If-Unmodified-Since
;; If-Unmodified-Since = "If-Unmodified-Since" ":" HTTP-date
;; If-Unmodified-Since: Sat, 29 Oct 1994 19:43:31 GMT
(defrule http-if-unmodified-since? (date)
  (:http-date? date)
  (:return date))

;; 14.31 Max-Forwards
;; Max-Forwards   = "Max-Forwards" ":" 1*DIGIT
(defrule http-max-forwards? (num)
  (:fixnum? num) (:return num))

;; 14.34 Proxy-Authorization
;; Proxy-Authorization     = "Proxy-Authorization" ":" credentials
(defrule http-proxy-authorization? (creds)
  (:http-credentials? creds)
  (:return creds))

;; 14.35 Range
;; 14.35.1 Byte Ranges
;; ranges-specifier = byte-ranges-specifier
;; byte-ranges-specifier = bytes-unit "=" byte-range-set
;; byte-range-set  = 1#( byte-range-spec | suffix-byte-range-spec )
;; byte-range-spec = first-byte-pos \"-\" [last-byte-pos]
;; first-byte-pos  = 1*DIGIT
;; last-byte-pos   = 1*DIGIT
;; suffix-byte-range-spec = \"-\" suffix-length
;; suffix-length = 1*DIGIT

(defrule http-bytes-unit? (c (unit (make-accumulator)))
  (:zom (:type alpha? c)
	(:collect c unit))
  (:return unit))

(defrule http-byte-range-set? (c l r)
  (:or (:and #\- (:fixnum? r) (:do (push (cons nil r) c)))        
       (:and
	(:fixnum? l) #\-       
	(:checkpoint
	 (:fixnum? r)
	 (:commit))
	(:do (push (cons l r) c))))
  (:zom #\, (:lwsp?)
	(:or (:and #\- (:fixnum? r) (:do (push (cons nil r) c)))        
	     (:and
	      (:fixnum? l) #\-       
	      (:checkpoint
	       (:fixnum? r)
	       (:commit))
	      (:do (push (cons l r) c)))))
  (:return c))

(defrule http-byte-ranges-specifier? (bytes-unit ranges)
  (:http-bytes-unit? bytes-unit)
  #\=
  (:http-byte-range-set? ranges)
  (:return (cons bytes-unit ranges)))

(defrule http-ranges-specifier? (c)
  (:http-byte-ranges-specifier? c)
  (:return c))

;; 14.35 Range
;; Range = "Range" ":" ranges-specifier
;; ranges-specifier = byte-ranges-specifier
;; byte-ranges-specifier = bytes-unit "=" byte-range-set
;; byte-range-set  = 1#( byte-range-spec | suffix-byte-range-spec )
;; byte-range-spec = first-byte-pos "-" [last-byte-pos]
;; first-byte-pos  = 1*DIGIT
;; last-byte-pos   = 1*DIGIT
;; suffix-byte-range-spec = "-" suffix-length
;; suffix-length = 1*DIGIT
(defrule http-range? (c)
  (:http-ranges-specifier? c)
  (:return c))

;; 14.36 Referer
;; Referer        = "Referer" ":" ( absoluteURI | relativeURI )
;; Referer: http://www.w3.org/hypertext/DataSources/Overview.html
(defrule http-referer? (uri)
  (:uri? uri) (:return uri))

;; 14.39 TE
;; TE        = "TE" ":" #( t-codings )
;; t-codings = "trailers" | ( transfer-extension [ accept-params ] )
;; TE: deflate
;; TE:
;; TE: trailers, deflate;q=0.5
(defrule http-te? (te acc)
  (:http-transfer-extension? te)
  (:do (push te acc))
  (:zom #\, (:lwsp?)
	(:http-transfer-extension? te)
	(:do (push te acc)))
  (:return (nreverse acc)))

;; 14.43 User-Agent
;; User-Agent     = "User-Agent" ":" 1*( product | comment )
;;
;; http://en.wikipedia.org/wiki/User_agent
;; Mozilla/MozVer (compatible; MSIE IEVer[; Provider]; Platform[; Extension]*) [Addition]
;; Mozilla/MozVer (Platform; Security; SubPlatform; Language; rv:Revision[; Extension]*) Gecko/GeckVer [Product/ProdVer]
(defrule user-agent-token? ((token (make-accumulator)) c)
  (:zom (:and (:not #\;)
	      (:checkpoint
	       #\)
	       (:if (> (length token) 0)
		    (:rewind-return token)
		    (:rewind-return nil)))
	      (:type http-header-value? c)) (:collect c token))
  (:lwsp?)
  (:if (> (length token) 0)
       (:return token)))

;; ie:     Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV1; .NET CLR 1.1.4322; .NET CLR 2.0.50727)
;; => ((BROWSER . IE) (MOZ-VER (4 0)) (VERSION (6 0)) (OS . "Windows NT 5.1"))
(defrule ie-user-agent? (moz-ver version os token)
  (:seq "Mozilla/") (:version? moz-ver) (:lwsp?) #\(
  (:checkpoint
   (:seq "compatible;") (:lwsp?) (:seq "MSIE") (:lwsp?)
   (:version? version)
   #\;
   (:lwsp?) (:user-agent-token? os) (:lwsp?)
   (:zom (:user-agent-token? token))
   #\)
   (:return (list (cons 'browser 'ie)
		  (list 'moz-ver moz-ver)
		  (list 'version version)
		  (cons 'os os)))))

;; ff:     Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.8.1) Gecko/20061010 Firefox/2.0
;; => ((BROWSER . FIREFOX) (MOZ-VER (5 0)) (OS . "Linux i686") (REVISION (1 8 1)) (VERSION (2 0)))
;; sea:    Mozilla/5.0 (X11; U; Linux i686; tr-TR; rv:1.8.1.2) Gecko/20070511 SeaMonkey/1.1.1
;; => ((BROWSER . SEAMONKEY) (MOZ-VER (5 0)) (OS . "Linux i686") (REVISION (1 8 1 2)) (VERSION (1 1 1)))
;; oldmoz: Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.7.13) Gecko/20060522
;; => ((BROWSER . MOZILLA) (MOZ-VER (5 0)) (OS . "Linux i686") (REVISION (1 7 13)))
;; iceweasel: Mozilla/5.0 (X11; U; Linux x86_64; en-US; rv:1.8.1.8) Gecko/20071004 Iceweasel/2.0.0.8 (Debian-2.0.0.8-1)
;; =>
(defrule gecko-user-agent? (moz-ver version os c browser revision)
  (:seq "Mozilla/") (:version? moz-ver) (:lwsp?) #\(
  (:checkpoint
    (:user-agent-token?) (:user-agent-token?) 
    (:do (setq os (make-accumulator))) (:user-agent-token? os)
    (:user-agent-token?) (:lwsp?) (:seq "rv:") (:version? revision)
    #\) (:lwsp?) (:zom (:type http-header-name?))
    (:or (:checkpoint
	   (:lwsp?)
	   (:do (setq browser (make-accumulator)))
	   (:zom (:not #\/) (:type visible-char? c) (:collect c browser))
	   (:version? version)
	   (:zom (:type http-header-value?)) ;; eat rest
	   (:return (list (cons 'browser (intern (string-upcase browser)))
			  (list 'moz-ver moz-ver)
			  (cons 'os os)
			  (list 'revision revision)
			  (list 'version version))))	
	 (:and (:zom (:type http-header-value?)) ;; eat rest
	  (:return (list (cons 'browser 'mozilla)
			 (list 'moz-ver moz-ver)
			 (cons 'os os)
			 (list 'revision revision)))))))

;; opera:  Opera/9.21 (X11; Linux i686; U; en-us)
;;         Opera/9.51 (Windows NT 5.1; U; en)
;; => ((BROWSER . OPERA) (VERSION (9 21)) (OS "X11" "Linux i686") (LANG . "en-us"))
(defrule opera-user-agent? (version os lang tok)
  (:seq "Opera/")
  (:version? version)
  (:lwsp?) #\(
  (:zom
   (:user-agent-token? tok)
   (:do (push tok os)))
  (:do (progn (setf lang (pop os))
	      (pop os))) ;; remove unused "U;"
  #\)
  (:return (list (cons 'browser 'opera)
		 (list 'version version)
		 (cons 'os (reverse os))
		 (cons 'lang lang))))

(defrule unparsed-user-agent? (c (str (make-accumulator)))
  (:zom (:or (:type (or visible-char? white-space?) c))
	(:collect c str))
  (:return (list (cons 'browser 'unknown)
		 (cons 'string str))))

(defrule http-user-agent? (agent)  
  (:or (:ie-user-agent? agent)
       (:gecko-user-agent? agent)
       (:opera-user-agent? agent))
  (:zom (:not (:crlf?)) (:type octet?))
  (:return agent))

;; RFC 2109 Cookie - rfc/2109.lisp
(defrule http-cookie? (cookie)
  (:cookie? cookie)
  (:return cookie))

;;;-----------------------------------------------------------------------------
;;; 5.3 HTTP RESPONSE HEADERS
;;;-----------------------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar +http-response-headers+
    '(ACCEPT-RANGES AGE ETAG LOCATION PROXY-AUTHENTICATE RETRY-AFTER SERVER
      VARY WWW-AUTHENTICATE SET-COOKIE))) ;; len=9

;; 14.5 Accept Ranges
;; Accept-Ranges     = "Accept-Ranges" ":" acceptable-ranges
;; acceptable-ranges = 1#range-unit | "none"
(defun http-accept-ranges! (stream &optional accept-ranges)
  (if accept-ranges
      (string! stream accept-ranges)
      (string! stream "none")))

;; 14.6 Age
;; Age = "Age" ":" age-value
;; age-value = delta-seconds
(defun http-age! (stream age)
  (fixnum! stream age))

;; 14.19 ETag
;; ETag = "ETag" ":" entity-tag
;; ETag: "xyzzy"
;; ETag: W/"xyzzy"
;; ETag: ""
;; FIXmE: whats thiz?
(defun http-etag! (stream etag)
  (if (cdr etag)
      (progn
	(char! stream #\W)
	(char! stream #\/)
	(quoted! stream (car etag)))
      (quoted! stream (car etag))))

;; 14.30 Location
;; Location       = "Location" ":" absoluteURI
;; Location: http://www.w3.org/pub/WWW/People.html
(defun http-location! (stream uri)
  (uri! stream uri))

;; 14.33 Proxy Authenticate
;; Proxy-Authenticate  = "Proxy-Authenticate" ":" 1#challenge
(defun http-proxy-authenticate! (stream challenge)
  (http-challenge! stream challenge))

;; 14.37 Retry-After
;; Retry-After  = "Retry-After" ":" ( HTTP-date | delta-seconds )
;; Retry-After: Fri, 31 Dec 1999 23:59:59 GMT
;; Retry-After: 120
(defun http-retry-after! (stream ra)
  (if (> ra 1000000000)
      (http-date! stream ra)
      (fixnum! stream ra)))

;; 14.38 Server
;; Server         = "Server" ":" 1*( product | comment )
;; Server: CERN/3.0 libwww/2.17
(defun http-server! (stream server)
  (string! stream server))

;; 14.44 Vary
;; Vary  = "Vary" ":" ( "*" | 1#field-name )
(defun http-vary! (stream vary)
  (if (> (length vary) 1)
      (if (car vary)
	  (progn
	    (string! stream (car vary))
	    (reduce #'(lambda (acc atom)
			(declare (ignore acc))
			(char! stream #\,)
			(string! stream atom))
		    (cdr vary) :initial-value nil))) 
      (char! stream #\*)))

;; 14.47 WWW-Authenticate
;; WWW-Authenticate  = "WWW-Authenticate" ":" 1#challenge
(defun http-www-authenticate! (stream challenge)
  (http-challenge! stream challenge))

;; RFC 2109 Cookie - rfc/2109.lisp
(defun http-set-cookie! (stream cookie)
  (cookie! stream cookie))

;;;-----------------------------------------------------------------------------
;;; 7.1 HTTP ENTITY HEADERS
;;;-----------------------------------------------------------------------------
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defvar +http-entity-headers+ 
    '(ALLOW CONTENT-ENCODING CONTENT-LANGUAGE CONTENT-LENGTH CONTENT-LOCATION
      CONTENT-MD5 CONTENT-RANGE CONTENT-TYPE EXPIRES LAST-MODIFIED))) ;; len=10

;; 14.7 Allow
;; Allow   = "Allow" ":" #method
;; Allow: GET, HEAD, PUT
(defrule http-allow? (method methods)
  (:zom (:and (:http-method? method)
	      (:do (push method methods))
	      #\,
	      (:zom (:type space?))))
  (:return (nreverse methods)))

(defun http-allow! (stream methods)
  (let ((methods (ensure-list methods)))
    (symbol! stream (car methods))
    (mapc #'(lambda (atom)
	      (string! stream ", ")
	      (symbol! stream atom))
	  (cdr methods))))

;; 14.11 Content-Encoding
;; Content-Encoding  = "Content-Encoding" ":" 1#content-coding
;; Content-Encoding: gzip
(defrule http-content-encoding? ((acc (make-accumulator)) c)
  (:type http-header-name? c) (:collect c acc)
  (:zom (:type http-header-name? c) (:collect c acc))
  (:return acc))

(defun http-content-encoding! (stream encodings)
  (let ((encodings (ensure-list encodings)))
    (symbol! stream (car encodings))
    (mapc #'(lambda (atom)
	      (string! stream ", ")
	      (symbol! stream atom))
	  (cdr encodings))))

;; 14.12 Content-Language
;; Content-Language  = "Content-Language" ":" 1#language-tag
;; Content-Language: mi, en
(defrule http-content-language? (c (acc (make-accumulator)) (langs '()))
  (:zom (:zom (:type alpha? c) (:collect c acc))
	(:do (push acc langs)
	     (setq acc (make-accumulator)))
	#\,
	(:zom (:type space?)))
  (:return (nreverse langs)))

(defun http-content-language! (stream langs)
  (let ((langs (ensure-list langs)))
    (typecase (car langs)
      (string (string! stream (car langs)))
      (symbol (symbol! stream (car langs))))
    (mapc #'(lambda (atom)
	      (string! stream ", ")
	      (typecase atom
		(string (string! stream atom))
		(symbol (symbol! stream atom))))
	  (cdr langs))))

;; 14.13 Content-Length
;; Content-Length    = "Content-Length" ":" 1*DIGIT
;; Content-Length: 3495
(defrule http-content-length? (num)
  (:fixnum? num) (:return num))

(defun/cc2 http-content-length! (stream length)
  (fixnum! stream length))

;; 14.14 Content-Location
;; Content-Location = "Content-Location" ":" ( absoluteURI | relativeURI )
(defrule http-content-location? (uri)
  (:uri? uri) (:return uri))

(defun http-content-location! (stream uri)
  (uri! stream uri))

;; 14.15 Content-MD5
;; Content-MD5   = "Content-MD5" ":" md5-digest
;; md5-digest   = <base64 of 128 bit MD5 digest as per RFC 1864>
(defrule http-content-md5? (c (acc (make-accumulator)))
  (:type alpha? c) (:collect c acc)  
  (:zom (:type alpha? c) (:collect c acc))
  (:return acc))

(defun http-content-md5! (stream md5)
  (string! stream md5))

;; 14.16 Content-Range
;; Content-Range = "Content-Range" ":" content-range-spec
;; content-range-spec      = byte-content-range-spec
;; byte-content-range-spec = bytes-unit SP
;;                           byte-range-resp-spec "/"
;;                         ( instance-length | "*" )
;; byte-range-resp-spec = (first-byte-pos "-" last-byte-pos) | "*"
;; instance-length           = 1*DIGIT
;; FIXmE: implement me.
(defrule http-content-range? (c (acc (make-accumulator)))
  (:type http-header-value? c) (:collect c acc)
  (:zom (:type http-header-value? c) (:collect c acc))
  (:return acc))

(defun http-content-range! (stream range)
  (string! stream range))

;; 14.17 Content-Type
;; Content-Type   = "Content-Type" ":" media-type
;; Content-Type: text/html; charset=ISO-8859-4
(defrule http-content-type? (type subtype params)
  (:http-media-range? type subtype params)
  (:return (if params
	       (list type subtype params)
	       (list type subtype))))

;; http-content-type! :: (string string . string)
;; ex: '("text" "html" ("charset" . "UTF-8") ...)
;; or: '("text" "javascript" ("charset" "UTF-8") ..)
(defun/cc2 http-content-type! (stream typesubtype-charset-cons)
  (string! stream (car typesubtype-charset-cons))
  (char! stream #\/)
  (string! stream (cadr typesubtype-charset-cons))
  (reduce #'(lambda (acc item)
	      (declare (ignore acc))
	      (char! stream #\;)
	      (string! stream (car item))
	      (char! stream #\=)
	      (if (listp (cdr item))
		  (string! stream (cadr item))
		  (string! stream (cdr item))))
	  (cddr typesubtype-charset-cons) :initial-value nil))

;; 14.21 Expires
;; Expires = "Expires" ":" HTTP-date
;; Expires: Thu, 01 Dec 1994 16:00:00 GMT
(defrule http-expires? (date)
  (:http-date? date) (:return date))

(defun http-expires! (stream timestamp)
  (http-date! stream timestamp))

;; 14.29 Last-Modified
;; Last-Modified  = "Last-Modified" ":" HTTP-date
;; Last-Modified: Tue, 15 Nov 1994 12:45:26 GMT
(defrule http-last-modified? (date)
  (:http-date? date) (:return date))

(defun http-last-modified! (stream timestamp)
  (http-date! stream timestamp))

;;;-----------------------------------------------------------------------------
;;; MODLISP REQUEST HEADERS
;;;-----------------------------------------------------------------------------
(eval-when (:compile-toplevel :execute :load-toplevel)
  (defvar +mod-lisp-request-headers+
    '(url method remote-ip-addr remote-ip-port server-ip-addr server-ip-port
      server-protocol script-filename ssl-session-id server-id
      server-baseversion modlisp-version modlisp-major-version))) ;; len=9

;; 1. URL
(defrule mod-lisp-http-url? (uri)
  (:uri? uri) (:return uri))

(defun mod-lisp-http-url! (stream url)
  (uri! stream url))

;; 2. METHOD
(defrule mod-lisp-http-method? (method)
  (:http-method? method) (:return method))

(defun mod-lisp-http-method! (stream method)
  (http-method! stream method))

;; 3. REMOTE-IP-ADDR
(defrule mod-lisp-http-remote-ip-addr? (ip)
  (:ipv4address? ip) (:return ip))

;; 4. REMOTE-IP-PORT
(defrule mod-lisp-http-remote-ip-port? (port)
  (:port? port) (:return port))

;; 5. SERVER-IP-ADDR
(defrule mod-lisp-http-server-ip-addr? (ip)
  (:ipv4address? ip) (:return ip))

;; 6. SERVER-IP-PORT
(defrule mod-lisp-http-server-ip-port? (port)
  (:port? port) (:return port))

;; 7. SERVER-PROTOCOL
(defrule mod-lisp-http-server-protocol? (proto)
  (:http-protocol? proto) (:return proto))

;; 8. SCRIPT-FILENAME
(defrule mod-lisp-http-script-filename? (c (acc (make-accumulator :byte)))
  (:zom (:type (and (not linefeed?) (not carriage-return?) octet?) c)
	(:collect c acc))
  (:return (octets-to-string acc :utf-8)))

;; 9. SSL-SESSION-ID
(defrule mod-lisp-http-ssl-session-id? (c (acc (make-accumulator)))
  (:zom (:type http-header-value? c) (:collect c acc))
  (:return acc))

;; 10. SERVER-ID
(defrule mod-lisp-http-server-id? (c (acc (make-accumulator)))
  (:zom (:type http-header-value? c) (:collect c acc))
  (:return acc))

;; 11. SERVER BASE VERSION
(defrule mod-lisp-http-server-baseversion? (version)
  (:seq "Apache/") (:version? version)
  (:return (list 'apache version)))

;; 12. mODLISP-VERSION
(defrule mod-lisp-http-modlisp-version? (version)
  (:version? version) (:return version))

;; 13. mODLISP-mAJOR-VERSION
(defrule mod-lisp-http-modlisp-major-version? (version)
  (:version? version) (:return version))

;;;-----------------------------------------------------------------------------
;;; HTTP mESSAGE
;;;-----------------------------------------------------------------------------
(defclass http-message ()
  ((version :accessor http-message.version :initform '(1 1))
   (general-headers :accessor http-message.general-headers :initarg :general-headers :initform '())
   (unknown-headers :accessor http-message.unknown-headers :initform '())
   (entities :accessor http-message.entities :initarg :entities :initform '())))

;;;-----------------------------------------------------------------------------
;;; HTTP REQUEST
;;;-----------------------------------------------------------------------------
(defclass http-request (http-message)
  ((method :accessor http-request.method)
   (uri :accessor http-request.uri :initarg :uri)
   (headers :accessor http-request.headers :initform '())
   (entity-headers :accessor http-request.entity-headers :initform '())
   (peer-type :accessor http-request.peer-type :initform 'http)
   (stream :accessor http-request.stream :initform nil :initarg :stream)))

(defmethod http-request.header ((request http-request) key)
  (cadr (assoc key (http-request.headers request))))

(defmethod http-request.referrer ((request http-request))
  (aif (http-request.header request 'referer)
       (uri.server it)))

(defmethod http-request.cookies ((request http-request))
  (http-request.header request 'cookie))

(defmethod http-request.cookie ((request http-request) name)
  (find name (http-request.cookies request) :key #'cookie.name :test #'string=))

(defrule http-request-first-line? (method uri proto)
  (:http-method? method) (:lwsp?) (:uri? uri) (:lwsp?)
  (:http-protocol? proto) (:return (values method uri (cadr proto))))

(defmacro defhttp-header-parser (name format header-list)
  `(defrule ,name (stub)
     (:or ,@(nreverse
	     (reduce #'(lambda (acc atom)
			 (cons
			  `(:checkpoint
			    (:sci ,(symbol-name atom))
			    (:or (:and #\: (:zom (:type space?)))
				 (:crlf?))
			    (,(intern (format nil format atom) :keyword) stub)
			    (:return (list ',atom stub)))
			  acc))
		     (eval header-list) :initial-value nil)))))

(defhttp-header-parser http-general-header? "HTTP-~A?" +http-general-headers+)
(defhttp-header-parser http-request-header? "HTTP-~A?" +http-request-headers+)
(defhttp-header-parser http-entity-header? "HTTP-~A?" +http-entity-headers+)
(defhttp-header-parser mod-lisp-header? "MOD-LISP-HTTP-~A?" +mod-lisp-request-headers+)

(defrule rfc2616-request-headers? (c method uri version key value header gh rh eh uh)
  (:http-request-first-line? method uri version)
  (:lwsp?)
  (:zom (:or (:and (:http-general-header? header) (:do (push header gh)))		    
	     (:and (:http-request-header? header) (:do (push header rh)))
	     (:and (:http-entity-header? header) (:do (push header eh)))
	     (:and (:do (setq key (make-accumulator)))
		   (:type http-header-name? c) (:collect c key)
		   (:zom (:type http-header-name? c) (:collect c key))
		   (:or (:and #\: (:zom (:type space?)))
			(:and (:zom (:type space?)) (:crlf?)))
		   (:do (setq value (make-accumulator :byte)))
		   (:zom (:type http-header-value? c) (:collect c value)) 
		   (:do (push (cons key value) uh))))
	(:crlf?))
  (:crlf?)
  (:return (values method uri version (nreverse gh)
		   (nreverse rh) (nreverse eh) (nreverse uh))))

(defrule http-unknown-header? ((key (make-accumulator)) (value (make-accumulator :byte)) c)
  (:oom (:type http-header-name? c) (:collect c key))
  (:or (:and #\: (:zom (:type space?))) (:crlf?))
  (:oom (:type http-header-value? c) (:collect c value))
  (:return (cons key value)))

(defrule mod-lisp-request-headers? (header mlh gh rh eh uh)
  (:mod-lisp-header? header) (:do (push header mlh)) (:crlf?)
  (:zom (:or (:and (:http-general-header? header) (:do (push header gh)))
	     (:and (:http-request-header? header) (:do (push header rh)))
	     (:and (:http-entity-header? header) (:do (push header eh)))	     
	     (:and (:mod-lisp-header? header) (:do (push header mlh)))
	     (:and (:http-unknown-header? header) (:do (push header uh))))
	(:optional (:crlf?))
	(:checkpoint
	  (:seq "end")
	  (:return (values  (cadr (assoc 'method mlh))	      ;;method
			    (cadr (assoc 'url mlh))	      ;; uri
			    (cadadr (assoc 'server-protocol mlh)) ;; version
			    (nreverse gh) (nreverse rh)
			    (nreverse eh) (nreverse uh)
			    (nreverse mlh)))))
  (:return (values  (cadr (assoc 'method mlh))	    ;;method
		    (cadr (assoc 'url mlh))	    ;; uri
		    (cadadr (assoc 'server-protocol mlh)) ;; version
		    (nreverse gh) (nreverse rh)
		    (nreverse eh) (nreverse uh)
		    (nreverse mlh))))

(defrule http-request-headers? (method uri version gh rh eh uh mlh)
  (:or (:and (:rfc2616-request-headers? method uri version gh rh eh uh)
	     (:return (values 'http method uri version gh rh eh uh)))
       (:and (:mod-lisp-request-headers? method uri version gh rh eh uh mlh)
	     (:return (values 'mod-lisp method uri version gh (append rh mlh) eh uh)))))

(defrule x-www-form-urlencoded? (query)  
  (:query? query)
  (:return query))


(deftrace http-headers
    (append (list 'http-request-first-line? 'rfc2616-request-headers?
		  'http-unknown-header? 'mod-lisp-request-headers?
		  'http-general-header? 'http-request-header? 'http-entity-header?
		  'mod-lisp-header?)
	    (append
	     (mapcar (lambda (header) (intern (format nil "HTTP-~A?" header)))
		     (append +http-general-headers+ +http-request-headers+
			     +http-entity-headers+))
	     (mapcar (lambda (header) (intern (format nil "MOD-LISP-HTTP-~A?" header)))
		     +mod-lisp-request-headers+))))

;;;-----------------------------------------------------------------------------
;;; HTTP RESPONSE
;;;-----------------------------------------------------------------------------
(defclass http-response (http-message)
  ((response-headers :accessor http-response.response-headers
		     :initarg :response-headers :initform '())
   (status-code :accessor http-response.status-code :initform (make-status-code 200))
   (entity-headers :accessor http-response.entity-headers :initform '()
		   :initarg :entity-headers)
   (peer-type :accessor http-response.peer-type :initform 'http
	      :initarg :peer-type)
   (stream :accessor http-response.stream :initform nil :initarg :stream)))

(defmethod http-response.add-entity-header ((self http-response) key val)
  (setf (slot-value self 'entity-headers)
	(cons (cons key val)
	      (remove-if #'(lambda (a) (eq a key))
			 (slot-value self 'entity-headers) :key #'car))))

(defmethod http-response.add-response-header ((self http-response) key val)
  (setf (slot-value self 'response-headers)
	(cons (cons key val)
	      (remove-if #'(lambda (a) (eq a key))
			 (slot-value self 'response-headers) :key #'car))))

(defmethod http-response.add-general-header ((self http-response) key val)
  (setf (slot-value self 'general-headers)
	(cons (cons key val)
	      (remove-if #'(lambda (a) (eq a key))
			 (slot-value self 'general-headers) :key #'car))))

(defmethod http-response.disable-cache ((self http-response))
  (http-response.add-general-header self 'cache-control 'no-cache)
  (http-response.add-general-header self 'pragma 'no-cache)
  (http-response.add-entity-header self 'expires 0))

(defmethod http-response.set-content-type ((self http-response) content-type)
  (http-response.add-entity-header self 'content-type content-type))

(defmethod http-response.add-cookie ((self http-response) (cookie cookie))
  (setf (slot-value self 'response-headers)
	(cons (cons 'set-cookie cookie)
	      (remove-if #'(lambda (a)
			     (and (typep (cdr a) 'cookie)
				  (string= (cookie.name (cdr a)) (cookie.name cookie))))
			 (slot-value self 'response-headers)))))

(defmacro defhttp-header-render (name format header-list) 
  (let ((hname (gensym)))
    `(defun/cc2 ,name (stream hdr)
       (let ((,hname (car hdr)))
	 (cond
	   ,@(mapcar #'(lambda (h)
			 `((eql ,hname ',h)
			   (progn
			     (string! stream (symbol-name ',h))
			     (char! stream #\:)
			     (char! stream #\ ) 
			     (,(intern (format nil format h)) stream (cdr hdr)))))
		     (eval header-list))
	   (t (error (format nil "Unknown header name: ~A" (car hdr)))))) 
       (char! stream #\Newline))))

(defhttp-header-render http-general-header! "HTTP-~A!" +http-general-headers+)
(defhttp-header-render http-response-header! "HTTP-~A!" +http-response-headers+)
(defhttp-header-render http-entity-header! "HTTP-~A!" +http-entity-headers+)
;; (defhttp-header-render mod-lisp-header! "MOD-LISP-HTTP-~A!" +mod-lisp-response-headers+)

(defmacro defmod-lisp-header-render (name format header-list) 
  (let ((hname (gensym)))
    `(defun ,name (stream hdr)
       (funcall (let ((,hname (car hdr)))
		  (cond
		    ,@(mapcar #'(lambda (h)
				  `((eql ,hname ',h)
				    (progn
				      (symbol! stream ',h)
				      (char! stream #\Newline)
				      (function ,(intern (format nil format h))))))
			      (eval header-list))
		    (t (error (format nil "Unknown header name: ~A" (car hdr))))))
		stream (cdr hdr))
       (char! stream #\Newline))))

(defmod-lisp-header-render mod-lisp-general-header! "HTTP-~A!" +http-general-headers+)
(defmod-lisp-header-render mod-lisp-response-header! "HTTP-~A!" +http-response-headers+)
(defmod-lisp-header-render mod-lisp-entity-header! "HTTP-~A!" +http-entity-headers+)
;; (defmod-lisp-header-render mod-lisp-header! "MOD-LISP-HTTP-~A!" +mod-lisp-response-headers+)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *status-codes*
    '((100 . "Continue")
      (101 . "Switching Protocols")
      (200 . "OK")
      (201 . "Created")
      (202 . "Accepted")
      (203 . "Non-Authoritative Information")
      (204 . "No Content")
      (205 . "Reset Content")
      (206 . "Partial Content")
      (300 . "Multiple Choices")
      (301 . "Moved Permanently")
      (302 . "Found")
      (303 . "See Other")
      (304 . "Not Modified")
      (305 . "Use Proxy")
      (307 . "Temporary Redirect")
      (400 . "Bad Request")
      (401 . "Unauthorized")
      (402 . "Payment Required")
      (403 . "Forbidden")
      (404 . "Not Found")
      (405 . "Method Not Allowed")
      (406 . "Not Acceptable")
      (407 . "Proxy Authentication Required")
      (408 . "Request Time-out")
      (409 . "Conflict")
      (410 . "Gone")
      (411 . "Length Required")
      (412 . "Precondition Failed")
      (413 . "Request Entity Too Large")
      (414 . "Request-URI Too Large")
      (415 . "Unsupported Media Type")
      (416 . "Requested range not satisfiable")
      (417 . "Expectation Failed")
      (500 . "Internal Server Error")
      (501 . "Not Implemented")
      (502 . "Bad Gateway")
      (503 . "Service Unavailable")
      (504 . "Gateway Time-out")
      (505 . "HTTP Version not supported")))
  
  (defun make-status-code (num &optional description)
    (if description
	(cons num description)
	(assoc num *status-codes*))))

;; status-code! :: stream -> [Int] -> (Int, String)
(defun/cc2 status-code! (stream version status-code)
  (string! stream  "HTTP/")
  (version! stream version)
  (char! stream #\ )
  (fixnum! stream (car status-code))
  (char! stream #\ )
  (string! stream (cdr status-code))
  (char! stream #\Newline))

(defun/cc2 http-response-headers! (stream response)
  ;; Status-Line
  (status-code! stream
		(http-message.version response)
		(http-response.status-code response))
  ;; general headers
  (reduce #'(lambda (acc item)
	      (declare (ignorable acc))
	      (http-general-header! stream item))
	  (http-message.general-headers response) :initial-value nil)
  ;; response headers
  (reduce #'(lambda (acc item)
	      (declare (ignorable acc))
	      (http-response-header! stream item))
	  (http-response.response-headers response) :initial-value nil)
  ;; entity headers
  (reduce #'(lambda (acc item)
	      (declare (ignorable acc))
	      (http-entity-header! stream item))
	  (http-response.entity-headers response) :initial-value nil))

(defun mod-lisp-status-code! (stream version status-code)
  (declare (ignore version))
  (string! stream "Status")
  (char! stream #\Newline)
  (fixnum! stream (car status-code))
  (char! stream #\ )
  (string! stream (cdr status-code))
  (char! stream #\Newline))

(defun mod-lisp-response-headers! (stream response)
  (mod-lisp-status-code! stream
			 (http-message.version response)
			 (http-response.status-code response))
  ;; general headers
  (reduce #'(lambda (acc item)
	      (declare (ignorable acc))
	      (mod-lisp-general-header! stream item))
	  (http-message.general-headers response) :initial-value nil)
  ;; response headers
  (reduce #'(lambda (acc item)
	      (declare (ignorable acc))
	      (mod-lisp-response-header! stream item))
	  (http-response.response-headers response) :initial-value nil)
  ;; entity headers
  (reduce #'(lambda (acc item)
	      (declare (ignorable acc))
	      (mod-lisp-entity-header! stream item))
	  (http-response.entity-headers response) :initial-value nil))

;; ;;;-----------------------------------------------------------------------------
;; ;;; MOD-LISP REQUEST
;; ;;;-----------------------------------------------------------------------------
;; (defclass mod-lisp-request (http-request)
;;   ())

;; ;;;-----------------------------------------------------------------------------
;; ;;; MOD-LISP RESPONSE
;; ;;;-----------------------------------------------------------------------------
;; (defclass mod-lisp-response (http-response)
;;   ())
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
