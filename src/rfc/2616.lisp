(in-package :tr.gen.core.server)
;;;-----------------------------------------------------------------------------
;;; RFC 2616 - Hypertext Transfer Protocol -- HTTP/1.1
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;; HTTP mETHOD/PROTOCOL
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

(defun http-method! (stream method)
  (symbol! stream method))

;;;-----------------------------------------------------------------------------
;;; HTTP HEADER TYPES
;;;-----------------------------------------------------------------------------
(defatom http-header-name? ()
  (and (visible-char? c) (not (eq c #.(char-code #\:)))))

(defatom http-header-value? ()
  (or (visible-char? c) (space? c)))

;;;-----------------------------------------------------------------------------
;;; 4.5 HTTP GENERAL HEADERS
;;;-----------------------------------------------------------------------------
(eval-when (:compile-toplevel :execute)
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

(defrule http-cache-control? (val)
  (:or (:and (:seq "no-cache") (:return (cons 'no-cache nil)))
       (:and (:seq "no-store") (:return (cons 'no-store nil)))
       (:and (:seq "max-age") #\= (:fixnum? val) (:return (cons 'max-age val)))
       (:and (:seq "max-stale") #\= (:fixnum? val) (:return (cons 'max-stale val)))
       (:and (:seq "min-fresh") #\= (:fixnum? val) (:return (cons 'min-fresh val)))
       (:and (:seq "no-transform") (:return (cons 'no-transform nil)))
       (:and (:seq "only-if-cached") (:return (cons 'only-if-cached nil)))))

(defvar +htt-cache-response-directives+
  '(public private no-cache no-store no-transform must-revalidate
    proxy-revalidate max-age s-maxage))

(defun http-cache-control! (stream cache-control-cons)
  (if (atom cache-control-cons)
      (typecase cache-control-cons
	(symbol (symbol! stream cache-control-cons))
	(string (string! stream cache-control-cons)))      
      (typecase (cdr cache-control-cons)
	(symbol (symbol! stream (car cache-control-cons))
		#\= (symbol! stream (cdr cache-control-cons)))
	(string (symbol! stream (car cache-control-cons))
		#\= (string! stream (cdr cache-control-cons)))
	(fixnum (symbol! stream (car cache-control-cons))
		#\= (fixnum! stream (cdr cache-control-cons)))
	(t (error "Invalid Cache-Control declaration!")))))

;; 14.10 Connection
;; Connection = "Connection" ":" 1#(connection-token)
;; connection-token  = token
(defrule http-connection? ((acc (make-accumulator)) c)
  (:type http-header-name? c) (:collect c acc)
  (:zom (:type http-header-name? c) (:collect c acc))
  (:return acc))

(defun http-connection! (stream connection)
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
  (position (string-downcase str) '("jan" "feb" "mar" "apr" "may" "jun" "jul"
				    "aug" "sep" "oct" "nov" "dec")
	    :test #'equal))

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
  (:return (encode-universal-time second minute hour day month year)))

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
  (:return (encode-universal-time second minute hour day month year)))

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
  (:return (encode-universal-time second minute hour day month year)))

(defrule http-date? (timestamp)
  (:or (:rfc1123-date? timestamp)   
       (:rfc850-date? timestamp)
       (:asctime-date? timestamp))
  (:return timestamp))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defvar +rfc1123-day-names+ '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
  (defvar +rfc1123-month-names+ '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul"
				  "Aug" "Sep" "Oct" "Nov" "Dec")))

;;Sun, 06 Nov 1994 08:49:37 GmT
(defun http-date! (stream timestamp)
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
;;
;; FIXmE: implement extension-pragma
(defrule http-pragma? ()
  (:seq "no-cache") (:return (cons 'no-cache nil)))

(defun http-pragma! (stream pragma-cons)
  (when pragma-cons
    (string! stream "no-cache")))

;; 14.40 Trailer
;;
;; Trailer  = "Trailer" ":" 1#field-name
;;
(defrule http-trailer? ((field (make-accumulator)) c)
  (:zom (:type alpha? c) (:collect c field)) (:return field))

(defun http-trailer! (stream field-name)
  (string! stream field-name))

;; 14.41 Transfer-Encoding
;;
;; Transfer-Encoding       = "Transfer-Encoding" ":" 1#transfer-coding
;; Transfer-Encoding: chunked (see 3.6)
(defrule http-transfer-encoding? ((encoding (make-accumulator)) c)
  (:type alpha? c) (:collect c encoding)
  (:zom (:type alpha? c) (:collect c encoding))
  (:return encoding))

(defun http-transfer-encoding! (stream encoding)
  (typecase encoding
    (string (string! stream encoding))
    (symbol (symbol! stream encoding))))

;; 14.42 Upgrade
;; Upgrade        = "Upgrade" ":" 1#product
;; Upgrade: HTTP/2.0, SHTTP/1.3, IRC/6.9, RTA/x11
;; FIXmE: implement as seperate protocols.
(defrule http-upgrade? (c (acc (make-accumulator)))
  (:type http-header-value? c) (:collect c acc)
  (:zom (:type http-header-value? c) (:collect c acc))
  (:return acc))

(defun http-upgrade! (stream upgrade)
  (string! stream upgrade))

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
;; FIXME: implement as seperate protocols.
(defrule http-via? (c (acc (make-accumulator)))
  (:type http-header-value? c) (:collect c acc)
  (:zom (:type http-header-value? c) (:collect c acc))
  (:return acc))

(defun http-via! (stream via)
  (string! stream via))

;; 14.46 Warning
;; Warning    = "Warning" ":" 1#warning-value
;; warning-value = warn-code SP warn-agent SP warn-text [SP warn-date]
;; warn-code  = 3DIGIT
;; warn-agent = ( host [ ":" port ] ) | pseudonym
;;           ; the name or pseudonym of the server adding
;;           ; the Warning header, for use in debugging
;; warn-text  = quoted-string
;; warn-date  = <"> HTTP-date <">
;; FIXME: implement me.
(defrule http-warning? (c (acc (make-accumulator)))
  (:type http-header-value? c) (:collect c acc)
  (:zom (:type http-header-value? c) (:collect c acc))
  (:return acc))

(defun http-warning! (stream warning)
  (string! stream warning))

;;;-----------------------------------------------------------------------------
;;; 5.3 HTTP REQUEST HEADERS
;;;-----------------------------------------------------------------------------
(eval-when ( :compile-toplevel :execute)
  (defvar +http-request-headers+
    '(ACCEPT ACCEPT-CHARSET ACCEPT-ENCODING ACCEPT-LANGUAGE AUTHORIZATION
      EXPECT FROM HOST IF-MATCH IF-MODIFIED-SINCE IF-RANGE IF-UNMODIFIED-SINCE
      MAX-FORWARDS PROXY-AUTHORIZATION RANGE REFERER TE USER-AGENT))) ;; len=18

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

;; header-parameter? :: stream -> (attr . val)
(defrule header-parameter? ((attr (make-accumulator))
			    (val (make-accumulator)) c)
  (:and #\; (:lwsp?)
	(:zom (:type alphanum? c) (:collect c attr))
	#\=
	(:zom (:type http-media-type? c)
	      (:collect c val)))
  (:return (cons attr val)))

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
;; FIXmE: parse quality values
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
;; FIXmE: parse quality
(defrule http-accept-language? (langs lang)
  (:zom (:and (:http-language? lang)
	      (:do (push lang langs))
	      (:zom (:not #\,) (:type http-header-name?)))
	(:zom (:type space?)))
  (:return (nreverse langs)))

;; 14.8 Authorization
;; Authorization  = "Authorization" ":" credentials
;; FIXmE: who's gonna implement me?
(defrule http-authorization? ()
  (:return nil))

;; 14.20 Expect
;; Expect       =  "Expect" ":" 1#expectation
;; expectation  =  "100-continue" | expectation-extension
;; expectation-extension =  token [ "=" ( token | quoted-string )
;;                         *expect-params ]
;; expect-params =  ";" token [ "=" ( token | quoted-string ) ]
;; FIXmE: implement extensions.
(defrule http-expect? ()
  (:seq "100-continue") (:return '100-continue))

;; 14.22 From
;; From   = "From" ":" mailbox
;; FIXmE: Implement mailbox parser from RFC 1123
(defrule http-from? (c (acc (make-accumulator)))
  (:type visible-char? c) (:collect c acc)
  (:zom (:type visible-char? c) (:collect c acc))
  (:return acc))

;; 14.23 Host
;; Host = "Host" ":" host [ ":" port ] ; Section 3.2.2
(defrule http-host? (hp)
  (:hostport? hp) (:return hp))

;; 14.24 If-Match
;; If-Match = "If-Match" ":" ( "*" | 1#entity-tag )
;; If-Match: "xyzzy"
;; If-Match: "xyzzy", "r2d2xxxx", "c3piozzzz"
;; If-Match: *
(defrule http-if-match? (q (entities '()))
  (:or (:and #\* (:return (cons '* nil)))
       (:and (:quoted? q) (:do (push q entities))
	     #\, (:lwsp?)
	     (:zom (:and (:quoted? q) (:do (push q entities)))
		   #\, (:lwsp?))))
  (:return (nreverse entities)))

;; 14.25 If-Modified-Since
;; If-Modified-Since = "If-Modified-Since" ":" HTTP-date
;; If-Modified-Since: Sat, 29 Oct 1994 19:43:31 GMT
(defrule http-if-modified-since? (date)
  (:http-date? date) (:return date))

;; 14.26 If-None-Match
;; If-None-Match = "If-None-Match" ":" ( "*" | 1#entity-tag )
;; If-None-Match: "xyzzy"
;; If-None-Match: W/"xyzzy"
;; If-None-Match: "xyzzy", "r2d2xxxx", "c3piozzzz"
;; If-None-Match: W/"xyzzy", W/"r2d2xxxx", W/"c3piozzzz"
;; If-None-Match: *
;; FIXmE: Implement me.
(defrule http-if-none-match? (c (acc (make-accumulator :byte)))
  (:type http-header-value? c) (:collect c acc)
  (:zom (:type http-header-value? c) (:collect c acc))
  (:return (octets-to-string acc :utf-8)))

;; 14.27 If-Range
;; If-Range = "If-Range" ":" ( entity-tag | HTTP-date )
;; FIXmE: Implement me.
(defrule http-if-range? ()
  (:return nil))

;; 14.28 If-Unmodified-Since
;; If-Unmodified-Since = "If-Unmodified-Since" ":" HTTP-date
;; If-Unmodified-Since: Sat, 29 Oct 1994 19:43:31 GMT
(defrule http-if-unmodified-since? (date)
  (:http-date? date) (:return date))

;; 14.31 Max-Forwards
;; Max-Forwards   = "Max-Forwards" ":" 1*DIGIT
(defrule http-max-forwards? (num)
  (:fixnum? num) (:return num))

;; 14.34 Proxy-Authorization
;; Proxy-Authorization     = "Proxy-Authorization" ":" credentials
;; FIXmE: Implement me.
(defrule http-proxy-authorization? ()
  (:return nil))

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
;; FIXmE: Implement me.
(defrule http-range? ()
  (:return nil))

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
;; FIXmE: Implement me.
(defrule http-te? ()
  (:return nil))

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
   (:and #\; (:lwsp?)
	 (:user-agent-token? os) (:lwsp?)
	 (:zom (:user-agent-token? token))
	 #\)
	 (:return (list (cons 'browser 'ie)
			(list 'moz-ver moz-ver)
			(list 'version version)
			(cons 'os os))))))

;; ff:     Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.8.1) Gecko/20061010 Firefox/2.0
;; => ((BROWSER . FIREFOX) (MOZ-VER (5 0)) (OS . "Linux i686") (REVISION (1 8 1)) (VERSION (2 0)))
;; sea:    Mozilla/5.0 (X11; U; Linux i686; tr-TR; rv:1.8.1.2) Gecko/20070511 SeaMonkey/1.1.1
;; => ((BROWSER . SEAMONKEY) (MOZ-VER (5 0)) (OS . "Linux i686") (REVISION (1 8 1 2)) (VERSION (1 1 1)))
;; oldmoz: Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.7.13) Gecko/20060522
;; => ((BROWSER . MOZILLA) (MOZ-VER (5 0)) (OS . "Linux i686") (REVISION (1 7 13)))
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
	 (:return (list (cons 'browser (intern (string-upcase browser)))
			(list 'moz-ver moz-ver)
			(cons 'os os)
			(list 'revision revision)
			(list 'version version))))	
	(:return (list (cons 'browser 'mozilla)
		       (list 'moz-ver moz-ver)
		       (cons 'os os)
		       (list 'revision revision))))))

;; opera:  Opera/9.21 (X11; Linux i686; U; en)
;; => ((BROWSER . OPERA) (VERSION (9 21)) (OS . "Linux i686"))
(defrule opera-user-agent? (version os)
  (:seq "Opera/")
  (:version? version)
  (:lwsp?) #\( (:user-agent-token?)
  (:user-agent-token? os)
  (:zom (:type http-header-value?))
  (:return (list (cons 'browser 'opera)
		 (list 'version version)
		 (cons 'os os))))

(defrule http-user-agent? (agent)  
  (:or (:ie-user-agent? agent)
       (:gecko-user-agent? agent)
       (:opera-user-agent? agent))
  (:return agent))

;;;-----------------------------------------------------------------------------
;;; 5.3 HTTP RESPONSE HEADERS
;;;-----------------------------------------------------------------------------
(eval-when ( :compile-toplevel :execute)
  (defvar +http-response-headers+
    '(ACCEPT-RANGES AGE ETAG LOCATION PROXY-AUTHENTICATE RETRY-AFTER SERVER
      VARY WWW-AUTHENTICATE))) ;; len=9

;; 14.5 Accept Ranges
;; Accept-Ranges     = "Accept-Ranges" ":" acceptable-ranges
;; acceptable-ranges = 1#range-unit | "none"
;; FIXmE: implement extenstion token for range-unit
(defun http-accept-ranges! (stream accept-ranges)
  (if accept-ranges
      (string! stream "bytes")
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
  (quoted! stream etag))

;; 14.30 Location
;; Location       = "Location" ":" absoluteURI
;; Location: http://www.w3.org/pub/WWW/People.html
(defun http-location! (stream uri)
  (uri! stream uri))

;; 14.33 Proxy Authenticate
;; Proxy-Authenticate  = "Proxy-Authenticate" ":" 1#challenge
(defun http-proxy-authenticate! (stream pa)
  (string! stream pa))

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
      (reduce #'(lambda (acc atom)
		  (declare (ignore acc))
		  (string! stream atom)
		  (char! stream #\,))
	      vary :initial-value nil)
      (char! stream #\*)))

;; 14.47 WWW-Authenticate
;; WWW-Authenticate  = "WWW-Authenticate" ":" 1#challenge
(defun http-www-authenticate! (stream wa)
  (string! stream wa))

;;;-----------------------------------------------------------------------------
;;; 7.1 HTTP ENTITY HEADERS
;;;-----------------------------------------------------------------------------
(eval-when ( :compile-toplevel :execute)
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

(defun http-content-langauge! (stream langs)
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

(defun http-content-length! (stream length)
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

(defun http-content-type! (stream type-subtype-cons)
  (string! stream (car type-subtype-cons))
  (string! stream "; charset=")
  (string! stream (cdr type-subtype-cons)))

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
(eval-when (:compile-toplevel :execute)
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
   (general-headers :accessor http-message.general-headers :initform '())
   (unknown-headers :accessor http-message.unknown-headers :initform '())
   (entities :accessor http-message.entities :initform '())))

;;;-----------------------------------------------------------------------------
;;; HTTP REQUEST
;;;-----------------------------------------------------------------------------
(defclass http-request (http-message)
  ((method :accessor http-request.method)
   (uri :accessor http-request.uri)
   (headers :accessor http-request.headers :initform '())
   (entity-headers :accessor http-request.entity-headers :initform '())))

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
  (:zom (:or
	 (:and (:http-general-header? header) (:do (push header gh)))		    
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
	(:zom (:type space?))
	(:crlf?)
	(:checkpoint
	 (:crlf?)
	 (:return (values method uri version (nreverse gh)
			  (nreverse rh) (nreverse eh) (nreverse uh))))))

(defrule mod-lisp-request-headers? (c header mlh gh rh eh key value uh)
  (:and (:mod-lisp-header? header) (:do (push header mlh)) (:crlf?)
	     (:zom (:or (:and (:http-general-header? header) (:do (push header gh)))
			(:and (:http-request-header? header) (:do (push header rh)))
			(:and (:http-entity-header? header) (:do (push header eh)))
			(:and (:mod-lisp-header? header) (:do (push header mlh)))
			(:and (:do (setq key (make-accumulator)))
			      (:type http-header-name? c) (:collect c key)
			      (:zom (:type http-header-name? c) (:collect c key))
			      (:or (:and #\: (:zom (:type space?)))
				   (:crlf?))
			      (:do (setq value (make-accumulator :byte)))
			      (:zom (:type http-header-value? c) (:collect c value))
			      (:do (push (cons key value) uh))))
		   (:crlf?)
		   (:checkpoint
		    (:seq "end")
		    (:return (values  (cadr (assoc 'method mlh)) ;;method
				      (cadr (assoc 'url mlh)) ;; uri
				      (cadadr (assoc 'server-protocol mlh)) ;; version
				      (nreverse gh) (nreverse rh)
				      (nreverse eh) (nreverse uh)
				      (nreverse mlh)))))))

(defrule http-request-headers? (method uri version gh rh eh uh mlh)
  (:or (:and (:rfc2616-request-headers? method uri version gh rh eh uh)
	     (:return (values method uri version gh rh eh uh)))
       (:and (:mod-lisp-request-headers? method uri version gh rh eh uh mlh)
	     (:return (values method uri version gh (append rh mlh) eh uh)))))

(defrule x-www-form-urlencoded? (query)
  (:query? query) (:return query))

;; (defrule http-request? (method uri version gh rh eh uh mlh)
;;   (:http-request-headers? method uri version gh rh eh uh mlh)
;;   (:if eh
;;        (:do (let ((content-type (cadr (assoc 'content-type eh))))
;; 	      (cond
;; 		((and (string= 'multipart (string-upcase (car content-type)))
;; 		      (string= 'form-data (string-upcase (cadr content-type))))
;; 		 (:) (values method uri version gh rh eh uh mlh))
;; 		((and (string= 'application (string-upcase (car content-type)))
;; 		      (string= 'x-www-form-urlencoded (string-upcase (cadr content-type))))
;; 		 ))))))
;;;-----------------------------------------------------------------------------
;;; HTTP RESPONSE
;;;-----------------------------------------------------------------------------
(defclass http-response (http-message)
  ((headers :accessor http-response.headers :initform '())
   (status-code :accessor http-response.status-code :initform '(200))))

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

(defun status-code! (stream response status-code &optional reason-phrase)
  (let ((status (assoc status *status-codes*)))
    (string! stream  "HTTP/")
    (version! stream (http-message.version response))
    (char! stream #\ )
    (fixnum! stream (if status (car status) status-code))
    (char! stream #\ )
    (string! stream (if status (cdr status) reason-phrase))
    (char! stream #\Newline)))

;;;-----------------------------------------------------------------------------
;;; MOD-LISP REQUEST
;;;-----------------------------------------------------------------------------
(defclass mod-lisp-request (http-request)
  ())

;;;-----------------------------------------------------------------------------
;;; MOD-LISP RESPONSE
;;;-----------------------------------------------------------------------------
(defclass mod-lisp-response (http-response)
  ())

;; ;;;-----------------------------------------------------------------------------
;; ;;; mOD-LISP HTTP mESSAGE
;; ;;;-----------------------------------------------------------------------------
;; (defclass mod-lisp-request-header ()
;;   ((protocol :initarg :protocol :initform nil)
;;    (url :initarg :url :initform url)))

;; (defatom mod-lisp-header-name? ()
;;   (visible-char? c))

;; (defatom mod-lisp-header-value? ()
;;   (or (visible-char? c) (space? c)))

;; (defrule mod-lisp-request-headers? (key val c (headers '()))   
;;   (:zom
;;    (:or (:checkpoint (:seq "end") (:return (nreverse headers)))
;; 	(:checkpoint
;; 	 (:sci "server-protocol") #\Newline (:http-protocol? val) #\Newline
;; 	 (:do (push (cons 'protocol val) headers)) (:commit))
;; 	(:checkpoint
;; 	 (:sci "url") #\Newline (:uri? val) #\Newline
;; 	 (:do (push (cons 'url val) headers)) (:commit))
;; 	(:checkpoint
;; 	 (:sci "method") #\Newline (:http-method? val) #\Newline
;; 	 (:do (push (cons 'method val) headers)) (:commit))
;; 	(:checkpoint
;; 	 (:sci "content-type") #\Newline (:http-content-type? key val) #\Newline
;; 	 (:do (push (list 'content-type key val) headers)) (:commit))
;; 	(:checkpoint
;; 	 (:sci "content-length") #\Newline (:number-value? val) #\Newline
;; 	 (:do (push (cons 'content-length val) headers)) (:commit))
;; 	(:checkpoint
;; 	 (:sci "server-ip-addr") #\Newline (:ipv4address? val) #\Newline
;; 	 (:do (push (cons 'server-ip-addr val) headers)) (:commit))
;; 	(:checkpoint
;; 	 (:sci "server-ip-port") #\Newline (:port? val) #\Newline
;; 	 (:do (push (cons 'server-ip-port val) headers)) (:commit))
;; 	(:checkpoint
;; 	 (:sci "remote-ip-addr") #\Newline (:ipv4address? val) #\Newline
;; 	 (:do (push (cons 'remote-ip-addr val) headers)) (:commit))
;; 	(:checkpoint
;; 	 (:sci "remote-ip-port") #\Newline (:port? val) #\Newline
;; 	 (:do (push (cons 'remote-ip-port val) headers)) (:commit))
;; 	(:checkpoint
;; 	 (:sci "script-filename") #\Newline (:do (setq val (make-accumulator :byte)))
;; 	 (:zom (:not #\Newline) (:type octet? c) (:collect c val))
;; 	 (:do (push (cons 'script-filename (octets-to-string val :utf-8))
;; 		    headers)) (:commit))
;; 	(:checkpoint
;; 	 (:sci "server-id") #\Newline (:do (setq val (make-accumulator)))
;; 	 (:zom (:type mod-lisp-header-value? c) (:collect c val)) #\Newline
;; 	 (:do (push (cons 'server-id val) headers)) (:commit))
;; 	(:checkpoint
;; 	 (:sci "user-agent") #\Newline (:http-user-agent? val) (:lwsp?)
;; 	 (:do (push (list 'user-agent val) headers)) (:commit))
;; 	(:checkpoint
;; 	 (:sci "host") #\Newline (:hostname? val) (:lwsp?)
;; 	 (:do (push (cons 'host val) headers)) (:commit))
;; 	(:checkpoint
;; 	 (:sci "accept") #\Newline (:http-accept? val) (:lwsp?)
;; 	 (:do (push (cons 'accept val) headers)) (:commit))
;; 	(:checkpoint
;; 	 (:sci "accept-language") #\Newline (:http-accept-language? val) (:lwsp?)
;; 	 (:do (push (cons 'accept-language val) headers)) (:commit))
;; 	(:checkpoint
;; 	 (:sci "accept-encoding") #\Newline (:http-accept-encoding? val) #\Newline
;; 	 (:do (push (cons 'accept-encoding val) headers)) (:commit))
;; 	(:checkpoint
;; 	 (:sci "accept-charset") #\Newline (:http-accept-charset? val) #\Newline
;; 	 (:do (push (cons 'accept-charset val) headers)) (:commit))
;; 	(:checkpoint
;; 	 (:sci "accept-ranges") (:lwsp?) (:http-ranges? val) (:lwsp?)
;; 	 (:do (push (cons 'accept-ranges val) headers)) (:commit))
;; 	(:checkpoint
;; 	 (:sci "age") (:lwsp?) (:http-age? val) (:lwsp?)
;; 	 (:do (push (cons 'age val) headers)) (:commit))
;; 	(:checkpoint
;; 	 (:sci "allow") (:lwsp?) (:http-allow? val) (:lwsp?)
;; 	 (:do (push (list 'allow val) headers)) (:commit))
;; 	(:checkpoint
;; 	 (:sci "keep-alive") #\Newline (:number-value? val) #\Newline
;; 	 (:do (push (cons 'keep-alive val) headers)) (:commit))
;; 	(:checkpoint
;; 	 (:sci "connection") #\Newline (:do (setq val (make-accumulator)))
;; 	 (:zom (:type mod-lisp-header-value? c) (:collect c val)) #\Newline
;; 	 (:do (push (cons 'connection val) headers)) (:commit))
;; 	(:checkpoint
;; 	 (:sci "referer") #\Newline (:uri? val) #\Newline
;; 	 (:do (push (cons 'referer val) headers)) (:commit))
;; 	(:checkpoint
;; 	 (:sci "server-baseversion") (:lwsp?)
;; 	 (:do (setq val (make-accumulator)))
;; 	 (:zom (:type http-header-value? c) (:collect c val))
;; 	 (:lwsp?)
;; 	 (:do (push (cons 'server-baseversion val) headers))
;; 	 (:commit))
;; 	(:checkpoint
;; 	 (:sci "modlisp-major-version") (:lwsp?)
;; 	 (:version? c) (:lwsp?)
;; 	 (:do (push (cons 'modlisp-major-version c) headers))
;; 	 (:commit))
;; 	(:checkpoint
;; 	 (:sci "modlisp-version") (:lwsp?)
;; 	 (:version? c) (:do (push (list 'modlisp-version c) headers))
;; 	 (:lwsp?) (:commit))
;; 	(:checkpoint
;; 	 (:sci "cache-control") (:lwsp?) (:http-cache-control? val) (:lwsp?)
;; 	 (:do (push (cons 'cache-control val) headers)) (:commit))
;; 	(:and
;; 	 (:do (setq key (make-accumulator)))
;; 	 (:zom (:type mod-lisp-header-name? c) (:collect c key))
;; 	 #\Newline
;; 	 (:do (setq val (make-accumulator :byte)))
;; 	 (:zom (:type mod-lisp-header-value? c) (:collect c val))
;; 	 (:do (push (cons key val) headers))
;; 	 #\Newline))))

;; (defvar +mod-lisp-request-headers+
;;   '("url" "content-type" "content-length" "method" "remote-ip-addr"
;;     "remote-ip-port" "server-ip-addr" "server-ip-port" "server-protocol"
;;     "script-filename" "ssl-session-id" "server-id" "server-baseversion"
;;     "modlisp-version" "modlisp-major-version" "lisp-content-length"))

;; (defclass mod-lisp-http-request (http-request)
;;   ())

;; (defun make-mod-lisp-request (stream)
;;   (flet ((2str (o &optional (charset :utf-8))
;; 	   (octets-to-string o charset)))
;;     (let ((headers (mod-lisp-request-headers? stream))
;; 	  (request (make-instance 'mod-lisp-http-request)))
;;       ;; (mapc #'(lambda (header)
;; ;; 		(cond
;; ;; 		  ((equal (car header) "method")
;; ;; 		   (setf (http-request.method request)
;; ;; 			 (intern (string-upcase (2str (cdr (assoc "method" headers :test #'equal))))
;; ;; 				 :keyword)))
;; ;; 		  (t
;; ;; 		   (push (cons (car header)
;; ;; 			       (2str (cdr header)))
;; ;; 			 (http-message.headers request)))))
;; ;; 	    headers)
;;       (setf (http-message.headers request)
;; 	    headers)
;;       request)))


(defvar *http-request*
  "POST /ee.gee HTTP/1.0
Host: localhost:3010
Accept: text/html, text/plain, application/vnd.sun.xml.writer, application/vnd.sun.xml.writer.global, application/vnd.stardivision.writer, application/vnd.stardivision.writer-global, application/x-starwriter, application/vnd.sun.xml.writer.template
Accept: application/msword, application/vnd.sun.xml.calc, application/vnd.stardivision.calc, application/x-starcalc, application/vnd.sun.xml.calc.template, application/excel, application/msexcel, application/vnd.ms-excel, application/x-msexcel
Accept: application/vnd.sun.xml.impress, application/vnd.stardivision.impress, application/vnd.stardivision.impress-packed, application/x-starimpress, application/vnd.sun.xml.impress.template, application/powerpoint, application/mspowerpoint
Accept: application/vnd.ms-powerpoint, application/x-mspowerpoint, application/vnd.sun.xml.draw, application/vnd.stardivision.draw, application/x-stardraw, application/vnd.sun.xml.draw.template, application/vnd.sun.xml.math
Accept: application/vnd.stardivision.math, application/x-starmath, text/sgml, */*;q=0.01
Accept-Encoding: gzip, compress
Accept-Language: en
Pragma: no-cache
Cache-Control: no-cache
User-Agent: Lynx/2.8.5rel.5 libwww-FM/2.14 SSL-MM/1.4.1 OpenSSL/0.9.7j
Content-type: multipart/form-data; boundary=LYNX
Content-length: 395

--LYNX
")

(defparameter *req-data* "gee=gee123&abuzer1=bas+bakalim&abuzer2=&abuzer3=&notparsed=gee&")

(defparameter *mod-lisp-req2*
  "server-protocol
HTTP/1.1
method
POST
url
/%C4%B1gee.gee
content-type
application/x-www-form-urlencoded
content-length
48
server-ip-addr
127.0.0.1
server-ip-port
80
remote-ip-addr
127.0.0.1
script-filename
/var/www/localhost/htdocs/gee.gee
remote-ip-port
50692
server-id
evrim
User-Agent
Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.8.1) Gecko/20061010 Firefox/2.0
Host
localhost
Accept
text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5
Accept-Language
tr,en;q=0.7,en-us;q=0.3
Accept-Encoding
gzip,deflate
Accept-Charset
ISO-8859-9,utf-8;q=0.7,*;q=0.7
Keep-Alive
300
Connection
keep-alive
Cache-Control
max-age=0
Content-Type
application/x-www-form-urlencoded
Content-Length
48
end
gee=gee123&abuzer1=bas+bakalim&abuzer2=&abuzer3=")

(defparameter *mod-lisp-req*
  "server-protocol
HTTP/1.1
method
POST
url
/%C4%B1gee.gee
content-type
application/x-www-form-urlencoded
content-length
48
server-ip-addr
127.0.0.1
server-ip-port
80
remote-ip-addr
127.0.0.1
script-filename
/var/www/localhost/htdocs/gee.gee
remote-ip-port
50692
server-id
evrim
server-baseversion
Apache/2.0.58
modlisp-version
1.3.1
modlisp-major-version
2
User-Agent
Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.8.1) Gecko/20061010 Firefox/2.0
Host
localhost
Accept
text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5
Accept-Language
tr,en;q=0.7,en-us;q=0.3
Accept-Encoding
gzip,deflate
Accept-Charset
ISO-8859-9,utf-8;q=0.7,*;q=0.7
Keep-Alive
300
Connection
keep-alive
Cache-Control
max-age=0
Content-Type
application/x-www-form-urlencoded
Content-Length
48
end
gee=gee123&abuzer1=bas+bakalim&abuzer2=&abuzer3=")

(defparameter *multipart/form-data*
  "-----------------------------180841493794515451098915474
Content-Disposition: form-data; name=\"ıgee\"

gee123ýþi
-----------------------------180841493794515451098915474
Content-Disposition: form-data; name=\"abuzer1\"

bas bakalim
-----------------------------180841493794515451098915474
Content-Disposition: form-data; name=\"abuzer2\"; filename=\"a.html\"
Content-Type: text/html

<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html
  ><body
      ><form action=\"http://localhost/ıgee.gee\" method=\"POST\"
	  enctype=\"multipart/form-data\" 
	        ><input name=\"ıgee\" type=\"text\" value=\"gee123\"
			      /><input type=\"submit\" name=\"abuzer1\" value=\"bas bakalim\">
				  <input type=\"file\" name=\"abuzer2\">
				  <input type=\"file\" name=\"abuzer3\">
				  </form
				      ></body
					    ></html
						>

-----------------------------180841493794515451098915474
Content-Disposition: form-data; name=\"abuzer3\"; filename=\"\"
Content-Type: application/octet-stream


-----------------------------180841493794515451098915474--
")

(defparameter *ie-mod-lisp-headers* "server-protocol
HTTP/1.1
method
GET
url
/eben.gee
server-ip-addr
10.0.0.10
server-ip-port
80
remote-ip-addr
10.0.0.11
script-filename
/var/www/localhost/htdocs/eben.gee
remote-ip-port
1035
server-id
evrim
server-baseversion
Apache/2.0.58
modlisp-version
1.3.1
modlisp-major-version
2
Accept
image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, application/x-shockwave-flash, */*
Accept-Language
tr
Accept-Encoding
gzip, deflate
Accept-Charset
iso-8859-5, unicode-1-1;q=0.8
Accept-Ranges
bytes
Age
341274938794237
Allow
GET, HEAD, PUT
User-Agent
Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.0)
Host
10.0.0.10
Connection
Keep-Alive
end
")

(defvar *ie-http-headers* "GET / HTTP/1.1
Accept: image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, application/x-shockwave-flash, */*
Accept-Language: tr
Accept-Encoding: gzip, deflate
User-Agent: Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.0)
Host: 10.0.0.10:3011
Connection: Keep-Alive

")

(defparameter *ie-http-form* "POST /ee.gee HTTP/1.1
Accept: image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, application/x-shockwave-flash, */*
Referer: http://10.0.0.10/a.html
Accept-Language: tr
Content-Type: multipart/form-data; boundary=---------------------------7d728030200f0
Accept-Encoding: gzip, deflate
User-Agent: Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.0)
Host: 10.0.0.10:3010
Content-Length: 646
Connection: Keep-Alive
Cache-Control: no-cache   

                                       
-----------------------------7d728030200f0
Content-Disposition: form-data; name=\"gee\"
                                       
gee123                                 
-----------------------------7d728030200f0
Content-Disposition: form-data; name=\"abuzer1\"
                                       
bas bakalim                            
-----------------------------7d728030200f0
Content-Disposition: form-data; name=\"abuzer2\"; filename=\"C:\Documents and Settings\Administrator\Desktop\a.txt\"              
Content-Type: text/plain               
                                       
ayim ben                               
-----------------------------7d728030200f0
Content-Disposition: form-data; name=\"abuzer3\"; filename=\"C:\Documents and Settings\Administrator\Desktop\b.txt\"              
Content-Type: text/plain               
                                       
beyim ben                              
-----------------------------7d728030200f0--
")

(defvar *ie-modlisp-form* "server-protocol
HTTP/1.1
method
POST
url
/eeg.gee
content-type
multipart/form-data; boundary=---------------------------7d72ec11200f0
content-length
644
server-ip-addr
10.0.0.10
server-ip-port
80
remote-ip-addr
10.0.0.11
script-filename
/var/www/localhost/htdocs/eeg.gee
remote-ip-port
1056
server-id
evrim
server-baseversion
Apache/2.0.58
modlisp-version
1.3.1
modlisp-major-version
2
Accept
image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, application/x-shockwave-flash, */*
Referer
http://10.0.0.10/a.html
Accept-Language
tr
Content-Type
multipart/form-data; boundary=---------------------------7d72ec11200f0
Accept-Encoding
gzip, deflate
User-Agent
Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.0)
Host
10.0.0.10
Content-Length
644
Connection
Keep-Alive
Cache-Control
no-cache
end
-----------------------------7d72ec11200f0
Content-Disposition: form-data; name=\"gee\"

gee123
-----------------------------7d72ec11200f0
Content-Disposition: form-data; name=\"abuzer1\"

bas bakalim
-----------------------------7d72ec11200f0
Content-Disposition: form-data; name=\"abuzer2\"; filename=\"C:\Documents and Settings\Administrator\Desktop\a.txt\"
Content-Type: text/plain

ayim ben
-----------------------------7d72ec11200f0
Content-Disposition: form-data; name=\"abuzer3\"; filename=\"C:\Documents and Settings\Administrator\Desktop\b.txt\"
Content-Type: text/plain

beyim ben
-----------------------------7d72ec11200f0--
")