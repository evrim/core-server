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

;;;-----------------------------------------------------------------------------
;;; RFC 2396 - Uniform Resource Identifiers (URI): Generic Syntax
;;; http://www.ietf.org/rfc/rfc2396.txt
;;;-----------------------------------------------------------------------------

;; A. Collected BNF for URI
;;

;;       mark          = "-" | "_" | "." | "!" | "~" | "*" | "'" | "(" | ")"
(defatom mark? ()
  (if (member c '#.(mapcar #'char-code '(#\- #\_ #\. #\! #\~ #\* #\' #\( #\))))
      t))

;;       unreserved    = alphanum | mark
(defatom unreserved? ()
  (or (alphanum? c) (mark? c)))

;;       reserved      = ";" | "/" | "?" | ":" | "@" | "&" | "=" | "+" |
;;                       "$" | ","
(defatom reserved? ()
  (if (member c '#.(mapcar #'char-code '(#\; #\/ #\? #\: #\@ #\& #\= #\+ #\$ #\,)))
      t))

;;       uric          = reserved | unreserved | escaped
(defrule uric? (c)
  (:or (:and (:type reserved? c) (:return c))
       (:and (:type unreserved? c) (:return c))
       (:escaped? c) (:return c)))

;;       query         = *uric
;; NOTE: this rule parses queries and return a list of key value cons

(defrule query-key? (c (key (make-accumulator)))  
  (:oom (:or (:escaped? c) (:type unreserved? c))
	(:collect c key))
  (:return key))

(defrule query-value? (c (val (make-accumulator :byte)))
  (:oom (:or (:escaped? c) (:type unreserved? c)	     
	     (:and #\+ (:do (setq c #.(char-code #\Space)))))
	(:collect c val))
  (:return (octets-to-string val :utf-8)))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defrule query? (key val c (queries '()))
    (:oom (:query-key? key)
	  (:type reserved?)
	  (:query-value? val)
	  (:do (push (cons key val) queries))
	  (:checkpoint (:type reserved?) (:commit)))
    (:return (nreverse queries))))

;;       fragment      = *uric
(defrule fragment? (q)
  (:query? q) (:return q))

;;       scheme        = alpha *( alpha | digit | "+" | "-" | "." )
(defatom scheme-specials? ()
  (and (member c '#.(mapcar #'char-code '(#\+ #\- #\.)))
       t))

(defrule scheme? ((scheme (make-accumulator)) c)  
  (:type alpha? c)
  (:collect c scheme)
  (:zom (:type (or alpha? digit? scheme-specials?) c)
	(:collect c scheme))
  (:return scheme))

;;       pchar         = unreserved | escaped | ":" | "@" | "&" | "=" | "+" | "$" | ","

;; FIX: added #\Space in order to parse
;; file://bla bla/Document and Settings/ although
;; it should be Document%20and%20Settings
;; - This is plain bullshit -evrim.
(defatom pchar-special? ()
  (if (member c '#.(mapcar #'char-code '(#\: #\@ #\& #\= #\+ #\$ #\,
					 ;; #\Space
					 )))
      t))

(defrule pchar? (c)
  (:or (:and (:type unreserved? c) (:return c))
       (:and (:escaped? c) (:return c))
       (:and (:type pchar-special? c) (:return c))))

;;       param         = *pchar
(defrule param? ((q (make-accumulator)) c)  
  (:zom (:pchar? c) (:collect c q))
  (:if (> (length q) 0) (:return q)))

;;       segment       = *pchar *( ";" param )
(defrule segment? ((seg (make-accumulator)) param (params '()))
  (:param? seg)
  (:zom #\; (:and (:param? param) (:do (push param params))))
  (:return (values seg params)))

;;       path_segments = segment *( "/" segment )
(defrule path-segments? ((segments '()) segment params)
  (:segment? segment params)
  (:do (push (cons segment params) segments))
  (:zom #\/
	(:segment? segment params)
	(:do (push (cons segment params) segments)))
  (:return (nreverse segments)))

;;       reg_name      = 1*( unreserved | escaped | "$" | "," |
;;                                ";" | ":" | "@" | "&" | "=" | "+" )
(defatom reg_name-specials? ()
  (and (member c '#.(mapcar #'char-code '(#\$ #\, #\; #\: #\@ #\& #\= #\+)))
       t))

(defrule reg_name? ((acc (make-accumulator)) c)
  (:oom (:or (:and (:type (or unreserved? reg_name-specials?) c)
		   (:collect c acc))
	     (:and (:escaped? c)
		   (:collect c acc))))
  (:if (not (null acc))
       (:return acc)))

;;       rel_segment   = 1*( unreserved | escaped |
;;                           ";" | "@" | "&" | "=" | "+" | "$" | "," )
(defatom rel_segment-specials? ()
  (and (member c '#.(mapcar #'char-code '(#\; #\@ #\& #\= #\+ #\$ #\,)))
       t))

(defrule rel_segment? ((segment (make-accumulator)) c)  
  (:or (:and (:type (or unreserved? rel_segment-specials?) c)
	     (:collect c segment))
       (:and (:escaped? c)
	     (:collect c segment)))
  (:zom (:or (:and (:type (or unreserved? rel_segment-specials?) c)
		   (:collect c segment))
	     (:and (:escaped? c)
		   (:collect c segment))))
  (:return segment))

;;       IPv4address   = 1*digit "." 1*digit "." 1*digit "." 1*digit
(defrule ipv4address? (d1 d2 d3 d4)
  (:fixnum? d1) #\. (:fixnum? d2) #\.
  (:fixnum? d3) #\. (:fixnum? d4)
  (:if (and (< d1 255) (< d2 255) (< d3 255) (< d4 255))
       (:return (make-array 4
			    :element-type '(unsigned-byte 8)
			    :initial-contents (list d1 d2 d3 d4)))))

(defun ipv4address! (stream lst)
  (reduce #'(lambda (acc i)
	      (declare (ignore acc))
	      (fixnum! stream i))
	  lst :initial-value nil))

;;       port          = *digit
(defrule port? (d)
  (:fixnum? d)
  (:if (and (>= d 0) (< d 65536)) (:return d)))

(defun port! (stream p)
  (fixnum! stream p))

;;       toplabel      = alpha | alpha *( alphanum | "-" ) alphanum
;; FIXmE: Toplabel can't end with a #\-
;; Fixed: first char can be a num, not just alpha, bnf was wrong! -evrim.
(defrule toplabel? ((label (make-accumulator)) a)  
  (:type alphanum? a) (:collect a label)
  (:zom (:or (:and (:type alphanum? a) (:collect a label))
	     (:and #\- (:collect #\- label))))
  (:return label))

;;       domainlabel   = alphanum | alphanum *( alphanum | "-" ) alphanum
(defrule domainlabel? (label)
  (:toplabel? label) (:return label))

;;       hostname      = *( domainlabel "." ) toplabel [ "." ]
(defrule hostname? (label (labels '()))
  (:zom (:and (:domainlabel? label)
	      (:do (push label labels))
	      (:and #\. (:do (push #\. labels)))))
  (:if (> (length labels) 0)
       (:return (format nil "~{~A~}" (nreverse labels)))))

;;       host          = hostname | IPv4address
(defrule host? (host)
  (:or (:and (:ipv4address? host) (:return host))
       (:and (:hostname? host) (:return host))))

(defun host! (stream host)
  (typecase host
    (list (ipv4address! stream host))
    (string (string! stream host))))

;;       hostport      = host [ ":" port ]
(eval-when (:compile-toplevel :load-toplevel :execute) 
  (defrule hostport? (host port)
    (:or (:checkpoint
	  (:host? host) #\: (:port? port) (:return (cons host port)))
	 (:and (:host? host) (:return (cons host nil))))))

(defun hostport! (stream hp-cons)
  (host! stream (car hp-cons))
  (when (cdr hp-cons)
    (char! stream #\:)
    (port! stream (cdr hp-cons))))

;;       userinfo      = *( unreserved | escaped | ";" | ":" | "&" | "=" | "+" | "$" | "," )
(defatom userinfo-specials? ()
  (and (member c '#.(mapcar #'char-code '(#\; #\: #\& #\= #\+ #\$ #\,))) t))

(defrule userinfo? (c (q (make-accumulator)) (userinfo '()))
  (:oom (:oom (:or (:type unreserved? c) (:escaped? c))
	      (:collect c q))
	(:do (push q userinfo)
	     (setq q (make-accumulator)))
	(:type userinfo-specials?))
  (:return (nreverse userinfo)))

;;       server        = [ [ userinfo "@" ] hostport ]
(defrule server? (userinfo hostport)
  (:or (:checkpoint
	(:userinfo? userinfo) #\@
	(:hostport? hostport)
	(:return (list (car hostport) (cdr hostport) userinfo)))
       (:and (:hostport? hostport)
	     (:return (list (car hostport) (cdr hostport) nil)))))

;;       authority     = server | reg_name
(defrule authority? (authority)
  (:or (:and (:server? authority) (:return authority))
       (:and (:reg_name? authority) (:return (list authority)))))

;;       abs_path      = "/"  path_segments
(defrule abs_path? (abs-path)
  #\/
  (:or (:and (:path-segments? abs-path) (:return abs-path))
       (:return (cons '("") nil))))

;;       rel_path      = rel_segment [ abs_path ]
(defrule rel_path? (segment path)
  (:rel_segment? segment)  
  (:checkpoint
   (:abs_path? path)
   (:commit))
  (:return (cons (list segment) path)))

;;       net_path      = "//" authority [ abs_path ]
(defrule net_path? (authority path)  
  #\/ #\/
  (:or (:and (:authority? authority)
	     (:abs_path? path)
	     (:return (values path authority)))
       (:and (:abs_path? path)
	     (:return (values path nil)))))

;;       uric_no_slash = unreserved | escaped | ";" | "?" | ":" | "@" |
;;                       "&" | "=" | "+" | "$" | ","
(defatom uric_no_slash-special? ()
  (and (member c '#.(mapcar #'char-code '(#\; #\? #\: #\@ #\& #\= #\+ #\$ #\,)))
       t))

(defrule uric_no_slash? (c)
  (:or (:and (:type (or unreserved? uric_no_slash-special?) c)
	     (:return c))
       (:and (:escaped? c) (:return c))))

;;       opaque_part   = uric_no_slash *uric
(defrule opaque_part? (c (part (make-accumulator)))
  (:uric_no_slash? c) (:collect c part)
  (:zom (:and (:uric? c) (:collect c part)))
  (:return part))

;; RFC BUG, unused.
;;       path          = [ abs_path | opaque_part ]
(defrule path? (path)
  (:or (:and (:abs_path? path) (:return path))
       (:and (:opaque_part? path) (:return (list path)))))

;;       hier_part     = ( net_path | abs_path ) [ "?" query ]
;; TODO: Fix parser -evrim
;; http://www.core.gen.tr/#index
;; http://www.core.gen.tr/?#index
;; http://www.core.gen.tr/?eben#index
;; file:///C:/Documents and Settings/evr'm/Desktop/perplex/index2.html
(defrule hier_part? (val path authority query fragment)
  (:or (:net_path? path authority)
       (:abs_path? path))  
  (:zom (:or (:and #\? (:query? val) (:do (setq query val)))
	     (:and #\# 
		   (:or (:query? val) (:query-key? val))
		   (:do (setq fragment val)))))
  (:return (values path query authority fragment)))

;;       relativeURI   = ( net_path | abs_path | rel_path ) [ "?" query ]
(defrule relative-uri? (path query authority)
  (:or (:net_path? path authority) (:abs_path? path) (:rel_path? path))
  (:or (:and #\? (:query? query) (:return (values path query authority)))
       (:return (values path nil authority))))

;;       absoluteURI   = scheme ":" ( hier_part | opaque_part )
(defrule absolute-uri? (scheme path query authority fragment)
  (:scheme? scheme)
  #\:
  (:or (:and (:hier_part? path query authority fragment)
	     (:return (values scheme authority path query fragment)))
       (:and (:opaque_part? path)
	     (:return (values scheme nil (list path) nil nil)))))

(defclass uri ()
  ((scheme :accessor uri.scheme :initarg :scheme :initform nil)
   (username :accessor uri.username :initarg :username :initform nil)
   (password :accessor uri.password :initarg :password :initform nil)
   (server :accessor uri.server :initarg :server :initform nil)
   (port :accessor uri.port :initarg :port :initform nil)   
   (paths :accessor uri.paths :initarg :paths :initform nil)
   (queries :accessor uri.queries :initarg :queries :initform nil) 
   (fragments :accessor uri.fragments :initarg :fragments :initform nil)))

(defprint-object (self uri :identity t :type t)
  (with-slots (scheme username password server
	       port paths queries fragments) self
    (format t "~A://" scheme)
    (if username (format t "~A:~A@" username password))
    (format t "~A" server)
    (if port (format t ":~A" port))
    (format t "~{/~A~}?~{~A~}#~{~A~}" paths (ensure-list queries)
	    (ensure-list fragments))))

(defgeneric urip (uri)
  (:method ((uri uri)) t)
  (:method ((uri t)) t))

(defmethod uri.query ((uri uri) name)
  (cdr (assoc name (uri.queries uri) :test #'equal)))

(defun make-uri (&key scheme username password server port paths queries
		      fragments)
  (make-instance 'uri
		 :scheme scheme
		 :server server
		 :port port
		 :username username
		 :password password
		 :paths paths
		 :queries queries
		 :fragments fragments))

;;       URI-reference = [ absoluteURI | relativeURI ] [ "#" fragment ]
(defrule uri? (fragment scheme path query authority)
  (:or (:absolute-uri? scheme authority path query fragment)
       (:relative-uri? path query authority))
  (:return (make-uri
	    :scheme scheme
	    :server (car authority)
	    :port (cadr authority)
	    :username (car (caddr authority))
	    :password (cadr (caddr authority))
	    :paths path
	    :queries query
	    :fragments fragment)))

(defconstant +uri-path-seperator+ #\/)
(defconstant +uri-segment-seperator+ #\;)
(defconstant +uri-query-equal-seperator+ #\=)
(defconstant +uri-query-seperator+ #\&)
(defun query! (stream query)
  (string! stream (escape-as-uri (car query)))
  (char! stream +uri-query-equal-seperator+)
  (string! stream (escape-as-uri (cdr query))))

(defmethod uri! ((stream core-stream) (uri string))
  (string! stream uri))

(defmethod uri! ((stream core-stream) (uri uri))
  (prog1 stream
    (with-slots (scheme username password server port paths queries fragments) uri
      (when (and scheme server)
	(string! stream scheme)
	(string! stream "://")
	(when username
	  (string! stream username)
	  (when password
	    (char! stream #\:)
	    (string! stream password))
	  (char! stream #\@))
	(string! stream server)
	(when port
	  (char! stream #\:)
	  (fixnum! stream port)))
      (mapc #'(lambda (path)
		(char! stream +uri-path-seperator+)
		(string! stream (car path))
		(mapc #'(lambda (path)
			  (char! stream +uri-segment-seperator+)
			  (string! stream path))
		      (cdr path)))
	    paths)
      (when (car queries)
	(char! stream #\?)
	(query! stream (car queries))
	(mapc #'(lambda (query)
		  (char! stream +uri-query-seperator+)
		  (query! stream query))
	      (cdr queries)))
      (when (car fragments)
	(char! stream #\#)
	(query! stream (car fragments))
	(mapcar #'(lambda (fragment)
		    (char! stream +uri-query-seperator+)
		    (query! stream fragment))
		(cdr fragments))))))

(defmethod relative-uri! ((stream core-stream) (uri uri))
  (prog1 stream
    (with-slots (paths queries fragments) uri
      (mapc #'(lambda (path)
		(char! stream +uri-path-seperator+)
		(string! stream (car path))
		(mapc #'(lambda (path)
			  (char! stream +uri-segment-seperator+)
			  (string! stream path))
		      (cdr path)))
	    paths)
      (when (car queries)
	(char! stream #\?)
	(query! stream (car queries))
	(mapc #'(lambda (query)
		  (char! stream +uri-query-seperator+)
		  (query! stream query))
	      (cdr queries)))
      (when (car fragments)
	(char! stream #\#)
	(query! stream (car fragments))
	(mapcar #'(lambda (fragment)
		    (char! stream +uri-query-seperator+)
		    (query! stream fragment))
		(cdr fragments))))))

(defun uri->string (uri)
  (with-core-stream (s "")
    (uri! s uri)
    (return-stream s)))

(defparameter +uri-parsers+
  '(query-key? query-value? query? uric? fragment? scheme-specials? scheme?
    pchar-special? pchar? param? segment? path-segments? reg_name-specials?
    reg_name? rel_segment-specials? rel_segment? ipv4address? port? toplabel?
    domainlabel? hostname? host? hostport? userinfo-specials? userinfo? server?
    authority? abs_path? rel_path? net_path? uric_no_slash-special? uric_no_slash?
    opaque_part? path? hier_part? relative-uri? absolute-uri? uri?))

(defun trace-uri-parsers ()
  (mapcar #'(lambda (parser)
	      (eval `(trace ,parser))) +uri-parsers+))

(defun untrace-uri-parsers ()
  (mapcar #'(lambda (parser)
	      (eval `(untrace ,parser)))
	   +uri-parsers+))

#|
(defparameter *u (make-uri :scheme "http" :username "evrim"
			   :password "password"
			   :server "nodeN.core.gen.tr"
			   :port 80
			   :paths '(("a") ("b") ("c"))
			   :queries (list (cons "a" "1")
					  (cons "b" "2"))
			   :fragments (list (cons "x" "1")
					    (cons "y" "2"))))
(describe *u)
(defparameter *s (make-core-stream ""))
(time (uri! *s *u))
(format t "~A" (octets-to-string (slot-value *s '%octets) :utf-8))
(defparameter *c (uri? (make-core-stream "http://evrim:password@nodeN.core.gen.tr:80/a/b/c?a:1;b:2#x:1;y:2")))
(describe *c)
#<URI {AC46169}> is an instance of class #<STANDARD-CLASS URI>.
The following slots have :INSTANCE allocation:
 SCHEME       "http"
 USERNAME     "evrim"
 PASSWORD     "password"
 SERVER       "nodeN.core.gen.tr"
 PORT         80
 PATHS        (("a") ("b") ("c"))
 QUERIES      (("a" . "1") ("b" . "2"))
 FRAGMENTS    (("x" . "1") ("y" . "2"))
|#