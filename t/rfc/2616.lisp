(in-package :tr.gen.core.server.test)

(deftest http-media-range?
    (with-core-stream (s "text/html; q=0.8; a=test;level=2")
      (multiple-value-list (core-server::http-media-range? s)))
  ("text" "html" (("level" . "2") ("a" . "test") ("q" . 0.8))))

;;;
;;; test request headers
;;;

;; ACCEPT
(deftest http-accept?
    (with-core-stream (s "text/*;q=0.3, text/html;q=0.7, text/html;level=1, text/html;level=2;q=0.4, */*;q=0.5")
      (http-accept? s))
  (("*" "*" (("q" . 0.5)))
   ("text" "html" (("q" . 0.4) ("level" . "2")))
   ("text" "html" (("level" . "1")))
   ("text" "html" (("q" . 0.7)))
   ("text" "*" (("q" . 0.3)))))

;; ACCEPT-CHARSET
(deftest http-accept-charset?
    (with-core-stream (s "iso-8859-5, unicode-1-1;q=0.8")
      (http-accept-charset? s))
  (("iso-8859-5" . 1.0) ("unicode-1-1" . 0.8)))

;; ACCEPT-ENCODING
(deftest http-accept-encoding?
    (with-core-stream (s "gzip;q=1.0, identity; q=0.5, *;q=0")
      (http-accept-encoding? s))
  (("gzip" . 1.0) ("identity" . 0.5) ("*" . 0.0)))

;; ACCEPT-LANGUAGE
(deftest http-accept-language?
    (with-core-stream (s "tr, en-us;q=0.1, en;q=0.7")
      (http-accept-language? s))
  (("tr" . 1.0) ("en-us" . 0.1) ("en" . 0.7)))

;; AUTHORIZATION
(deftest http-authorization?
    (with-core-stream (s "Digest mac=\"asd\",
cam=\"dsa\"")
      (http-authorization? s))
  ("Digest" ("cam" . "dsa") ("mac" . "asd")))

;; EXPECT
(deftest http-expect?
    (with-core-stream (s "asd=asd;q=324")
      (http-expect? s))
  (("asd" . "asd") ("q" . "324")))

(deftest http-expect-100-continue?    
    (with-core-stream (s "100-continue")
      (http-expect? s))
  core-server::100-continue)

;; FROM
(deftest http-from?
    (with-core-stream (s "<bill.gates@microsoft.com>")
      (slot-value (http-from? s) 'core-server::addr))
  "bill.gates@microsoft.com")

(deftest http-host?
    (with-core-stream (s "www.core.gen.tr:80")
      (http-host? s))
  ("www.core.gen.tr" . 80))

;; IF-MATCH
(deftest http-etag?
    (and (with-core-stream (s "W/\"testmeup\"")
	   (equal (core-server::http-etag? s)
		  '("testmeup" . t)))
	 (with-core-stream (s "\"testmeup\"")
	   (equal (core-server::http-etag? s)
		  '("testmeup"))))
  t)

(deftest http-if-match?
    (and (with-core-stream (s "*")
	   (equal (http-if-match? s)
		  '(*)))
	 (with-core-stream (s "W/\"test\"")
	   (equal (http-if-match? s)
		  '(("test" . t))))
	 (with-core-stream (s "\"test\", W/\"me\", W/\"up\"")
	   (equal (http-if-match? s)
		  '(("test") ("me" . T) ("up" . T)))))
  t)

;; IF-MODIFIED-SINCE
(deftest http-if-modified-since?
    (with-core-stream (s "Sat, 29 Oct 1994 19:43:31 GMT")
      (http-if-modified-since? s))
  2992448611)

;; IF-NONE-MATCH
(deftest http-if-none-match?
    (and (with-core-stream (s "*")
	   (equal (http-if-none-match? s)
		  '(*)))
	 (with-core-stream (s "\"xyzzy\", \"r2d2xxxx\", \"c3piozzzz\"")
	   (equal (http-if-none-match? s)
		  '(("xyzzy") ("r2d2xxxx") ("c3piozzzz"))))
	 (with-core-stream (s "W/\"test\",W/\"meplease\"")
	   (equal (http-if-none-match? s)
		  '(("test" . t) ("meplease" . t))))
	 (with-core-stream (s "\"test\", W/\"me\", W/\"up\"")
	   (equal (http-if-none-match? s)
		  '(("test") ("me" . T) ("up" . T)))))
  t)

;; IF-RANGE
(deftest http-if-range?
    (with-core-stream (s "Tue, 01 Feb 2008 04:20:00 GMT")
      (http-if-range? s))
  3410828400)

(deftest http-if-range2?
    (with-core-stream (s "W/\"ranger\"")
      (http-if-range? s))
  ("ranger" . t))

;; IF-UNMODIFIED-SINCE
(deftest http-if-unmodified-since
    (with-core-stream (s "Tue, 01 Feb 2008 04:20:00 GMT")
      (http-if-unmodified-since? s))
  3410828400)

;; MAX-FORWARDS
(deftest http-max-forwards?
    (with-core-stream (s "414")
      (equal (http-max-forwards? s)
	     414))
  t)

;; PROXY-AUTHORIZATION
(deftest http-proxy-authorization?    
    (with-core-stream (s "digest mac=\"0E:F0:FF:FF:FF:00\",
cam=\"foo\"")
      (equal (http-proxy-authorization? s)
	     '("digest" . ("mac" . "0E:F0:FF:FF:FF:00")))) 
  t)

;; RANGE
(deftest http-range?
    (with-core-stream (s "bytes=300-400,-300,300-")
      (equal (http-range? s)
	     '("bytes" (300) (NIL . 300) (300 . 400))))
  t)

;; REFERER
(deftest http-referer?
  (with-core-stream (s "http://www.w3.org/hypertext/DataSources/Overview.html")
    (let ((uri (http-referer? s)))
      (and (equal (uri.scheme uri) "http")
	   (equal (uri.server uri) "www.w3.org")
	   (equal (uri.paths uri)
		  '(("hypertext") ("DataSources") ("Overview.html"))))))
  t)

;; TE
(deftest http-te?
    (with-core-stream (s "trailers, deflate;q=0.3;asd=\"val\"")
      (equal (http-te? s)
	     '(("trailers")
	       ("deflate" ("asd" . "val") ("q" . "0.3")))))
  t)

;; USER-AGENT
(deftest http-user-agent-opera?
    (with-core-stream (s "Opera/9.21 (X11; Linux i686; U; en)")
      (http-user-agent? s))
  ((BROWSER . OPERA) (VERSION (9 21)) (OS . ("X11" "Linux i686")) (LANG . "en")))

(deftest http-user-agent-seamonkey?
    (with-core-stream (s "Mozilla/5.0 (X11; U; Linux i686; tr-TR; rv:1.8.1.2) Gecko/20070511 SeaMonkey/1.1.1")
      (http-user-agent? s))
  ((BROWSER . SEAMONKEY) (MOZ-VER (5 0)) (OS . "Linux i686") (REVISION (1 8 1 2)) (VERSION (1 1 1))))

(deftest http-user-agent-ie?    
    (with-core-stream (s "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV1; .NET CLR 1.1.4322; .NET CLR 2.0.50727)")
      (http-user-agent? s))
  ((BROWSER . IE) (MOZ-VER (4 0)) (VERSION (6 0)) (OS . "Windows NT 5.1")))

;;;
;;; test response headers
;;;

;; ACCEPT-RANGES
(deftest http-accept-ranges!
    (with-core-stream (s "")
      (http-accept-ranges! s "bytes")
      (equal (return-stream s)
	     "bytes"))
  t)

;; AGE
(deftest http-age!
    (with-core-stream (s "")
      (http-age! s 12)
      (equal (return-stream s)
	     "12"))
  t)

;; ETAG
(deftest http-etag!
    (and (with-core-stream (s "")
	   (http-etag! s '("test" . t))
	   (equal (return-stream s)
		  "W/\"test\""))
	 (with-core-stream (s "")
	   (http-etag! s '("wotest"))
	   (equal (return-stream s)
		  "\"wotest\"")))
  t)

;; LOCATION
(deftest http-location!
    (with-core-stream (s "")
      (http-location! s (make-uri :scheme "http"
				  :username "john"
				  :password "foo"
				  :server "127.0.0.1"
				  :port 8080
				  :paths '(("test") ("me") ("up.html"))))
      (equal (return-stream s)
	     "http://john:foo@127.0.0.1:8080/test/me/up.html"))
  t)

;; PROXY-AUTHENTICATE
(deftest http-proxy-authenticate!
    (with-core-stream (s "")
      (http-proxy-authenticate! s '("Digest" . (("attribute1" . "value1") ("attr2" . "val2"))))
      (equal (return-stream s)
	     "Digest attribute1=\"value1\",
attr2=\"val2\""))
  t)

;; RETRY-AFTER
(deftest http-retry-after!
    (with-core-stream (s "")
      (http-retry-after! s (encode-universal-time 0 20 6 1 1 2008))
      (equal (return-stream s)
	     "Tue, 01 Feb 2008 04:20:00 GMT"))
  t)

;; SERVER
(deftest http-server!
    (with-core-stream (s "")
      (http-server! s "(CORE-SERVER . (0 2))")
      (equal (return-stream s)
	     "(CORE-SERVER . (0 2))"))
  t)

;; VARY
(deftest http-vary!
    (and (with-core-stream (s "")
	   (http-vary! s '())
	   (equal (return-stream s) "*"))
	 (with-core-stream (s "")
	   (http-vary! s '("Free" "Software"))
	   (equal (return-stream s) "Free,Software")))
  t)

;; WWW-AUTHENTICATE
(deftest http-www-authenticate!
    (with-core-stream (s "")
      (http-www-authenticate! s '("digest" . (("mac" . "0E:F0:FF:FF:FF:00") ("cam" . "foo"))))
      (equal (return-stream s)
	     "digest mac=\"0E:F0:FF:FF:FF:00\",
cam=\"foo\""))
  t)

;;;
;;; test general-headers
;;;

;; CACHE-CONTROL
(deftest http-cache-control-no-cache?
    (with-core-stream (s "no-cache")
      (http-cache-control? s))
  ((NO-CACHE)))

(deftest http-cache-control-max-age?
    (with-core-stream (s "max-age=3600")
      (http-cache-control? s))
  ((MAX-AGE . 3600)))

(deftest http-cache-control-no-store?
    (with-core-stream (s "no-store")
      (http-cache-control? s))
  ((NO-STORE)))

(deftest http-cache-control-public!
    (with-core-stream (s "")
      (http-cache-control! s 'PUBLIC)
      (return-stream s))
  "public")

(deftest http-cache-control-no-cache!
    (with-core-stream (s "")
      (http-cache-control! s '((NO-CACHE . ("extension" . "value"))))
      (return-stream s))
  "no-cache,extension=\"value\"")

(deftest http-cache-control-private!
    (with-core-stream (s "")
      (http-cache-control! s '((PRIVATE . ("here" . "Iam"))))
      (return-stream s))
  "private,here=\"Iam\"")

;; CONNECTION
(deftest http-connection?
    (with-core-stream (s "close")
      (http-connection? s))
  ("close"))

(deftest http-connection!
    (with-core-stream (s "")
      (http-connection! s 'CLOSE)
      (return-stream s))
  "close")

;; DATE
(deftest http-date?
    (with-core-stream (s "Tue, 01 Feb 2008 04:20:00 GMT")
      (http-date? s))
  3410828400)

(deftest http-date!
    (with-core-stream (s "")
      (http-date! s (encode-universal-time 0 20 6 1 1 2008))
      (return-stream s))
  "Tue, 01 Feb 2008 04:20:00 GMT")

;; PRAGMA
(deftest http-pragma-no-cache?
    (with-core-stream (s "no-cache")
      (http-pragma? s))
  (NO-CACHE))

(deftest http-pragma-key-val?
    (with-core-stream (s "name=val")
      (http-pragma? s))
  ("name" . "val"))

(deftest http-pragma?
    (with-core-stream (s "name=\"quoted\"")
      (http-pragma? s))
  ("name" . "quoted"))

(deftest http-pragma-no-cache!
    (with-core-stream (s "")
      (http-pragma! s '(NO-CACHE))
      (return-stream s))
  "no-cache")

(deftest http-pragma-key-val!
    (with-core-stream (s "")
      (http-pragma! s '("name" . "val"))
      (return-stream s))
  "name=val")

;; TRAILER
(deftest http-trailer?
    (with-core-stream (s "Content-Type, Cache-Control, Server")
      (equal (http-trailer? s)
	     '("Server" "Cache-Control" "Content-Type")))
  t)

(deftest http-trailer!
    (with-core-stream (s "")
      (http-trailer! s '("Content-Type, Cache-Control, Server"))
      (equal (return-stream s)
	     "Content-Type, Cache-Control, Server"))
  t)

;; TRANSFER-ENCODING
(deftest http-transfer-encoding?
    (and (with-core-stream (s "chunked")
	   (equal (http-transfer-encoding? s)
		  '(("chunked"))))
	 (with-core-stream (s "token;attribute=\"value\"")
	   (equal (http-transfer-encoding? s)
		  '(("token" ("attribute" . "value"))))))
  t)

(deftest http-transfer-encoding!
    (and (with-core-stream (s "")
	   (http-transfer-encoding! s '((CHUNKED)))
	   (equal (return-stream s)
		  "chunked"))
	 (with-core-stream (s "")
	   (http-transfer-encoding! s '(("token" . (("attr" . "val") ("attr2" . "val2")))))
	   (equal (return-stream s)
		  "token;attr=\"val\";attr2=\"val2\"")))
  t)

;; UPGRADE
(deftest http-upgrade?
    (with-core-stream (s "HTTP/2.0, SHTTP/1.3, IRC/6.9, RTA/x11")
      (equal (http-upgrade? s)
	     '(("RTA" . "x11") ("IRC" . "6.9")
	       ("SHTTP" . "1.3") ("HTTP" . "2.0"))))
  t)

(deftest http-upgrade!
    (with-core-stream (s "")
      (http-upgrade! s '(("RTA" . "x11") ("IRC" . "6.9")
			 ("SHTTP" . "1.3") ("HTTP" . "2.0")))
      (equal (return-stream s)
	     "RTA/x11, IRC/6.9, SHTTP/1.3, HTTP/2.0"))
  t)

;; VIA
(deftest http-via?
    (with-core-stream (s "HTTP/1.1 core.gen.tr:80 (core server site), 1.1 nasa, 1.0 cgrid (asd)") 
      (http-via? s))
  ((("HTTP" . "1.1") ("core.gen.tr" . 80) "core server site")
   ((nil . "1.1") ("nasa") NIL)
   ((nil . "1.0") ("cgrid") "asd")))

(deftest http-via!
    (with-core-stream (s "")
      (http-via! s '((("HTTP" . "1.1") ("core.gen.tr" . 80) "core server site")
		     ((NIL . "1.1") ("nasa") NIL)
		     ((NIL . "1.0") ("cgrid") "asd")))
      (equal (return-stream s)
	     "HTTP/1.1 core.gen.tr:80 (core server site), 1.1 nasa , 1.0 cgrid (asd)"))
  t)

;; WARNING
(deftest http-warning?
    (with-core-stream (s "199 www.core.gen.tr:80 \"warn text\" \"Tue, 01 Feb 2008 04:20:00 GMT\"")
      (http-warning? s))
  ((199 ("www.core.gen.tr" . 80) "warn text" 3410828400)))

(deftest http-warning!
    (with-core-stream (s "")
      (http-warning! s '((199 ("www.core.gen.tr" . 80) "warn text" 3408142800)))
      (equal (return-stream s) 
	     "199 www.core.gen.tr:80 \"warn text\" \"Tue, 01 Feb 2008 02:20:00 GMT\""))
  t)

(deftest http-response-render!
    (let ((date (encode-universal-time 0 20 6 1 1 2008))
	  (response (make-instance 'http-response)))
      (with-core-stream (s "")
	(setf (http-message.general-headers response)
	      `((CACHE-CONTROL . "private")
		(DATE . ,date)
		(CONNECTION . CLOSE)))
	(setf (http-response.response-headers response)
	      `((SERVER . "(CORE-SERVER . (0 2))")))
	(setf (http-response.entity-headers response)
	      `((CONTENT-TYPE . ("text" "html" . (("charset" . "UTF-8"))))))
	(core-server::http-response-headers! s response)
	(return-stream s)))
  "HTTP/1.1 200 OK
CACHE-CONTROL: private
DATE: Tue, 01 Feb 2008 04:20:00 GMT
CONNECTION: close
SERVER: (CORE-SERVER . (0 2))
CONTENT-TYPE: text/html;charset=UTF-8
")

(deftest x-www-form-urlencoded?
    (with-core-stream (s "username=kazim&password=sananelazim&email=kazim%40patates.com&Sent=sent")
      (core-server::x-www-form-urlencoded? s))
  (("username" . "kazim")
   ("password" . "sananelazim") 
   ("email" . "kazim@patates.com")
   ("Sent" . "sent")))

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

(defvar *ie-modlisp-urlencoded*
  "server-protocol
HTTP/1.1
method
POST
url
/coretal/js.core?s=dGLvrnRW&k=act-sGcKrhFB
content-type
application/x-www-form-urlencoded
content-length
43
server-ip-addr
10.0.0.1
server-ip-port
80
remote-ip-addr
10.0.0.103
script-filename
/var/www/coretal/js.core
remote-ip-port
1117
server-id
core-server
server-baseversion
Apache/2.2.6
modlisp-version
1.3.1
modlisp-major-version
2
Accept
*/*
Accept-Language
tr
Referer
http://10.0.0.1/coretal/#
Content-Type
application/x-www-form-urlencoded
UA-CPU
x86
Accept-Encoding
gzip, deflate
User-Agent
Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; .NET CLR 1.1.4322; .NET CLR 2.0.50727; .NET CLR 3.0.04506.30)
Host
10.0.0.1
Content-Length
43
Connection
Keep-Alive
Cache-Control
no-cache
end
username=%22admin%22&password=%22c0r3t4l%22")

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