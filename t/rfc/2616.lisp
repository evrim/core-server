(in-package :tr.gen.core.server.test)

(deftest http-media-range?
    (with-core-stream (s "text/html; q=0.8; a=test")
      (equal (list "text" "html" '(("a" . "test") ("q" . 0.8)))
	     (multiple-value-list (core-server::http-media-range? s))))
  t)

;;;; test request headers
;;ACCEPT ACCEPT-CHARSET ACCEPT-ENCODING ACCEPT-LANGUAGE AUTHORIZATION
;;EXPECT FROM HOST IF-MATCH IF-MODIFIED-SINCE IF-RANGE IF-UNMODIFIED-SINCE
;;MAX-FORWARDS PROXY-AUTHORIZATION RANGE REFERER TE USER-AGENT

;; ACCEPT
(deftest http-accept?
    (with-core-stream (s "text/*;q=0.3, text/html;q=0.7, text/html;level=1,
                          text/html;level=2;q=0.4, */*;q=0.5")
      (equal (http-accept? s)
	     '(("*" "*" (("q" . 0.5)))
	       ("text" "html" (("q" . 0.4) ("level" . "2")))
	       ("text" "html" (("level" . "1")))
	       ("text" "html" (("q" . 0.7)))
	       ("text" "*" (("q" . 0.3)))))) 
  t)

;; ACCEPT-CHARSET
(deftest http-accept-charset?
    (with-core-stream (s "iso-8859-5, unicode-1-1;q=0.8")
      (equal (http-accept-charset? s)
	     '(("iso-8859-5" . 1.0) ("unicode-1-1" . 0.8))))
  t)

;; ACCEPT-ENCODING
(deftest http-accept-encoding?
    (with-core-stream (s "gzip;q=1.0, identity; q=0.5, *;q=0")
      (equal (http-accept-encoding? s)
	     '(("gzip" . 1.0) ("identity" . 0.5) ("*" . 0.0))))
  t)

;; ACCEPT-LANGUAGE
(deftest http-accept-language?
    (with-core-stream (s "tr, en-us;q=0.1, en;q=0.7")
      (equal (http-accept-language? s)
	     '(("tr" . 1.0) ("en-us" . 0.1) ("en" . 0.7))))
  t)

;; AUTHORIZATION
(deftest http-authorization?
    "Implement http-authorization (rfc2616) plz!"
  t)

;; EXPECT
(deftest http-expect?
    (and (with-core-stream (s "asd=asd;q=324")
	   (equal (http-expect? s)
		  '(("asd" . "asd") ("q" . "324"))))
	 (with-core-stream (s "100-continue")
	   (string= (http-expect? s)
		    '100-continue))) 
  t)

;; FROM
(deftest http-from?
    "Implement http-from (rfc2616) plz!"
  t)

(deftest http-host?
    (with-core-stream (s "www.core.gen.tr:80")
      (equal (http-host? s)
	     '("www.core.gen.tr" . 80)))
    t)

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
      (equal (http-if-modified-since? s)
	     2989849411))
    t)

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
    (and (with-core-stream (s "Tue, 01 Feb 2008 04:20:00 GMT")
	   (equal (http-if-range? s)
		  3408142800))
	 (with-core-stream (s "W/\"ranger\"")
	   (equal (http-if-range? s)
		  '("ranger" . t))))
  t)


;; IF-UNMODIFIED-SINCE
(deftest http-if-unmodified-since
    (with-core-stream (s "Tue, 01 Feb 2008 04:20:00 GMT")
      (equal (http-if-unmodified-since? s)
	     3408142800))
  t)

;; MAX-FORWARDS
(deftest http-max-forwards?
    (with-core-stream (s "414")
      (equal (http-max-forwards? s)
	     414))
  t)

;; PROXY-AUTHORIZATION
(deftest http-proxy-authorization?
    "Implement proxy-authorization (rfc2616) plz!"
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
    (with-core-stream (s "deflate;q=0.3;asd=234")
      (equal (http-te? s)
	     '("deflate" ("asd" . "234") ("q" . 0.3))))
  t)

;; USER-AGENT
(deftest http-user-agent?
    (and (with-core-stream (s "Opera/9.21 (X11; Linux i686; U; en)")
	   (equal (http-user-agent? s)
		  '((BROWSER . OPERA) (VERSION (9 21)) (OS . "Linux i686"))))
	 (with-core-stream (s "Mozilla/5.0 (X11; U; Linux i686; tr-TR; rv:1.8.1.2) Gecko/20070511 SeaMonkey/1.1.1")
	   (equal (http-user-agent? s)
		  '((BROWSER . SEAMONKEY) (MOZ-VER (5 0)) (OS . "Linux i686") (REVISION (1 8 1 2)) (VERSION (1 1 1)))))
	 (with-core-stream (s "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV1; .NET CLR 1.1.4322; .NET CLR 2.0.50727)")
	   (equal (http-user-agent? s)
		  '((BROWSER . IE) (MOZ-VER (4 0)) (VERSION (6 0)) (OS . "Windows NT 5.1"))))) 
  t)

;;;; request headers test end

;;;; test response headers
;; ACCEPT-RANGES
(deftest http-accept-ranges!
    (with-core-stream (s "")
      (http-accept-ranges! s "bytes")
      (equal (core-server::stream-data s)
	     "bytes"))
  t)

;; AGE
(deftest http-age!
    (with-core-stream (s "")
      (http-age! s 12)
      (equal (core-server::stream-data s)
	     "12"))
  t)

;; ETAG
(deftest http-etag!
    (and (with-core-stream (s "")
	   (http-etag! s '("test" . t))
	   (equal (core-server::stream-data s)
		  "W/\"test\""))
	 (with-core-stream (s "")
	   (http-etag! s '("wotest"))
	   (equal (core-server::stream-data s)
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
      (equal (core-server::stream-data s)
	     "http://john:foo@127.0.0.1:8080/test/me/up.html"))
  t)

;; PROXY-AUTHENTICATE
(deftest http-proxy-authenticate!
    "Implement proxy-authenticate (rfc2616) plz!"
  t)

;; RETRY-AFTER
(deftest http-retry-after!
    (with-core-stream (s "")
      (http-retry-after! s (encode-universal-time 0 20 6 1 1 2008))
      (equal (core-server::stream-data s)
	     "Tue, 01 Feb 2008 04:20:00 GMT"))
  t)

;; SERVER
(deftest http-server!
    (with-core-stream (s "")
      (http-server! s "(CORE-SERVER . (0 2))")
      (equal (core-server::stream-data s)
	     "(CORE-SERVER . (0 2))"))
  t)

;; VARY
(deftest http-vary!
    (and (with-core-stream (s "")
	   (http-vary! s '())
	   (equal (core-stream::stream-data s) "*"))
	 (with-core-stream (s "")
	   (http-vary! s '("Free" "Software"))
	   (equal (core-stream::stream-data s) "Free,Software")))
  t)

;; WWW-AUTHENTICATE

(deftest http-response-render?
    (let ((date (encode-universal-time 0 20 6 1 1 2008))
	  (response (make-instance 'http-response)))
      (with-core-stream (s "")
	(setf (http-message.general-headers response)
	      `((CACHE-CONTROL . "private")
		(DATE . ,date)
		(CONNECTION . CLOSE)))
	(setf (http-response.headers response)
	      `((SERVER . "(CORE-SERVER . (0 2))")))
	(setf (http-message.entities response)
	      `((CONTENT-TYPE . ("text" "html" . (("charset" . "UTF-8"))))))
	(http-response! s response)
	(equal (core-server::stream-data s)
	       "HTTP/1.1 200 OK
CACHE-CONTROL: private
DATE: Tue, 01 Feb 2008 04:20:00 GMT
CONNECTION: CLOSE
SERVER: (CORE-SERVER . (0 2))
CONTENT-TYPE: text/html;charset=UTF-8
")))
  t)