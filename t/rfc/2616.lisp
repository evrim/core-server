(in-package :tr.gen.core.server.test)

(deftest http-media-range?
    (with-core-stream (s "text/html; q=0.8; a=test")
      (equal (list "text" "html" '(("a" . "test") ("q" . 0.8)))
	     (multiple-value-list (core-server::http-media-range? s))))
  t)

;; test request headers
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
    "Implement http-authorization!"
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

;;EXPECT FROM HOST IF-MATCH IF-MODIFIED-SINCE

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