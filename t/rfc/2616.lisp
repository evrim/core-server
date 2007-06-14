(in-package :tr.gen.core.server.test)

(deftest http-media-range
    (let ((cstream (make-core-stream "text/html; q=0.8; a=test")))
      (equal (list "text" "html" '(("a" . "test") ("q" . 0.8)))
	     (multiple-value-list (core-server::http-media-range? cstream))))
  t)

(deftest http-accept
    (let ((cstream (make-core-stream "text/html;q=0.8; a=test, text/plain;q=0.7")))
      (equal
       '(("text" "plain" (("q" . 0.7))) ("text" "html" (("a" . "test") ("q" . 0.8)))) 
       (core-server::http-accept? cstream)))
  t)

(deftest http-language
    (let ((cstream (make-core-stream "utf-8; q=0.3")))
      (equal '("utf-8" . 0.3)
	     (core-server::http-language? cstream)))
  t)

(deftest http-accept-charset
    (let ((cstream (make-core-stream "iso-8859-5, unicode-1-1; q=0.8, *;q=0")))
      (equal '(("iso-8859-5" . 1.0) ("unicode-1-1" . 0.8) ("*" . 0.0)) 
	     (core-server::http-accept-language? cstream)))
  t)

(deftest http-accept-encoding
    (let ((cstream (make-core-stream "gzip;q=1.0, identity; q=0.5, *;q=0")))
      (equal '(("gzip" . 1.0) ("identity" . 0.5) ("*" . 0.0))
	     (core-server::http-accept-language? cstream)))
  t)

(deftest http-accept-language
    (let ((cstream (make-core-stream "tr, en-gb;q=0.8, en;q=0.7")))
      (equal '(("tr" . 1.0) ("en-gb" . 0.8) ("en" . 0.7))
	     (core-server::http-accept-language? cstream)))
  t)

;; test request headers
;;ACCEPT ACCEPT-CHARSET ACCEPT-ENCODING ACCEPT-LANGUAGE AUTHORIZATION
;;EXPECT FROM HOST IF-MATCH IF-MODIFIED-SINCE IF-RANGE IF-UNMODIFIED-SINCE
;;MAX-FORWARDS PROXY-AUTHORIZATION RANGE REFERER TE USER-AGENT

;; ACCEPT
(deftest http-accept?
    (let ((cstream (make-core-stream "text/*;q=0.3, text/html;q=0.7, text/html;level=1,
                                      text/html;level=2;q=0.4, */*;q=0.5")))
      (equal (http-accept? cstream)
	     '(("*" "*" (("q" . 0.5)))
	      ("text" "html" (("q" . 0.4) ("level" . "2")))
	      ("text" "html" (("level" . "1")))
	      ("text" "html" (("q" . 0.7)))
	      ("text" "*" (("q" . 0.3)))))) 
  t)

;; ACCEPT-CHARSET
(deftest http-accept-charset?
    (let ((cstream (make-core-stream "iso-8859-5, unicode-1-1;q=0.8")))
      (equal (http-accept-charset? cstream)
	     '(("iso-8859-5" . 1.0) ("unicode-1-1" . 0.8))))
  t)

;; ACCEPT-ENCODING
(deftest http-accept-encoding?
    (let ((cstream (make-core-stream "gzip;q=1.0, identity; q=0.5, *;q=0")))
      (equal (http-accept-encoding? cstream)
	     '(("gzip" . 1.0) ("identity" . 0.5) ("*" . 0.0))))
  t)

;; ACCEPT-LANGUAGE
(deftest http-accept-language?
    (let ((cstream (make-core-stream "tr, en-us;q=0.1, en;q=0.7")))
      (equal (http-accept-language? cstream)
	     '(("tr" . 1.0) ("en-us" . 0.1) ("en" . 0.7))))
  t)

;; AUTHORIZATION
(deftest http-authorization?
    "Implement http-authorization!"
  t)

(deftest http-response-render
    (let ((date (encode-universal-time 0 20 6 1 1 2008))
	  (cstream (make-core-stream ""))
	  (response (make-instance 'http-response)))
      (setf (http-message.general-headers response)
	    `((CACHE-CONTROL . "private")
	      (DATE . ,date)
	      (CONNECTION . CLOSE)))
      (setf (http-response.headers response)
	    `((SERVER . "(CORE-SERVER . (0 2))")))
      (setf (http-message.entities response)
	    `((CONTENT-TYPE . ("text" "html" . (("charset" . "UTF-8"))))))
      (http-response! cstream response)
      (equal (core-server::stream-data cstream)
	     "HTTP/1.1 200 OK
CACHE-CONTROL: private
DATE: Tue, 01 Feb 2008 04:20:00 GMT
CONNECTION: CLOSE
SERVER: (CORE-SERVER . (0 2))
CONTENT-TYPE: text/html;charset=UTF-8
"))
  t)