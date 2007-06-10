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