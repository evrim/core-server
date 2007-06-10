(in-package :tr.gen.core.server.test)

(deftest http-media-range
    (let ((media-type (make-core-stream "text/html;q=0.8;a=test")))
      (equal (list "text" "html" '(("a" . "test") ("q" . "0.8")))
	     (multiple-value-list (core-server::http-media-range? media-type))))
  t)
