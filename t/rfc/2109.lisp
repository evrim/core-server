(in-package :tr.gen.core.server.test)

(deftest test-cookie
    (let ((cookie-o (make-cookie "neym" "valyu"
				 :version 2 :comment "coment"
				 :domain "domeyn" :max-age 0
				 :path "/acme" :secure t))
	  (cookie-s "neym=\"valyu\"; Version=\"2\"; Comment=\"coment\"; Domain=\"domeyn\"; Max-age=\"0\"; Path=\"/acme\"; Secure")
	  (cstream (make-core-stream "")))
      ;; action
      (cookie! cstream cookie-o)
      ;; test 
      (string= (core-server::stream-data cstream) cookie-s))
  ;; expected result
  t)
