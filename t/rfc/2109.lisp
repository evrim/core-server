(in-package :tr.gen.core.server.test)

(deftest test-cookie-to-stream
    (let ((cookie-o (make-cookie "neym" "valyu"
				 :version 2 :comment "coment"
				 :domain "domeyn" :max-age 0
				 :path "/acme" :secure t))
	  (cookie-s "neym=\"valyu\"; Version=\"2\"; Comment=\"coment\"; Domain=\"domeyn\"; Max-age=\"0\"; Path=\"/acme\"; Secure")
	  (cstream (make-core-stream "")))
      ;; write to stream
      (cookie! cstream cookie-o)
      ;; read from stream and compare
      (string= (core-server::stream-data cstream) cookie-s))
  ;; expected result
  t)

(deftest test-cookie-from-stream
    (let* ((cstream (make-core-stream "$Version=\"1\";
Customer=\"WILE_E_COYOTE\"; $Path=\"/acme\"; $Domain=\"gee\"; 
Part_Number=\"Rocket_Launcher_0001\"; $Path=\"/acme\"; 
Shipping=\"FedEx\"; $Path=/acme;"))
	   (cookies (cookie? cstream)))
      (and (equal 3 (length cookies))
	   (equal (cookie.name (car cookies)) "Customer")
	   (equal (cookie.value (car cookies)) "WILE_E_COYOTE")
	   (equal (cookie.domain (car cookies)) "gee")
	   (equal (cookie.path (caddr cookies)) "/acme")))
  t)
