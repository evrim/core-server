(in-package :tr.gen.core.server)

(defclass http-peer (stream-peer)
  ())

(defmethod read-request ((self http-peer) stream)
  (multiple-value-bind (headers method uri version) (http-request-headers? stream)
    (when method
      (let ((request (make-instance 'http-request)))
	(setf (http-request.headers request) headers
	      (http-request.uri request) uri
	      (http-request.method request) method
	      (http-message.version request) version)
	(describe request)
	request))))

(defmethod eval-request ((self http-peer) request)
  )

(defparameter *response-body* ;;  "Hello, World!"
  (with-yaclml-output-to-string
    (<:html
     (<:head
      (<:title "Test1"))
     (<:body
      (loop for i from 0 upto 13000
 	 do (<:ai "A"))))))

(defmethod print-response ((self http-peer) response stream)
  (checkpoint-stream stream)
  (string! stream "HTTP/1.1 200 OK")
  (char! stream #\Newline)
  (string! stream "Content-Type: text/html")
  (char! stream #\Newline)  
  (string! stream "Content-Length: ")
  (fixnum! stream (length *response-body*))
  (char! stream #\Newline)
  (char! stream #\Newline)
  (string! stream *response-body*)
  (char! stream #\Newline)
  (commit-stream stream))

(defmethod mod-lisp-print-response ((self http-peer) response stream)
  (checkpoint-stream stream)
  (string! stream "Status")
  (char! stream #\Newline)
  (string! stream "200 OK")
  (char! stream #\Newline)
  (string! stream "Content-Type")
  (char! stream #\Newline)
  (string! stream "text/plain")
  (char! stream #\Newline)
  (string! stream "Content-Length")
  (char! stream #\Newline)
  (fixnum! stream (length *response-body*))
  (char! stream #\Newline)
  (string! stream "end")
  (char! stream #\Newline)
  (string! stream *response-body*)
  (char! stream #\Newline)
  (commit-stream stream))

(defmethod/unit handle-stream :async-no-return ((self http-peer) stream address)
  (let* ((request (read-request self stream))
	 (response (eval-request self request)))
    (print-response self response stream))
  (close-stream stream))


(defparameter *req1*
  "server-protocol
HTTP/1.1
method
POST
url
/gee.gee
content-type
multipart/form-data; boundary=---------------------------1883461282365618981994564355
content-length
1095
server-ip-addr
127.0.0.1
server-ip-port
80
remote-ip-addr
127.0.0.1
script-filename
/var/www/localhost/htdocs/gee.gee
remote-ip-port
45445
server-id
evrim
server-baseversion
Apache/2.0.58
modlisp-version
1.3.1
modlisp-major-version
2
Host
localhost
User-Agent
Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.8.1) Gecko/20061010 Firefox/2.0
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
Content-Type
multipart/form-data; boundary=---------------------------1883461282365618981994564355
Content-Length
1095
end
-----------------------------1883461282365618981994564355
Content-Disposition: form-data; name=\"gee\"

gee123
-----------------------------1883461282365618981994564355
Content-Disposition: form-data; name=\"abuzer1\"

bas bakalim
-----------------------------1883461282365618981994564355
Content-Disposition: form-data; name=\"abuzer2\"; filename=\"a.html\"
Content-Type: text/html

<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html
  ><body
      ><form action=\"http://localhost/gee.gee\" method=\"POST\"
	  enctype=\"multipart/form-data\"
	        ><input name=\"gee\" type=\"text\" value=\"gee123\"
			      /><input type=\"submit\" name=\"abuzer1\" value=\"bas bakalim\">
				  <input type=\"file\" name=\"abuzer2\">
				  <input type=\"file\" name=\"abuzer3\">
				  </form
				      ></body
					    ></html
						>

-----------------------------1883461282365618981994564355
Content-Disposition: form-data; name=\"abuzer3\"; filename=\"\"
Content-Type: application/octet-stream


-----------------------------1883461282365618981994564355--
")
