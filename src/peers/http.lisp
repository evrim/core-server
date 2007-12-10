(in-package :tr.gen.core.server)

(defclass http-peer (stream-peer)
  ((peer-type :accessor http-peer.peer-type :initform 'http))
  (:default-initargs :name "Http Peer Handling Unit"))

(defmethod parse-request ((self http-peer) stream)
  (multiple-value-bind (peer-type method uri version general-headers
				  request-headers entity-headers unknown-headers)
      (http-request-headers? stream)
    (setf (http-peer.peer-type self) peer-type)
    (if method
	(let ((request (make-instance 'http-request :stream stream)))
	  (let ((content-type (cadr (assoc 'content-type entity-headers))))
	    (cond
	      ((and (string= 'multipart (string-upcase (car content-type)))
		    (string= 'form-data (string-upcase (cadr content-type))))
	       (describe content-type)
	       (setf (http-message.entities request)
		     (rfc2388-mimes? stream
		      (cdr
		       (assoc "boundary" (caddr content-type) :test #'string=)))) ())
	      ((and (string= 'application (string-upcase (car content-type)))
		    (string= 'x-www-form-urlencoded (string-upcase (cadr content-type))))
	       (setf (uri.queries uri) (append (uri.queries uri)
					       (x-www-form-urlencoded? stream))))
	      ((not (null content-type))
	       (error "unknown content-type:~A" (cadr (assoc 'content-type entity-headers))))))
	  (setf (http-message.general-headers request) general-headers
		(http-message.unknown-headers request) unknown-headers
		(http-request.headers request) request-headers
		(http-request.entity-headers request) entity-headers
		(http-request.uri request) uri
		(http-request.method request) method
		(http-message.version request) version)
;;	  (describe request)
;;	  (describe uri)
	  (mapcar #'(lambda (mime)
		      (describe mime)
		      (when (mime.data mime)
			(describe (octets-to-string
				   (mime.data mime) :iso-8859-9)))) (http-message.entities request))
;;	  (describe (mime-part.data (nth 3 (http-message.entities request))))
	  request))))

(defmethod render-http-headers ((self http-peer) stream response)  
  (checkpoint-stream stream)
  (http-response-headers! stream response)
  (char! stream #\Newline)
  (char! stream #\Newline)
  (commit-stream stream))

(defmethod render-mod-lisp-headers ((self http-peer)stream response)  
  (checkpoint-stream stream)
  (mod-lisp-response-headers! stream response)
  (string! stream "end")
  (char! stream #\Newline)
  (commit-stream stream))

(defmethod render-headers ((self http-peer) stream response)
  (if (eq 'mod-lisp (s-v 'peer-type))
      (render-mod-lisp-headers self stream response)
      (render-http-headers self stream response)))

(defun make-response ()
  (let ((response (make-instance 'http-response :stream (make-core-stream ""))))
    (setf (http-message.general-headers response)
	  (list (cons 'date (get-universal-time))
		(list 'pragma 'no-cache)
		(cons 'connection 'keep-alive))
	  (http-response.entity-headers response)
	  (list (cons 'content-type (list "text" "html" (list "charset" "UTF-8"))))
	  (http-response.response-headers response)
	  (list (cons 'server  "Core-serveR - www.core.gen.tr")))
    response))

(defmethod eval-request ((self http-peer) request)
  (let ((response (make-response)))    
    (checkpoint-stream (http-response.stream response))
    response))

(defmethod render-response ((self http-peer) stream response)
  (render-headers self stream response)
  (commit-stream (http-response.stream response))
  (string! stream (return-stream (http-response.stream response)))
  (commit-stream stream)
  (close-stream stream))

(defmethod render-error ((self http-peer) stream)
  (let ((response (make-response)))
    (checkpoint-stream stream)
    (setf (http-response.status-code response) (make-status-code 200))
    (render-headers self stream response)
    (with-yaclml-stream stream
      (<:html
       (<:body
	(<:ah "An error occured."))))
    (commit-stream stream)
    (close-stream stream)))

(defmethod/unit handle-stream :async-no-return ((self http-peer) stream address)		
  (restart-case
      (progn
	(let ((swank::*sldb-quit-restart* 'fail)
	      (request (parse-request self stream)))
	  (if request
	      (let ((response (eval-request self request)))
		(if response		  
		    (render-response self stream response))))))
    (fail () :report "Give up evaluating request"
	  (render-error self stream))
    (retry () :report "Retry evaluating request"
	   (handle-stream self stream address))))

;; (defparameter *response-body* ;;  "Hello, World!"
;;   (with-yaclml-output-to-string
;;     (<:html
;;      (<:head
;;       (<:title "Test1"))
;;      (<:body
;;       (loop for i from 0 upto 400
;;  	 do (<:ai "A"))))))

;; (defparameter *req1*
;;   "server-protocol
;; HTTP/1.1
;; method
;; POST
;; url
;; /gee.gee
;; content-type
;; multipart/form-data; boundary=---------------------------1883461282365618981994564355
;; content-length
;; 1095
;; server-ip-addr
;; 127.0.0.1
;; server-ip-port
;; 80
;; remote-ip-addr
;; 127.0.0.1
;; script-filename
;; /var/www/localhost/htdocs/gee.gee
;; remote-ip-port
;; 45445
;; server-id
;; evrim
;; server-baseversion
;; Apache/2.0.58
;; modlisp-version
;; 1.3.1
;; modlisp-major-version
;; 2
;; Host
;; localhost
;; User-Agent
;; Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.8.1) Gecko/20061010 Firefox/2.0
;; Accept
;; text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5
;; Accept-Language
;; tr,en;q=0.7,en-us;q=0.3
;; Accept-Encoding
;; gzip,deflate
;; Accept-Charset
;; ISO-8859-9,utf-8;q=0.7,*;q=0.7
;; Keep-Alive
;; 300
;; Connection
;; keep-alive
;; Content-Type
;; multipart/form-data; boundary=---------------------------1883461282365618981994564355
;; Content-Length
;; 1095
;; end
;; -----------------------------1883461282365618981994564355
;; Content-Disposition: form-data; name=\"gee\"

;; gee123
;; -----------------------------1883461282365618981994564355
;; Content-Disposition: form-data; name=\"abuzer1\"

;; bas bakalim
;; -----------------------------1883461282365618981994564355
;; Content-Disposition: form-data; name=\"abuzer2\"; filename=\"a.html\"
;; Content-Type: text/html

;; <!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
;; <html
;;   ><body
;;       ><form action=\"http://localhost/gee.gee\" method=\"POST\"
;; 	  enctype=\"multipart/form-data\"
;; 	        ><input name=\"gee\" type=\"text\" value=\"gee123\"
;; 			      /><input type=\"submit\" name=\"abuzer1\" value=\"bas bakalim\">
;; 				  <input type=\"file\" name=\"abuzer2\">
;; 				  <input type=\"file\" name=\"abuzer3\">
;; 				  </form
;; 				      ></body
;; 					    ></html
;; 						>

;; -----------------------------1883461282365618981994564355
;; Content-Disposition: form-data; name=\"abuzer3\"; filename=\"\"
;; Content-Type: application/octet-stream


;; -----------------------------1883461282365618981994564355--
;; ")
