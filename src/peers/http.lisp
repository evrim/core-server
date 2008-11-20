;; Core Server: Web Application Server

;; Copyright (C) 2006-2008  Metin Evrim Ulu, Aycan iRiCAN

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :tr.gen.core.server)

;;+----------------------------------------------------------------------------
;;| HTTP Peer
;;+----------------------------------------------------------------------------
;;
;; This file contains implementation of HTTP Handling peer. Its server is
;; an instance of http-server.
;;
(defclass http-peer (stream-peer)
  ((peer-type :accessor http-peer.peer-type :initform 'http
	      :documentation "Current peer type, it can be 'http' or 'mod-lisp'"))
  (:default-initargs :name "Http Peer Handling Unit")
  (:documentation "HTTP Peer - This peer handles HTTP requests and
evaulates to a HTTP response. Its' server is an instance of http-server"))

(defvar +default-encoding-for-remote-mimes+
  :utf-8 "FIXME: Browser shoudl supply in which encoding did it encode data.")

(defmethod parse-request ((self http-peer) stream)
  "Returns a fresh RFC 2616 HTTP Request object parsing from 'stream',
nil if stream data is invalid"
  (multiple-value-bind (peer-type method uri version general-headers
				  request-headers entity-headers unknown-headers)
      (http-request-headers? stream)
    (setf (http-peer.peer-type self) peer-type)
    (if method
	(let ((request (make-instance 'http-request :stream stream)))
	  (let ((content-type (cadr (assoc 'content-type entity-headers)))
		(content-length (cadr (assoc 'content-length entity-headers))))
	    (if (null content-length)
		(setf content-length 0))
	    (cond
	      ;; content-type = '("multipart" "form-data")
	      ((and (string-equal "multipart" (car content-type))
		    (string-equal "form-data" (cadr content-type))
		    (> content-length 0))
	       ;; eat lineer whayt spaces.
	       (lwsp? stream)
	       ;; set max-read to content-length for termination
	       (setf (slot-value stream '%max-read) (1- content-length))
	       (setf (http-message.entities request)
		     (rfc2388-mimes?
		      stream
		      (cdr
		       (assoc "boundary" (caddr content-type) :test #'string=))))
	       (setf (uri.queries uri)
		     (append (uri.queries uri)
			     (reduce0 (lambda (acc media)						     
					(cond
					  ((mime.filename media)
					   (cons (cons (mime.name media) media)
						 acc))
					  ((mime.name media)
					   (cons (cons (mime.name media)
						       (octets-to-string (mime.data media)
									 +default-encoding-for-remote-mimes+))
						 acc))
					  (t
					   (warn "Unkown media received:~A" media)
					   acc)))
				      (filter (lambda (a)
						(typep a 'top-level-media))
					      (http-message.entities request))))))
	      ;; content-type = '("application" "x-www-form-urlencoded")
	      ((and (string-equal "application" (car content-type))
		    (string-equal "x-www-form-urlencoded" (string-upcase (cadr content-type)))
		    (>  content-length 0))
	       ;; eat lineer whayt spaces.
	       (lwsp? stream)
	       ;; set max-read to content-length for termination	       
	       (setf (slot-value stream '%max-read) (1- content-length)
		     (uri.queries uri) (append (uri.queries uri)
					       (aif (json? stream)
						    (hash-to-alist it)
						    (x-www-form-urlencoded? stream)))))))
	  (setf (http-message.general-headers request) general-headers
		(http-message.unknown-headers request) unknown-headers
		(http-request.headers request) request-headers
		(http-request.entity-headers request) entity-headers
		(http-request.uri request) uri
		(http-request.method request) method
		(http-message.version request) version)
	  request))))

;;; (mapcar #'(lambda (mime)
;;; 		      (describe mime)
;;; 		      (when (and (mime.filename mime) (mime.data mime))
;;; 			(with-core-stream (stream (pathname (concatenate 'string "/tmp/" (mime.filename mime)))) 
;;; 			  (describe (length (mime.data mime)))
;;; 			  (reduce #'(lambda (s a)
;;; 				      (prog1 s (write-stream s a)))
;;; 				  (mime.data mime) :initial-value stream))))
;;; 		  (http-message.entities request))
	  ;;	  (describe (mime-part.data (nth 3 (http-message.entities request))))

(defmethod render-http-headers ((self http-peer) stream response)
  "Renders RFC 2616 HTTP 'response' to 'stream'"
  (http-response-headers! stream response)
  (char! stream #\Newline))

(defmethod render-mod-lisp-headers ((self http-peer) stream response)
  "Renders Mod-Lisp HTTP 'response' to 'stream'"
  (mod-lisp-response-headers! stream response)
  (string! stream "end")
  (char! stream #\Newline))

(defmethod render-headers ((self http-peer) stream response)
  "Renders HTTP headers from 'response' to 'stream'"
  (if (eq 'mod-lisp (s-v 'peer-type))
      (render-mod-lisp-headers self stream response)
      (render-http-headers self stream response)))

(defun make-response (&optional (stream (make-core-stream "")))
  "Returns an empty HTTP Response object"
  (let ((response (make-instance 'http-response :stream stream)))
    (setf (http-message.general-headers response)
	  (list (cons 'date (get-universal-time))
		;; (list 'pragma 'no-cache)
		(cons 'connection 'keep-alive))
	  (http-response.entity-headers response)
	  (list (cons 'content-type (list "text" "html" (list "charset" "UTF-8"))))
	  (http-response.response-headers response)
	  (list (cons 'server  "Core-serveR - www.core.gen.tr")))
    response))

(defmethod eval-request ((self http-peer) request)
  "Evaluates request and returns response, to be overriden by subclasses"
  (let ((response (make-response (make-core-stream (slot-value (http-request.stream request) '%stream)))))
    (checkpoint-stream (http-response.stream response))
    response))

(defmethod render-response ((self http-peer) stream response request)
  "Renders HTTP/Mod-Lisp 'response' to 'stream'"
  (let ((content-length (length (slot-value (http-response.stream response) '%write-buffer))))
    (http-response.add-entity-header response 'content-length content-length)
    (render-headers self stream response)
    (commit-stream stream)
    (if (not (eq 'head (http-request.method request)))
	(commit-stream (http-response.stream response)))))

(defmethod render-error ((self http-peer) stream)
  "Renders a generic server error response to 'stream'"
  (let ((response (make-response stream)))
    (checkpoint-stream stream)
    (setf (http-response.status-code response) (make-status-code 500))
    (render-headers self stream response)
    (with-html-output stream (<:html (<:body "An error condition occured and ignored.")))
    (commit-stream stream)))

(defmethod/unit handle-stream :async-no-return ((self http-peer) stream address)
  (flet ((handle-error (condition)
	   (if (typep condition 'sb-int::simple-stream-error)    
	       (return-from handle-stream nil))
	   (flet ((ignore-error (unit)
		    (render-error unit stream)
		    (close-stream stream))
		  (retry-error (unit)
		    (do ((i (current-checkpoint stream)
			    (current-checkpoint stream)))
			((< (the fixnum i) 0) nil)
		      (rewind-stream stream))
		    (handle-stream unit stream address)))
	     (debug-condition (aif (s-v '%debug-unit) it self)
			      condition
			      (lambda () (send-message self #'ignore-error))
			      (lambda () (send-message self #'retry-error)))
	   (return-from handle-stream nil))))
    (handler-bind ((error #'handle-error))
      (checkpoint-stream stream)
      (let ((request (parse-request self stream)))
	(if request
	    (let ((response (eval-request self request)))
	      (if response
		  (render-response self stream response request))))
	(close-stream stream)))))

(deftrace http-peer
    '(handle-stream render-error render-response eval-request parse-request
      render-headers make-response render-http-headers render-mod-lisp-headers))


(defclass nio-http-peer-mixin (unit)
  ((continuations :initform (make-hash-table) :accessor continuations))
  (:default-initargs :name "NIO Http Peer Handling Unit"))

(defmethod queue-stream ((self nio-http-peer-mixin) stream k)
  (setf (gethash stream (continuations self))
	k))

(defmethod/unit handle-stream :dispatch ((self nio-http-peer-mixin) stream address)
  (call-next-method (make-instance 'core-fd-nio-stream :stream stream :worker self) address))

;; FIXmE: what to do with below?

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
