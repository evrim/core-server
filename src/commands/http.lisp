(in-package :core-server)

(defcommand http ()
  ((method :host local :initform 'get)
   (url :host local :initform (error "Please provide :url"))
   (post-data :host local :initform nil)
   (%stream :host none :initform nil)))

(defmethod http.ssl-p ((self http))
  (string= "https" (uri.scheme (s-v 'url))))

(defmethod run :around ((self http))
  (if (stringp (s-v 'url))
      (setf (s-v 'url) (uri? (make-core-stream (s-v 'url)))))

  #+ssl
  (if (and (http.ssl-p self) (null (uri.port (s-v 'url))))
      (setf (uri.port (s-v 'url)) 443))
  
  (setf (s-v '%stream)
	(connect (uri.server (s-v 'url)) (or (uri.port (s-v 'url)) 80)))

  #+ssl
  (when (http.ssl-p self)
    (setf (s-v '%stream)
	  (make-core-stream
	   (cl+ssl:make-ssl-client-stream
	    (slot-value (s-v '%stream) '%stream)))))
  
  (prog1 (call-next-method self)
    (close-stream (s-v '%stream))
    (setf (s-v '%stream) nil)))


(defparameter *http-request*
  "GET / HTTP/1.1
User-Agent: curl/7.21.0 (x86_64-pc-linux-gnu) libcurl/7.21.0 OpenSSL/0.9.8o zlib/1.2.3.4 libidn/1.15 libssh2/1.2.5
Host: 127.0.0.1:3000
Accept: */*
Content-type: application/x-www-form-urlencoded


")

(defparser read-everything? (c (acc (make-accumulator :byte)))
  (:oom (:type octet? c) (:collect c acc))
  (:return acc))

(defmethod run ((self http))
  (with-slots (method url) self
    (let ((request (make-http-request
		    :method method
		    :uri url
		    :headers (list (cons 'user-agent
					 "Core-serveR/(labs.core.gen.tr)")
				   (cons 'host (cons (uri.server url)
						     (uri.port url)))
				   (cons 'accept (list "*" "*")))
		    :entity-headers (if (s-v 'post-data)
					(list (cons 'content-length
						    (length (s-v 'post-data)))
					      (cons 'content-type
						    (list "application"
							  "x-www-form-urlencoded")))))))

      (http-request! (s-v '%stream) request)

      (awhen (s-v 'post-data)
	(string! (s-v '%stream) it))
      
      ;; Flush buffered stream
      #+ssl
      (if (not (http.ssl-p self))
	  (sb-impl::flush-output-buffer
	   (slot-value (s-v '%stream) '%stream))
	  (force-output (slot-value (s-v '%stream)'%stream)))

      (let* ((response (http-response? (s-v '%stream)))
	     (content-type (http-response.get-content-type response))
	     (content-length (http-response.get-content-length response))
	     (stream (cond
		       ((string= "chunked"
				 (caar (http-response.get-response-header
					response 'transfer-encoding)))
			(core-server::make-chunked-stream (s-v '%stream)))
		       ((and content-length (> content-length 0))
			(make-bounded-stream (s-v '%stream) content-length))
		       (t (s-v '%stream)))))
	(cond
	  ((or (and (string= "text" (car content-type))
		    (string= "html" (cadr content-type)))
	       (and (string= "application" (car content-type))
		    (string= "atom+xml" (cadr content-type))))
	   (setf (http-response.entities response)
		 (list (read-stream (make-xml-stream stream))))) 
	  (t
	   (setf (http-response.entities response)
		 (list (read-everything? stream)))))
	(setf (http-response.stream response) (s-v '%stream))
	response))))

;; (defparameter *req*
;;   (make-http-request
;;    :method 'get
;;    :uri (uri? (make-core-stream "http://localhost/foo.bar"))
;;    :headers (list (cons 'user-agent "Core-serveR/(labs.core.gen.tr)")
;; 		  (cons 'host (cons "localhost" nil))
;; 		  (cons 'accept (list "*" "*")))))
