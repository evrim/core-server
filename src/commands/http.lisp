(in-package :core-server)

(defcommand http ()
  ((method :host local :initform 'get)
   (url :host local :initform (error "Please provide :url"))
   (post-data :host local :initform nil) 
   (parse-p :host local :initform t)
   (debug :host local :initform t)))

(defparser read-everything? (c (acc (make-accumulator :byte)))
  (:oom (:type octet? c) (:collect c acc))
  (:return acc))

(defmethod http.make-request ((self http))
  (with-slots (method url) self
    (make-http-request
     :method method
     :uri url
     :headers (list (cons 'user-agent +x-http-client+)
		    (cons 'host (cons (uri.server url)
				      (uri.port url)))
		    (cons 'accept (list "*" "*")))
     :entity-headers (if (s-v 'post-data)
			 (list (cons 'content-length
				     (length (s-v 'post-data)))
			       (cons 'content-type
				     (list "application"
					   "x-www-form-urlencoded")))))))
(defmethod http.ssl-p ((self http))
  (string= "https" (uri.scheme (s-v 'url))))

(defmethod http.add-query ((self http) name value)
  (setf (uri.queries (s-v 'url))
	(nreverse
	 (cons (cons name value)
	       (nreverse (uri.queries (s-v 'url))))))
  (s-v 'url))

(defmethod http.add-post ((self http) name value)
  (setf (s-v 'post-data)
	(if (null (s-v 'post-data))
	    (format nil "~A=~A" name (escape-as-uri value))
	    (format nil "~A&~A=~A" (s-v 'post-data) name
		    (escape-as-uri value)))))

(defmethod http.setup-uri ((self http))
  (if (stringp (s-v 'url))
      (setf (s-v 'url) (uri? (make-core-stream (s-v 'url)))))

  #+ssl
  (if (and (http.ssl-p self) (null (uri.port (s-v 'url))))
      (setf (uri.port (s-v 'url)) 443))

  (s-v 'url))

(defmethod http.send-request ((self http) (stream core-fd-io-stream))
  (let ((request (http.make-request self)))
    (checkpoint-stream stream)
    (http-request! stream request)
    (if (s-v 'debug) (describe request))
    (awhen (s-v 'post-data)
      (if (s-v 'debug) (describe (list 'post-data (s-v 'post-data))))
      (string! stream it))

    (commit-stream stream)
    request))

(defmethod http.parse-response ((self http) (stream core-fd-io-stream))
  (let* ((response (http-response? stream))
	 (content-type (http-response.get-content-type response))
	 (content-length (http-response.get-content-length response))
	 (stream (cond
		   ((string= "chunked"
			     (caar (http-response.get-response-header
				    response 'transfer-encoding)))
		    (core-server::make-chunked-stream stream))
		   ((and content-length (> content-length 0))
		    (make-bounded-stream stream content-length))
		   (t stream))))
    (if (s-v 'debug) (describe response))
    (cond
      ((or (and (string= "text" (car content-type))
		(string= "html" (cadr content-type)))
	   (and (string= "application" (car content-type))
		(string= "atom+xml" (cadr content-type))))
       (let ((entity (read-stream (make-relaxed-xml-stream stream))))
	 (if entity
	     (setf (http-response.entities response) (list entity)))))
      ((or (and (string= "text" (car content-type))
		(string= "javascript" (cadr content-type))))
       (let ((entity (json? stream)))
	 (if entity
	     (setf (http-response.entities response) (list entity)))))
      (t
       (let ((entity (read-everything? stream)))
	 (setf (http-response.entities response) (list entity)))))
    (setf (http-response.stream response) stream)
    response))

(defmethod http.raise-error ((self http) request response)
  (error "HTTP Status ~A at uri ~A"
	 (http-response.status-code response)
	 (uri->string (s-v 'url))))

(defmethod run :before ((self http))
  (http.setup-uri self))

(defmethod run ((self http))
  (let* ((url (s-v 'url))
	 (stream (connect (uri.server url) (or (uri.port url) 80)))
	 #+ssl
	 (stream (if (http.ssl-p self)
		     (make-core-stream (cl+ssl:make-ssl-client-stream
					(slot-value stream '%stream)))
		     stream)))
    (let ((request (http.send-request self stream)))
      (if (http.parse-p self)
	  (let ((response (http.parse-response self stream)))
	    (prog1 (if (eq (http-response.status-code response) 200)
		       (let ((entities (http-response.entities response)))
			 (if (eq 1 (length entities))
			     (car entities)
			     entities))
		       (http.raise-error self request response))
	      (close-stream stream)))
	  stream))))
