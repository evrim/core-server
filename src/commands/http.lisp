(in-package :core-server)

(defvar +http-cache+
  (make-hash-table :test #'equal :synchronized t))

(defcommand http ()
  ((method :host local :initform 'get)
   (url :host local :initform (error "Please provide :url"))
   (post-data :host local :initform nil) 
   (parse-p :host local :initform t)
   (debug :host local :initform nil)
   (cache-p :host local :initform nil)))

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
      ((eq (s-v 'method) 'head)
       response)
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
  (when (http.debug self)
    (error "HTTP Status ~A at uri ~A"
	   (http-response.status-code response)
	   (uri->string (s-v 'url)))))

(defmethod fetch-url ((self http))
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
	    (cond
	      ((and (or (eq (http-response.status-code response) 301)
			(eq (http-response.status-code response) 302)
			(eq (http-response.status-code response) 303)
			(eq (http-response.status-code response) 307))
		    (http-response.get-response-header response 'location)
		    (not (equal (uri->string (http-response.get-response-header response 'location))
				(uri->string (s-v 'url)))))
	       (let ((location (http-response.get-response-header response 'location)))
		 (if (s-v 'debug)
		     (describe (list 'redirecting-to location)))
		 (http :method (s-v 'method)
		       :post-data (s-v 'post-data)
		       :url (uri->string location)
		       :cache-p (s-v 'cache-p)
		       :parse-p (s-v 'parse-p)
		       :debug (s-v 'debug))))
	      ((not (eq (http-response.status-code response) 200))
	       (http.raise-error self request response)))
	    (close-stream stream)
	    (values (let ((entities (http-response.entities response)))
		      (if (null (cdr entities))
			  (car entities)
			  entities))
		    response))
	  stream))))

(defmethod run :before ((self http))
  (http.setup-uri self))

(defmethod run ((self http))
  (let ((url-string (uri->string (s-v 'url))))
    (cond
      ((and (not (eq (s-v 'method) 'head)) (http.cache-p self)
	    (http.parse-p self))
       (cond
	 ((null (gethash url-string +http-cache+))
	  (multiple-value-bind (entity response) (fetch-url self)
	    (setf (gethash url-string +http-cache+) (cons entity response))
	    (values entity response)))
	 ((eq 0 (random 20))
	  (multiple-value-bind (entity response)
	      (http :url url-string :post-data (s-v 'post-data)
		    :method 'head)
	    (declare (ignore entity))
	    (let ((last-modified (http-response.get-entity-header response 'last-modified)))
	      (destructuring-bind (entity . response)
		  (gethash url-string +http-cache+) 
		(cond
		  ((> last-modified (http-response.get-entity-header response 'last-modified))
		   (remhash url-string +http-cache+)
		   (run self))
		  (t (values entity response)))))))
	 (t
	  (destructuring-bind (entity . response)
	      (gethash url-string +http-cache+)
	    (values entity response)))))
      (t
       (fetch-url self)))))
