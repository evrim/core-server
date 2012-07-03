(in-package :core-server)

;; -------------------------------------------------------------------------
;; HTTP Client
;; -------------------------------------------------------------------------
(defvar +http-cache+ (make-hash-table :test #'equal :synchronized t))

(defcommand http ()
  ((method :host local :initform 'get)
   (url :host local :initform (error "Please provide :url"))
   (username :host local :initform nil)
   (password :host local :initform nil)
   (post-data :host local :initform nil) 
   (parse-p :host local :initform t)
   (debug :host local :initform nil)
   (cache-p :host local :initform nil) 
   (request-headers :host local :initform nil)))

(defmethod http.%debug ((self http) &rest args)
  (if (s-v 'debug) (format *standard-output* "HTTP:~{ ~A~}~%" args)))

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

(defparser read-everything? (c (acc (make-accumulator :byte)))
  (:oom (:type octet? c) (:collect c acc)) (:return acc))

;; -------------------------------------------------------------------------
;; Make Request
;; -------------------------------------------------------------------------
(defmethod http.make-request ((self http))
  (with-slots (method url) self
    (let ((gh (list (cons 'user-agent +x-http-client+)
		    (cons 'host (cons (uri.server url) (uri.port url)))
		    (cons 'accept (list "*" "*"))))
	  (eh (if (s-v 'post-data)
		  (list (cons 'content-length (length (s-v 'post-data)))
			(cons 'content-type
			      (list "application"
				    "x-www-form-urlencoded"))))))
      (make-http-request :method method
			 :uri url
			 :headers (append gh (s-v 'request-headers))
			 :entity-headers eh))))

(defmethod http.send-request ((self http) (stream core-fd-io-stream))
  (let ((request (http.make-request self)))
    (checkpoint-stream stream)
    (http-request! stream request)
    (if (s-v 'debug) (describe request))
    (awhen (s-v 'post-data)
      (http.%debug self 'post-data (s-v 'post-data))
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
		   ((and content-length (>= content-length 0))
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
      ((and (string= "javascript" (cadr content-type))
	    (or (string= "text" (car content-type))
		(string= "application" (car content-type))))
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

(defmethod http.connect ((self http))
  (with-slots (url) self
    (let* ((stream (connect (uri.server url) (or (uri.port url) 80)))
	   #+ssl
	   (stream (if (http.ssl-p self)
		       (make-core-stream (cl+ssl:make-ssl-client-stream
					  (slot-value stream '%stream)))
		       stream)))
      stream)))

(defmethod http.authorize ((self http) (type (eql 'basic)) parameters)
  (with-slots (username password) self
    (let ((authorization (with-core-stream (s "")
			   (base64! s (concat username ":" password))
			   (return-stream s))))
      (prog1 self
	(setf (s-v 'request-headers)
	      (list (cons 'authorization (cons 'basic authorization))))))))

(defmethod http.authorize ((self http) (type (eql 'digest)) parameters)
  (flet ((get-param (name) (cdr (assoc name parameters :test #'equal)))
	 (calc-auth-response (realm username password uri nonce nc qop cnonce)
	   (let ((method (symbol-name (s-v 'method))))
	     (let ((ha1 (md5 (concat username ":" realm ":" password)))
		   (ha2 (md5 (concat method ":" uri))))
	       (md5 (concat ha1 ":" nonce ":" nc ":" cnonce ":" qop ":" ha2)))))
	 (calc-response (realm username password uri nonce)
	   (let ((method (symbol-name (s-v 'method))))
	     (let ((ha1 (md5 (concat username ":" realm ":" password)))
		   (ha2 (md5 (concat method ":" uri))))
	       (md5 (concat ha1 ":" nonce ":" ha2)))))
	 (set-header (params)
	   (setf (s-v 'request-headers)
		 (list (cons 'authorization (cons 'digest params))))))
    (with-slots (username password) self
      (let ((realm (get-param "realm"))
	    (nonce (get-param "nonce"))
	    (opaque (get-param "opaque")) 
	    (qop (split "," (get-param "qop"))))
	(cond
	  ((member "auth" qop :test #'equal)
	   (let ((uri (uri->string
		       (make-uri :paths (uri.paths (s-v 'url))
				 :queries (uri.queries (s-v 'url)))))
		 (cnonce (random-string))
		 (nc "000001")
		 (qop "auth"))
	     (set-header (list (cons "username" (s-v 'username))
			       (cons "qop" qop)
			       (cons "uri" uri)
			       (cons "nc" nc)
			       (cons "cnonce" cnonce)
			       (cons "nonce" nonce)
			       (cons "opaque" opaque)
			       (cons "realm" realm)
			       (cons "response"
				     (calc-auth-response realm username
							 password uri
							 nonce nc
							 qop cnonce))))))
	  ((null qop)
	   (let ((uri (uri->string
		       (make-uri :paths (uri.paths (s-v 'uri))
				 :queries (uri.queries (s-v 'uri)))))
		 (cnonce (random-string))
		 (nc "000001")
		 (qop "auth"))
	     (set-header (list (cons "username" (s-v 'username))
			       (cons "qop" qop)
			       (cons "nc" nc)
			       (cons "cnonce" cnonce)
			       (cons "nonce" nonce)
			       (cons "opaque" opaque)
			       (cons "realm" realm)
			       (cons "response"
				     (calc-response realm username
						    password
						    uri nonce))))))
	  (t
	   (http.%debug self "Unsupported digest qop" qop)))
	self))))

(defmethod http.fetch ((self http) &optional (recurse t))
  (with-slots (url) self
    (let* ((stream (http.connect self))
	   (request (http.send-request self stream)))
      
      ;; Return stream immediately, for debugging.
      (if (not (http.parse-p self))
	  (return-from http.fetch stream))
      
      (let* ((response (http.parse-response self stream))
	     (status-code (http-response.status-code response)))
	(flet ((return-response ()
		 (close-stream stream)
		 (values (let ((entities (http-response.entities response)))
			   (if (null (cdr entities))
			       (car entities)
			       entities))
			 response)))
	  (cond
	    ((and recurse (eq 401 status-code))
	     (http.%debug self 'unauthorized (uri->string url))
	     (with-slots (username password) self

	       ;; No username & password
	       (if (or (not username) (not password))
		   (return-from http.fetch (return-response)))
	       
	       (let ((authenticate (http-response.response-header
				    response 'www-authenticate)))
		 (destructuring-bind (scheme &rest parameters) authenticate
		   (cond
		     ((eq scheme 'basic) ;; Basic Auth
		      (http.authorize self 'basic parameters)
		      (http.fetch self nil))
		     ((eq scheme 'digest)
		      (cond
			;; Stale nonce, retry
			((equal "true" (cdr (assoc "stale" parameters
						   :test #'equal)))
			 (http.fetch self))
			(t
			 (http.authorize self 'digest parameters)
			 (http.fetch self nil))))
		     (t
		      (http.%debug self "Unknown authorization scheme" scheme)
		      (values nil response)))))))
	    ((and (member status-code '(301 302 303 307))
		  (http-response.get-response-header response 'location)
		  (not (equal (uri->string
			       (http-response.get-response-header
				response 'location))
			      (uri->string (s-v 'url)))))
	     (let ((location (http-response.get-response-header
			      response 'location)))
	       (http.%debug self 'redirecting-to location)
	       (cond
		 (recurse
		  (setf (s-v 'url) location)
		  (http.fetch self nil))
		 (t (return-response)))))
	    ((not (eq (http-response.status-code response) 200))
	     (http.raise-error self request response)
	     (return-response))
	    (t (return-response))))))))

(defmethod run :before ((self http))
  (http.setup-uri self))

(defmethod run ((self http))
  (let ((url-string (uri->string (s-v 'url))))
    (cond
      ((and (not (eq (s-v 'method) 'head)) (http.cache-p self)
	    (http.parse-p self))
       (cond
	 ((null (gethash url-string +http-cache+))
	  (multiple-value-bind (entity response) (http.fetch self)
	    (setf (gethash url-string +http-cache+) (cons entity response))
	    (values entity response)))
	 ((eq 0 (random 20))
	  (multiple-value-bind (entity response)
	      (http :url url-string :post-data (s-v 'post-data)
		    :method 'head)
	    (declare (ignore entity))
	    (let ((last-modified (http-response.get-entity-header
				  response 'last-modified)))
	      (destructuring-bind (entity . response)
		  (gethash url-string +http-cache+) 
		(cond
		  ((> last-modified (http-response.get-entity-header
				     response 'last-modified))
		   (remhash url-string +http-cache+)
		   (run self))
		  (t (values entity response)))))))
	 (t
	  (destructuring-bind (entity . response)
	      (gethash url-string +http-cache+)
	    (values entity response)))))
      (t
       (http.fetch self)))))

(deftrace http-client '(run http.fetch http.authorize
			http.send-request http.parse-response))
