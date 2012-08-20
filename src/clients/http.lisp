(in-package :core-server)

;; -------------------------------------------------------------------------
;; HTTP Client
;; -------------------------------------------------------------------------
;; Author: Evrim Ulu <evrim@core.gen.tr>
;; Last Update: August 2012

(defvar +http-cache+ (make-hash-table :test #'equal :synchronized t)
  "HTTP Cache Hash Table (url . entities)")

;; -------------------------------------------------------------------------
;; HTTP Client
;; -------------------------------------------------------------------------
(defcommand http ()
  ((method :host local :initform 'get)
   (url :host local :initform (error "Please provide :url"))
   (username :host local :initform nil)
   (password :host local :initform nil)
   (post-data :host local :initform nil)
   (debug-p :host local :initform nil :documentation "Enable debug statements")
   (parse-p :host local :initform t :documentation "Parse response entities")
   (cache-p :host local :initform nil :documentation "Cache?")
   (error-p :host local :initform t :documentation "Raise error?")   
   (request-headers :host local :initform nil)))

(defmethod http.debug ((self http) &rest args)
  (if (http.debug-p self)
      (format *standard-output* "HTTP:~{ ~A~}~%" args)))

(defmethod http.raise-error ((self http) request response)
  (if (http.error-p self)
      (error "HTTP Status ~A at uri ~A" (http-response.status-code response)
	     (uri->string (s-v 'url)))))

(defmethod http.ssl-p ((self http))
  (string= "https" (uri.scheme (s-v 'url))))

(defmethod http.add-query ((self http) name value)
  (setf (uri.queries (s-v 'url))
	(nreverse (cons (cons name value)
			(nreverse (uri.queries (s-v 'url))))))
  (s-v 'url))

(defmethod http.add-post ((self http) name value)
  (with-slots (post-data) self
    (setf post-data
	  (if (null post-data)
	      (format nil "~A=~A" (escape-as-uri name) (escape-as-uri value))
	      (format nil "~A&~A=~A" post-data
		      (escape-as-uri name) (escape-as-uri value))))))

(defmethod http.setup-uri ((self http))
  (if (stringp (s-v 'url))
      (setf (s-v 'url) (uri? (make-core-stream (s-v 'url)))))

  (s-v 'url))

(defmethod http.connect ((self http))
  (with-slots (url) self
    (let* ((stream (connect (uri.server url) (or (uri.port url)
						 (if (http.ssl-p self)
						     443
						     80))))
	   #+ssl
	   (stream (if (http.ssl-p self)
		       (make-core-stream (cl+ssl:make-ssl-client-stream
					  (slot-value stream '%stream)))
		       stream)))
      stream)))

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

    (if (s-v 'debug-p) (describe request))

    (awhen (s-v 'post-data)
      (http.debug self 'post-data (s-v 'post-data))
      (string! stream it))

    (when (s-v 'debug-p)
      (format *standard-output* "Request output buffer:~%")
      (format *standard-output* "-------------------------------------------~%")
      (format *standard-output* "~A"
	      (octets-to-string (slot-value stream '%write-buffer) :utf-8))
      (format *standard-output* "-------------------------------------------~%"))
    
    (commit-stream stream)
    (assert (< (current-checkpoint stream) 0))
    request))

(defparser read-everything? (c (acc (make-accumulator :byte)))
  (:oom (:type octet? c) (:collect c acc)) (:return acc))

(defmethod http.parse-response ((self http) (stream core-fd-io-stream))
  (let* ((response (http-response? stream))
	 (content-type (http-response.get-content-type response))
	 (content-length (http-response.get-content-length response))
	 (stream (cond
		   ((string= "chunked" (caar (http-response.get-response-header
					      response 'transfer-encoding)))
		    (make-chunked-stream stream))
		   ((and content-length (>= content-length 0))
		    (make-bounded-stream stream content-length))
		   (t stream))))
    (prog1 response
      (setf (http-response.stream response) stream)

      (if (s-v 'debug-p) (describe response))
      
      (cond
	((not (http.parse-p self))
	 (typecase stream
	   ((or core-chunked-stream bounded-stream)
	    (let ((entity (read-everything? stream)))
	      (setf (http-response.entities response) (list entity))))
	   (t (error "Cannot read-everything?, no content-length"))))
	((eq (s-v 'method) 'head) response)
	((or (and (string= "text" (car content-type))
		  (string= "html" (cadr content-type)))
	     (and (string= "application" (car content-type))
		  (string= "atom+xml" (cadr content-type))))
	 (let ((entity (read-stream (make-relaxed-xml-stream stream))))
	   (if entity
	       (setf (http-response.entities response) (list entity)))))
	((and (or (string= "javascript" (cadr content-type))
		  (string= "json" (cadr content-type)))
	      (or (string= "text" (car content-type))
		  (string= "application" (car content-type))))
	 (let ((entity (json? stream)))
	   (if entity
	       (setf (http-response.entities response) (list entity)))))
	((and (string= "text" (car content-type))
	      (string= "plain" (cadr content-type)))
	 (let ((entity (read-everything? stream)))
	   (if entity
	       (setf (http-response.entities response)
		     (list (octets-to-string entity :utf-8))))))
	(t
	 (typecase stream
	   ((or core-chunked-stream bounded-stream)
	    (let ((entity (read-everything? stream)))
	      (setf (http-response.entities response) (list entity))))
	   (t (error "Cannot read-everything?, no content-length"))))))))

(defmethod http.authorize ((self http) (type t) parameters)
  (error "Unsupported authentication method ~{~A~}" (cons type parameters)))

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
	  (t (http.debug self "Unsupported digest qop" qop)))
	self))))

(defmethod http.make-response ((self http) (response http-response))
  (values (let ((entities (http-response.entities response)))
	    (if (null (cdr entities))
		(car entities)
		entities))
	  response))

(defmethod http.fetch ((self http) &optional (recurse t))
  (http.debug self 'fetch (uri->string (s-v 'url)))
  (let* ((stream (http.connect self))
	 (request (http.send-request self stream))
	 (response (http.parse-response self stream))
	 (status-code (http-response.status-code response)))
    (declare (ignore request))
    (cond
      ((and (eq 401 status-code)
	    (http-response.response-header response 'www-authenticate))       
       (http.debug self 'unauthorized (uri->string (s-v 'url)))
       (with-slots (username password) self
	 (let ((header (http-response.response-header response 'www-authenticate)))

	   (cond
	     ;; No username & password & www-authenticate header.
	     ((or (null username) (null password) (null header))
	      (close-stream stream)
	      (http.make-response self response))
	     (t
	      (close-stream stream)
	      (http.authorize self (car header) (cdr header))
	      (http.fetch self))))))
      ((and (member status-code '(301 302 303 307 308))
	    (http-response.response-header response 'location)
	    (not (equal (uri->string
			 (http-response.get-response-header
			  response 'location))
			(uri->string (s-v 'url)))))
       (let ((location (http-response.get-response-header
			response 'location)))
	 (http.debug self 'redirecting-to location)
	 (cond
	   (recurse
	    (setf (s-v 'url) location)
	    (close-stream stream)
	    (http.fetch self nil))
	   (t
	    (close-stream stream)
	    (http.make-response self response)))))
      (t
       (close-stream stream)
       (http.make-response self response)))))

(defmethod run :before ((self http))
  (http.setup-uri self))

(defmethod http.evaluate ((self http) result response)
  (values result response))

(defmethod run :around ((self http))
  (if (or (not (http.parse-p self)) (not (http.error-p self)))
      (call-next-method self)
      (multiple-value-bind (result response) (call-next-method self)
  	(http.evaluate self result response))))

(defmethod run ((self http))
  ;; (let ((url-string (uri->string (s-v 'url))))
  ;;   (cond
  ;;     ((and (not (eq (s-v 'method) 'head)) (http.cache-p self)
  ;; 	    (http.parse-p self))
  ;;      (cond
  ;; 	 ((or (http.debug self) (null (gethash url-string +http-cache+)))
  ;; 	  (multiple-value-bind (entity response) (http.fetch self)
  ;; 	    (setf (gethash url-string +http-cache+) (cons entity response))
  ;; 	    (values entity response)))5B
  ;; 	 ((eq 0 (random 20))
  ;; 	  (multiple-value-bind (entity response) (http :url url-string
  ;; 						       :post-data (s-v 'post-data)
  ;; 						       :method 'head
  ;; 						       :debug-p (http.debug self))
  ;; 	    (declare (ignore entity))
  ;; 	    (let ((last-modified (http-response.get-entity-header
  ;; 				  response 'last-modified)))
  ;; 	      (destructuring-bind (entity . response) (gethash url-string +http-cache+) 
  ;; 		(cond
  ;; 		  ((> last-modified (http-response.get-entity-header
  ;; 				     response 'last-modified))
  ;; 		   (remhash url-string +http-cache+)
  ;; 		   (run self))
  ;; 		  (t (values entity response)))))))
  ;; 	 (t
  ;; 	  (destructuring-bind (entity . response) (gethash url-string +http-cache+)
  ;; 	    (values entity response)))))
  ;;     (t
  ;;      (http.fetch self))))
  (http.fetch self))

(deftrace http-client '(run http.fetch http.authorize
			http.send-request http.parse-response http.setup-uri
			http.evaluate http.debug))



;; (destructuring-bind (scheme &rest parameters) authenticate
;;   (http.authorize self scheme parameters)
;;   (http.fetch self)
;;   (cond
;;     ((eq scheme 'basic) ;; Basic Auth
;; 	(http.authorize self 'basic parameters)
;; 	(close-stream stream)
;; 	(http.fetch self))
;;     ((eq scheme 'digest)
;; 	(cond
;; 	  ((equal "true" ;; Stale nonce, retry
;; 		  (cdr (assoc "stale" parameters :test #'equal)))
;; 	   (close-stream stream)
;; 	   (http.fetch self))
;; 	  (t
;; 	   (http.authorize self 'digest parameters)
;; 	   (close-stream stream)
;; 	   (http.fetch self))))
;;     (t
;; 	(error self "Unknown authorization scheme" scheme)
;; 	(values nil response))))
