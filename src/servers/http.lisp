(in-package :core-server)

;; +-------------------------------------------------------------------------
;; | HTTP Server
;; +-------------------------------------------------------------------------
(defclass+ http-server (web-server socket-server logger-server)
  ((applications :accessor server.applications :initform '())
   (root-application :accessor server.root-application :initform nil))
  (:default-initargs :port 3001 :peer-class '(http-unit)))

;;--------------------------------------------------------------------------
;; Server Protocol Implementation
;;--------------------------------------------------------------------------
(defmethod find-application ((self http-server) fqdn)
  (find fqdn (server.applications self) :key #'web-application.fqdn
	:test #'equal))

(defmethod register ((self http-server) (app http-application))
  (setf (server.applications self)
	(sort (cons app
		    (remove (web-application.fqdn app)
			    (server.applications self)
			    :test #'equal :key #'web-application.fqdn))
	      #'> :key (lambda (app) (length (web-application.fqdn app)))))
  (setf (application.server app) self))

(defmethod unregister ((self http-server) (app http-application))
  (setf (server.applications self)
	(remove (web-application.fqdn app)
		(server.applications self)
		:test #'equal :key #'web-application.fqdn)))

(defmethod register ((self http-server) (app root-http-application-mixin))
  (setf (slot-value self 'root-application) app))

(defmethod unregister ((self http-server) (app root-http-application-mixin))
  (setf (slot-value self 'root-application) nil))

;; -------------------------------------------------------------------------
;; Parse Request
;; -------------------------------------------------------------------------
(defvar +default-encoding+ :utf-8 "NOTE: Browser should supply this but does not")
(defmethod parse-request ((server http-server) (stream core-stream))
  "Returns a fresh RFC 2616 HTTP Request object parsing from 'stream',
nil if stream data is invalid"
  (multiple-value-bind (peer-type method uri version general-headers request-headers
				  entity-headers unknown-headers)
      (http-request-headers? stream)
    (when (null method)
      (log-me server 'error (format nil  "Request with null method ~A" uri))
      (return-from parse-request nil))
    
    (let ((request (make-http-request :stream stream :method method
				      :uri uri :version version
				      :general-headers general-headers
				      :entity-headers entity-headers
				      :unknown-headers unknown-headers
				      :headers request-headers)))
      (let* ((content-type (http-request.content-type request))
	     (content-length (http-request.content-length request))
	     (stream (if (and content-length (> content-length 0))
			 (make-bounded-stream stream content-length)
			 stream)))
	(flet ((process-multipart ()
		 ;; content-type = '("multipart" "form-data")
		 (lwsp? stream)
		 (let* ((boundary (cdr (assoc "boundary" (caddr content-type)
					      :test #'string=)))
			(entities (rfc2388-mimes? stream boundary)))
		   (setf (http-message.entities request) entities)

		   (flet ((process-media (acc media)
			    (cond
			      ((mime.filename media)
			       (cons (cons (mime.name media) media) acc))
			      ((mime.name media)
			       (cons
				(cons (mime.name media)
				      (octets-to-string (mime.data media)
							+default-encoding+))
				acc))
			      (t
			       (let ((message (format nil "Unkown media received, uri:~A, media:~A"
						      uri media)))
				 (log-me server 'warn message)
				 acc)))))
		     (let ((media (reduce0 #'process-media
					   (filter (lambda (a)
						     (typep a 'top-level-media))
						   entities))))
		       (setf (uri.queries uri)
			     (append (uri.queries uri) media))))))
	       (process-json ()
		 (let ((hash-table (json? stream)))
		   (if (typep hash-table 'hash-table)
		       (let ((data (hash-to-alist hash-table)))
			 (setf (uri.queries uri)
			       (append (uri.queries uri) data))))))
	       (process-urlencoded ()
		 (lwsp? stream)
		 (let ((data (x-www-form-urlencoded? stream)))
		   (setf (uri.queries uri)
			 (append (uri.queries uri) data)))))
	  (cond
	    ;; content-type = '("multipart" "form-data")
	    ((and (string-equal "multipart" (car content-type))
		  (string-equal "form-data" (cadr content-type))
		  (> content-length 0))
	     (process-multipart))
	    ;; content-type = '("application" "json")
	    ((and (string-equal "text" (car content-type))
		  (string-equal "json" (cadr content-type)))
	     (process-json))
	    ;; content-type = '("application" "x-www-form-urlencoded")
	    ((and (string-equal "application" (car content-type))
		  (string-equal "x-www-form-urlencoded" (cadr content-type))
		  (>  content-length 0))
	     (process-urlencoded)))))
      request)))

;; -------------------------------------------------------------------------
;; Eval Request & Return Response
;; -------------------------------------------------------------------------
(defmethod eval-request ((server http-server) (request http-request))
  (let* ((host (car (http-request.header request 'host)))
	 (app-name (caar (uri.paths (http-request.uri request))))
	 (application (or (aif (find-application server app-name)
			       (prog1 it
				 (pop (uri.paths (http-request.uri request)))))
			  (find-application server host)
			  (slot-value server 'root-application))))
    (let ((response (if application (dispatch application request))))
      (if response
	  response
	  (let ((m (format nil "request uri: ~A" (http-request.uri request))))
	    (log-me server 'eval-request m)
	    (make-404-response request))))))

;; -------------------------------------------------------------------------
;; Make Response
;; -------------------------------------------------------------------------
(defun make-response (&optional (stream (make-core-list-output-stream)))
  "Returns an empty HTTP Response object"
  (let ((response (make-http-response :stream stream)))
    (setf (http-message.general-headers response)
	  (list (cons 'date (get-universal-time))
		;; (list 'pragma 'no-cache)
		(cons 'connection 'keep-alive))
	  (http-response.entity-headers response)
 	  (list (cons 'content-type (list "text" "html" (list "charset" "UTF-8"))))
	  (http-response.response-headers response)
	  (list (cons 'server  "Core-serveR - www.core.gen.tr")))
    response))

(defun make-404-response (&optional (stream (make-core-list-output-stream)))
  (let ((response (make-response stream)))
    (setf (http-response.status-code response) (make-status-code 404))
    (with-html-output stream
      (<:html (<:body (<:h1 "URL Not found")
		      (<:h2 "[Core-serveR]"))))
    response))

(defun make-error-response (&optional (stream (make-core-list-output-stream)))
  (let ((response (make-response stream)))
    (setf (http-response.status-code response) (make-status-code 500))
    (with-html-output stream
      (<:html (<:body
	       (<:h1 "Sorry, an error occured ")
	       (<:h2 "[Core-serveR]"))))
    response))

(defmethod render-response ((self http-server) (response http-response)
			    (request http-request))
  "Renders HTTP 'response' to 'stream'"
  (let ((stream (http-request.stream request))
	(entities (http-response.entities response))
	(compressed-p (and (member "gzip"
				   (http-request.header request 'accept-encoding)
				   :key #'car :test #'string=) t)))
    (assert (eq 0 (current-checkpoint stream)))
    (labels ((write-headers ()
	       (let ((stream (make-core-stream (slot-value stream '%stream))))
		 (checkpoint-stream stream)
		 (http-response-headers! stream response)
		 (char! stream #\Newline)
		 (commit-stream stream)))
	     (get-content ()
	       (let ((content-type (http-response.get-content-type response)))
		 (if (and (cdr content-type)
			  (string= "javascript" (cadr content-type)))
		     (let* ((stream (make-core-stream (make-accumulator :byte)))
			    (stream2 (if (server.debug self)
					 (make-indented-stream stream)
					 (make-compressed-stream stream))))
		       (serialize-to (http-response.stream response) stream2)
		       (return-stream stream2))
		     (let ((stream (make-instance 'core-vector-io-stream)))
		       (serialize-to (http-response.stream response) stream)
		       (return-stream stream)))))
	     (do-finish-compression (stream)
	       (let ((content-length (length (slot-value stream '%write-buffer))))
		 (http-response.add-entity-header response 'content-length
						  content-length)
		    
		 (write-headers)
		 (commit-stream stream)))
	     (compression-callback (stream)
	       (lambda (buffer end)
		 (write-stream stream (subseq buffer 0 end)))))
      (cond
	((and compressed-p (pathnamep (car entities)))
	 (let ((path (car entities))
	       (callback (compression-callback stream)))
	   (with-input-from-file (input path :element-type '(unsigned-byte 8))
	     (http-response.set-content-type response (split "/" (mime-type path)))
	     (http-response.add-entity-header response 'content-encoding 'gzip)
	     (checkpoint-stream stream)
	     (let ((seq (make-array 4096 :element-type '(unsigned-byte 8))))
	       (with-compressor (compressor 'gzip-compressor :callback callback)
		 (do ((len (read-sequence seq input)
			   (read-sequence seq input)))
		     ((<= len 0) nil) 
		   (compress-octet-vector seq compressor :end len :start 0)))
	       (do-finish-compression stream)))))
	((pathnamep (car entities))
	 (let ((path (car entities)))
	   (with-open-file (input path :element-type '(unsigned-byte 8)
				  :direction :input)
	     (http-response.add-entity-header response 'content-length
					      (file-length input))
	     (http-response.set-content-type response (split "/" (mime-type path)))
	     (write-headers)
	     (let ((seq (make-array 4096 :element-type '(unsigned-byte 8))))
	       (do ((len (read-sequence seq input) (read-sequence seq input)))
		   ((<= len 0) nil)
		 (write-stream stream (subseq seq 0 len)))))))
	(compressed-p
	 (http-response.add-entity-header response 'content-encoding 'gzip)
	 (checkpoint-stream stream)
	 (let ((content (get-content)))
	   (with-compressor (compressor 'gzip-compressor
					:callback (compression-callback stream))
	     (compress-octet-vector (make-array (length content)
						:initial-contents content 
						:element-type '(unsigned-byte 8))
				    compressor))
	   (do-finish-compression stream)))
	(t
	 (let ((data (get-content)))
	   (http-response.add-entity-header response 'content-length (length data))
	   (write-headers)
	   (write-stream stream data))))
      (close-stream stream))))

(defmethod render-error ((self http-server) (stream core-stream) error)
  (render-response self (make-error-response)
		   (make-http-request :stream stream)))

;; +-------------------------------------------------------------------------
;; | HTTP Unit
;; +-------------------------------------------------------------------------
(defclass http-unit (stream-peer)
  ()
  (:default-initargs :name "Http Peer Handling Unit")
  (:documentation "HTTP Peer - This peer handles HTTP requests and
evaulates to a HTTP response. Its' server is an instance of http-server"))

(defmethod/unit handle-stream :async-no-return ((peer http-unit)
						(stream core-stream) address)
  (flet ((handle-error (condition)
	   (if (typep condition 'sb-int::simple-stream-error)    
	       (return-from handle-stream nil))
	   (flet ((ignore-error (unit)
		    (declare (ignore unit))
		    (log-me (peer.server peer) 'error
			    (format nil "~A" condition))
		    (render-error (peer.server peer) stream condition)
		    (close-stream stream))
		  (retry-error (unit)
		    (do ((i (current-checkpoint stream)
			    (current-checkpoint stream)))
			((< (the fixnum i) 0) nil)
		      (rewind-stream stream))
		    (handle-stream unit stream address)))
	     (debug-condition (aif (slot-value peer '%debug-unit) it peer)
			      condition
			      (lambda () (send-message peer #'ignore-error))
			      (lambda () (send-message peer #'retry-error)))
	   (return-from handle-stream nil))))
    (handler-bind ((error #'handle-error))
      (checkpoint-stream stream)
      (let ((request (parse-request (peer.server peer) stream)))
	(if request
	    (let ((response (eval-request (peer.server peer) request)))
	      (cond
		(response
		 (render-response (peer.server peer) response request)
		 (close-stream (http-response.stream response)))
		(t
		 (close-stream stream))))
	    (progn (render-error (peer.server peer) stream nil)
		   (close-stream stream)))))))

(deftrace http-server
    '(handle-stream dispatch parse-request eval-request make-response
      render-response eval-request parse-request))

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






;; -------------------------------------------------------------------------
;; Garbage
;; -------------------------------------------------------------------------
;; (defclass http-cps-unit (local-unit peer)
;;   ((%max-events :initarg :max-events :initform 100)
;;    (%timeout :initarg :timeout :initform 10)
;;    (%continuations :initform (make-hash-table))
;;    (%epoll-fd :initform nil)
;;    (%epoll-size :initarg :epoll-size :initform 131072)
;;    (%stream :initform (make-instance 'core-cps-stream :stack (list)))
;;    (%receive-messages :initform t)))

;; (defmethod start ((self http-cps-unit))
;;   (setf (s-v '%epoll-fd) (core-ffi::epoll-create (s-v '%epoll-size))))

;; (defmethod start ((self http-server))
;; ;; ;;   (thread-kill (s-v '%socket-thread))
;; ;; ;;   (setf (s-v '%socket-thread) nil)
;; ;;   (let ((listenfd (socket-file-descriptor (s-v '%socket))))
;; ;;     (core-ffi::set-nonblock listenfd)
;; ;;     (handle-accept (car (s-v '%peers)) listenfd (s-v '%peers)))
;;   )

;; (defmethod/unit add-fd ((self http-cps-unit) fd modes k)
;;   (setf (gethash fd (s-v '%continuations)) k)
;;   (with-foreign-object (event 'core-ffi::epoll-event)
;;     (core-ffi::bzero event core-ffi::size-of-epoll-event)
;;     (with-foreign-slots ((core-ffi::events core-ffi::data) event core-ffi::epoll-event)
;;       (setf core-ffi::events (apply #'logior (ensure-list modes))
;; 	    (foreign-slot-value
;; 	     (foreign-slot-value event 'core-ffi::epoll-event 'core-ffi::data)
;; 	     'core-ffi::epoll-data 'core-ffi::fd) fd))
;;     (core-ffi::epoll-ctl (s-v '%epoll-fd) core-ffi::epoll-ctl-add fd event)))

;; (defmethod/unit del-fd ((self http-cps-unit) fd)
;;   (core-ffi::epoll-ctl (s-v '%epoll-fd) core-ffi::epoll-ctl-del fd (null-pointer)))

;; (defmethod receive-messages ((self http-cps-unit))
;;   (let ((thread (or (s-v '%thread) (current-thread))))
;;     (let* ((mbox (thread-mailbox thread))
;; 	   (lock (mailbox.lock mbox)))
;;       (with-lock-held (lock)
;; 	(loop
;; 	   (let ((q (mailbox.queue mbox)))
;; 	     (setf (mailbox.queue mbox) '())
;; 	     (return q)))))))

;; (defmethod receive-events ((self http-cps-unit))
;;   (when (s-v '%epoll-fd)
;;     (with-foreign-object (events 'core-ffi::epoll-event (s-v '%max-events))
;;       (core-ffi::bzero events (* (s-v '%max-events) core-ffi::size-of-epoll-event))
;;       (labels ((collect (i &optional (acc nil))
;; 		 (if (< i 0)
;; 		     acc
;; 		     (collect (1- i) (cons
;; 				      (foreign-slot-value
;; 				       (foreign-slot-value
;; 					(mem-ref events 'core-ffi::epoll-event (1- i))
;; 					'core-ffi::epoll-event 'core-ffi::data)
;; 				       'core-ffi::epoll-data 'core-ffi::fd) acc)))))
;; 	(let ((n (core-ffi::epoll-wait (s-v '%epoll-fd) events (s-v '%max-events)
;; 				       (if (s-v '%receive-messages)
;; 					   (s-v '%timeout)
;; 					   -1))))
;; 	  (if (> n 0)
;; 	      (collect n)
;; 	      nil))))))

;; (defmethod run ((self http-cps-unit))
;;   (handler-bind ((error (lambda (condition)
;; 			  (let ((swank::*sldb-quit-restart* 'ignore-error))
;; 			    (restart-case (swank:swank-debugger-hook condition nil)
;; 			      (ignore-error ()
;; 				:report "Ignore the error and return (values)")
;; 			      (retry ()
;; 				:report "Retry the funcall"))))))
;;     (flet ((process-message (message)	  	   
;; 	     (cond
;; 	       ((eq message 'shutdown) (return-from run nil))
;; 	       ((functionp message) (funcall message self))
;; 	       (t (format *standard-output* "Uniw ~A got unkown message:~A~%" self message))))
;; 	   (process-event (fd)
;; 	     (let ((k (gethash fd (s-v '%continuations))))
;; 	       (when k
;; 		 (remhash k (s-v '%continuations))
;; 		 (let ((+stream+ (s-v '%stream)))
;; 		   (setf (slot-value +stream+ '%stack) (list k))
;; 		   (run +stream+))))))
;;       (loop
;; 	 (progn
;; 	   (if (s-v '%receive-messages) (mapc #'process-message (receive-messages self)))
;; 	   (mapc #'process-event (receive-events self)))))))

;; (defmethod/cc1 handle-stream4 ((peer http-cps-unit) (stream core-fd-nio-stream) address)
;; ;;   (describe (current-checkpoint stream))
;; ;;   (checkpoint-stream stream)
;;   (let ((time (get-universal-time)))
;;     ;; (let ((request (parse-request (peer.server peer) stream)))
;; ;;       ;; (if request
;; ;; ;; 	  (let ((response (eval-request (peer.server peer) request)))
;; ;; ;; 	    (if response
;; ;; ;; 		(render-response (peer.server peer) response request))))
;; ;;       (if request
;; ;; 	  (render-response (peer.server peer) (make-response) request))
;; ;;       (close-stream stream)
;; ;;       (push (- (get-universal-time) time) *timing*))
;;     (write-stream stream "HTTP/1.1 200 OK
;; DATE: Thu, 30 Nov 2008 17:58:12 GMT
;; CONNECTION: keep-alive
;; SERVER: Core-serveR - www.core.gen.tr
;; CONTENT-LENGTH: 5192
;; CONTENT-TYPE: text/html

;; ")
;; ;;     (write-stream stream *5k*)
;;     (write-content stream)
;;     (close-stream stream)
;;     ;; (render-response (peer.server peer) (make-response) nil)
;;     ))

;; (defmethod/unit handle-stream :async-no-return ((peer http-cps-unit) (stream core-stream) address)
;;   (let ((fd (sb-sys::fd-stream-fd (slot-value stream '%stream))))
;;     (add-fd peer fd (list core-ffi::epollin #.(ash 1 31))
;; 	    (lambda (&rest values)
;; 	      (declare (ignore values))
;; 	      (handle-stream4 peer
;; 			      (make-instance 'core-fd-nio-stream
;; 					     :stream fd
;; 					     :unit peer)
;; 			      address)))))

;; (defmethod/unit handle-stream :async-no-return ((peer http-cps-unit) (stream number) address)
;;   (let ((fd stream))
;;     (add-fd peer fd (list core-ffi::epollin #.(ash 1 31))
;; 	    (lambda (&rest values)
;; 	      (declare (ignore values))
;; 	      (handle-stream4 peer
;; 			      (make-instance 'core-fd-nio-stream
;; 					     :stream fd
;; 					     :unit peer)
;; 			      address)))))

;; (defmethod/unit handle-accept :async-no-return ((self http-cps-unit) listenfd peers)
;;   (let* ((peers (copy-list peers))
;; 	 (peers2 (copy-list peers)))
;;     (labels ((kont (&rest values)
;; 	       (declare (ignore values))
;; 	       (multiple-value-bind (fd address) (core-ffi::accept listenfd)
;; 		 (handle-stream self fd address)
;; 		 ;; (if (car peers)
;; ;; 		     (progn
;; ;; 		       (handle-stream (car peers) fd address)
;; ;; 		       (setq peers (cdr peers))
;; ;; 		       ;; reschedule
;; ;; 		       (setf (gethash listenfd (s-v '%continuations)) #'kont))
;; ;; 		     (progn
;; ;; 		       (setq peers peers2)
;; ;; 		       (kont values)))
;; 		 )))
;;       (add-fd (car peers) listenfd (list core-ffi::epollin #.(ash 1 31)) #'kont)
;;       (setf (s-v '%receive-messages) nil))))




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


      
;; )
;;     (describe (list 'pos
;; 		    (sb-impl::fd-stream-get-file-position
;; 		     (slot-value (http-request.stream request)
;; 				 '%stream))
;; 		    (slot-value (http-request.stream request)
;; 				'%stream)))
    
;;     ;; (commit-stream stream)
;;     (describe (list 'pos
;; 		    (sb-impl::fd-stream-get-file-position
;; 		     (slot-value (http-request.stream request)
;; 				  '%stream))))
;;     ;;     (describe stream)
;;     ;;     (commit-stream stream)
;;     ;;     (if (not (eq 'head (http-request.method request)))
;;     ;; 	(commit-stream (http-response.stream response)))
;;     ;;     (write-stream stream "HTTP/1.1 200 OK
;;     ;; DATE: Thu, 30 Nov 2008 17:58:12 GMT
;;     ;; CONNECTION: keep-alive
;;     ;; SERVER: Core-serveR - www.core.gen.tr
;;     ;; CONTENT-LENGTH: 5192
;;     ;; CONTENT-TYPE: text/html

;;     ;; ")
;;     ;;     (write-stream stream *5k*)
;;     ;; (when (not (eq 'head (http-request.method request)))
;;     ;;   (commit-stream stream)
;;     ;;   ;; (write-stream stream (slot-value (http-response.stream response) '%buffer))
;;     ;;   )
;;     ;; (describe stream)
;;     ))

;; (defclass nio-custom-http-peer (nio-http-peer-mixin custom-http-peer)
;;   ())

;; (defclass+ nio-http-server (web-server nio-socket-server logger-server)
;;   ()
;;   (:default-initargs :peer-class '(nio-custom-http-peer)))



;; (defmethod render-404 ((server http-server) (request http-request) (response http-response))
;;   (setf (http-response.status-code response) (make-status-code 404))
;;   (rewind-stream (http-response.stream response))
;;   (checkpoint-stream (http-response.stream response))
;;   (with-html-output (http-response.stream response)
;;     (<:html
;;      (<:body
;;       "Core-serveR - URL Not Found")))
;;   response)

;; (defmethod/cc2 render-error ((self http-server) stream)
;;   "Renders a generic server error response to 'stream'"
;;   (let ((response (make-response stream)))
;;     (checkpoint-stream stream)
;;     (setf (http-response.status-code response) (make-status-code 500))
;;     (http-response-headers! stream response)
;;     (char! stream #\Newline)
;;     (with-html-output stream (<:html (<:body "An error condition occured and ignored.")))
;;     (commit-stream stream)))

;; (defmethod/cc2 render-response ((self http-server) response request)
;;   "Renders HTTP 'response' to 'stream'"
;;   (let ((accept-encoding (http-request.header request 'accept-encoding))
;; 	(stream (http-response.stream response)))
;;     (assert (eq 0 (current-checkpoint stream)))
;;     (flet ((write-headers ()
;; 	     (let ((header-stream
;; 		    (make-core-stream
;; 		     (slot-value (http-request.stream request) '%stream))))
;; 	       (checkpoint-stream header-stream)
;; 	       (http-response-headers! header-stream response)
;; 	       (char! header-stream #\Newline)
;; 	       (commit-stream header-stream))))
      
      
;;       (cond
;; 	((member "gzip" accept-encoding :key #'car :test #'string=)
;; 	 (let* ((content (slot-value stream '%write-buffer))
;; 		(content-length (length content)))
;; 	   (rewind-stream stream)
;; 	   (labels ((do-finish ()
;; 		      (let ((content-length (length (slot-value stream '%write-buffer))))
;; 			(http-response.add-entity-header response 'content-length
;; 							 content-length))		      
;; 		      (http-response.add-entity-header response 'content-encoding 'gzip)
;; 		      (write-headers)
;; 		      (commit-stream stream))
;; 		    (callback (stream)
;; 		      (lambda (buffer end)
;; 			(cond
;; 			  ((< end (length buffer))
;; 			    (write-stream stream (subseq buffer 0 end))
;; 			   (do-finish))
;; 			  (t
;; 			   (write-stream stream buffer))))))
;; 	     (with-compressor (compressor 'gzip-compressor :callback (callback stream))
;; 	       (checkpoint-stream stream)
;; 	       (compress-octet-vector (make-array content-length
;; 						  :initial-contents content 
;; 						  :element-type '(unsigned-byte 8))
;; 				      compressor)))))
;; 	(t
;; 	 (http-response.add-entity-header response 'content-length
;; 					  (length
;; 					   (slot-value stream '%write-buffer)))
;; 	 (write-headers)
;; 	 (commit-stream stream))))))
      
