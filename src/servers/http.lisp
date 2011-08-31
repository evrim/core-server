(in-package :core-server)

;; +-------------------------------------------------------------------------
;; | HTTP Server
;; +-------------------------------------------------------------------------
(defclass+ http-server (web-server socket-server logger-server)
  ((applications :accessor server.applications :initform '())
   (root-application :accessor server.root-application :initform nil))
  (:default-initargs :port 3001 :peer-class '(http-unit)))

;; (defclass nio-custom-http-peer (nio-http-peer-mixin custom-http-peer)
;;   ())

;; (defclass+ nio-http-server (web-server nio-socket-server logger-server)
;;   ()
;;   (:default-initargs :peer-class '(nio-custom-http-peer)))

(defvar +default-encoding-for-remote-mimes+
  :utf-8 "FIXME: Browser shoudl supply in which encoding did it encode data.")

(defmethod/cc2 parse-request ((server http-server) (stream core-stream))
  "Returns a fresh RFC 2616 HTTP Request object parsing from 'stream',
nil if stream data is invalid"
  (multiple-value-bind (peer-type method uri version general-headers
				  request-headers entity-headers unknown-headers)
      (http-request-headers? stream)
    (if method
	(let ((request (make-instance 'http-request :stream stream)))
	  (let ((content-type (cadr (assoc 'content-type entity-headers)))
		(content-length (cadr (assoc 'content-length entity-headers))))
	    (flet ((do-process (stream)
		     (cond
		       ;; content-type = '("multipart" "form-data")
		       ((and (string-equal "multipart" (car content-type))
			     (string-equal "form-data" (cadr content-type))
			     (> content-length 0))
			;; eat lineer whayt spaces.
			(lwsp? stream)

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
		       ((and (string-equal "text" (car content-type))
			     (string-equal "json" (cadr content-type)))
			(let ((hash-table (json? stream)))
			  (if (typep hash-table 'hash-table)
			      (setf (uri.queries uri) (append (uri.queries uri)
							      (hash-to-alist hash-table))))))
		       ;; content-type = '("application" "x-www-form-urlencoded")
		       ((and (string-equal "application" (car content-type))
			     (string-equal "x-www-form-urlencoded" (cadr content-type))
			     (>  content-length 0))
			;; eat lineer whayt spaces.
			(lwsp? stream)
			(setf (uri.queries uri) (append (uri.queries uri)					       
							(x-www-form-urlencoded? stream)))))))
	      (do-process (if (and content-length (> content-length 0))
			      (make-bounded-stream stream content-length)
			      stream))))
	  (setf (http-message.general-headers request) general-headers
		(http-message.unknown-headers request) unknown-headers
		(http-request.headers request) request-headers
		(http-request.entity-headers request) entity-headers
		(http-request.uri request) uri
		(http-request.method request) method
		(http-message.version request) version
		(http-request.peer-type request) peer-type)
	  request))))


(defmethod/cc2 render-404 ((server http-server) (request http-request) (response http-response))
  (setf (http-response.status-code response) (make-status-code 404))
  (rewind-stream (http-response.stream response))
  (checkpoint-stream (http-response.stream response))
  (with-html-output (http-response.stream response)
    (<:html
     (<:body
      "Core-serveR - URL Not Found")))
  response)

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

(defmethod/cc2 eval-request ((server http-server) (request http-request))
  (let* ((stream (http-request.stream request))
	 (response (make-response (make-core-stream
				   (slot-value stream '%stream))))
	 (host (caadr (assoc 'HOST (http-request.headers request))))
	 (app-name (caar (uri.paths (http-request.uri request)))))

    (checkpoint-stream (http-response.stream response))
    (prog1
	(cond
	  ;; dispatch by app-name like http://servername/app-fqdn/js.core
	  ((any #'(lambda (app)
		    (when (string= app-name (web-application.fqdn app)) 
		      (pop (uri.paths (http-request.uri request)))
		      (dispatch app request response)))
		(server.applications server))
	   response)
	  ;; dispatch by hostname like http://servername/js.core
	  ((and (stringp host)
		(any #'(lambda (app)
			 (when (string= host (web-application.fqdn app)) 
			   (dispatch app request response)))
		     (server.applications server)))
	   response)
	  ((and (slot-value server 'root-application)
		(dispatch (slot-value server 'root-application)
			  request response))
	   response)
	  ;; catch-all via 404
	  (t
	   (progn
	     (log-me server 'eval-request
		     (format nil "request uri: ~A" (http-request.uri request)))
	     (render-404 server request response)))))))

(defmethod/cc2 render-error ((self http-server) stream)
  "Renders a generic server error response to 'stream'"
  (let ((response (make-response stream)))
    (checkpoint-stream stream)
    (setf (http-response.status-code response) (make-status-code 500))
    (http-response-headers! stream response)
    (char! stream #\Newline)
    (with-html-output stream (<:html (<:body "An error condition occured and ignored.")))
    (commit-stream stream)))

(defmethod/cc2 render-response ((self http-server) response request)
  "Renders HTTP/Mod-Lisp 'response' to 'stream'"
  (let ((accept-encoding (http-request.header request 'accept-encoding))
	(stream (http-response.stream response)))
    (assert (eq 0 (current-checkpoint stream)))
    (flet ((write-headers ()
	     (let ((header-stream
		    (make-core-stream
		     (slot-value (http-request.stream request) '%stream))))
	       (checkpoint-stream header-stream)
	       (cond
		 ((eq 'mod-lisp (http-request.peer-type request))
		  ;; Renders Mod-Lisp HTTP 'response' to 'stream'
		  (mod-lisp-response-headers! header-stream response)
		  (string! stream "end"))
		 (t
		  ;; Renders RFC 2616 HTTP 'response' to 'stream'
		  (http-response-headers! header-stream response)))
	       (char! header-stream #\Newline)
	       (commit-stream header-stream))))
      
      
      (cond
	((member "gzip" accept-encoding :key #'car :test #'string=)
	 (let* ((content (slot-value stream '%write-buffer))
		(content-length (length content)))
	   (rewind-stream stream)
	   (labels ((do-finish ()
		      (let ((content-length (length (slot-value stream '%write-buffer))))
			(http-response.add-entity-header response 'content-length
							 content-length))		      
		      (http-response.add-entity-header response 'content-encoding 'gzip)
		      (write-headers)
		      (commit-stream stream))
		    (callback (stream)
		      (lambda (buffer end)
			(cond
			  ((< end (length buffer))
			    (write-stream stream (subseq buffer 0 end))
			   (do-finish))
			  (t
			   (write-stream stream buffer))))))
	     (with-compressor (compressor 'gzip-compressor :callback (callback stream))
	       (checkpoint-stream stream)
	       (compress-octet-vector (make-array content-length
						  :initial-contents content 
						  :element-type '(unsigned-byte 8))
				      compressor)))))
	(t
	 (http-response.add-entity-header response 'content-length
					  (length
					   (slot-value stream '%write-buffer)))
	 (write-headers)
	 (commit-stream stream))))))
      

;;--------------------------------------------------------------------------
;; Server Protocol Implementation
;;--------------------------------------------------------------------------
(defmethod register ((self http-server) (app http-application))
  (setf (server.applications self)
	(sort (cons app
		    (remove (web-application.fqdn app)
			    (server.applications self)
			    :test #'equal :key #'web-application.fqdn))
	      #'> :key (lambda (app) (length (web-application.fqdn app)))))
  (setf (application.server app) self))

(defmethod register ((self http-server) (app root-http-application-mixin))
  (setf (slot-value self 'root-application) app)
  self)

(defmethod unregister ((self http-server) (app http-application))
  (setf (server.applications self)
	(remove (web-application.fqdn app)
		(server.applications self)
		:test #'equal :key #'web-application.fqdn)))

(defmethod unregister ((self http-server) (app root-http-application-mixin))
  (setf (slot-value self 'root-application) nil)
  self)

(defmethod find-application ((self http-server) fqdn)
  (find fqdn (server.applications self)
	:key #'web-application.fqdn :test #'equal))

;; +-------------------------------------------------------------------------
;; | HTTP Unit
;; +-------------------------------------------------------------------------
(defclass http-unit (stream-peer)
  ()
  (:default-initargs :name "Http Peer Handling Unit")
  (:documentation "HTTP Peer - This peer handles HTTP requests and
evaulates to a HTTP response. Its' server is an instance of http-server"))

(defmethod/unit handle-stream :async-no-return ((peer http-unit) (stream core-stream) address)
  (flet ((handle-error (condition)
	   (if (typep condition 'sb-int::simple-stream-error)    
	       (return-from handle-stream nil))
	   (flet ((ignore-error (unit)
		    (declare (ignore unit))
		    (render-error (peer.server peer) stream)
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
	    (close-stream stream))))))

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

(defmethod start ((self http-server))
;; ;;   (thread-kill (s-v '%socket-thread))
;; ;;   (setf (s-v '%socket-thread) nil)
;;   (let ((listenfd (socket-file-descriptor (s-v '%socket))))
;;     (core-ffi::set-nonblock listenfd)
;;     (handle-accept (car (s-v '%peers)) listenfd (s-v '%peers)))
  )

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

(deftrace http-server
    '(handle-stream dispatch render-error render-response eval-request parse-request
      handle-stream4 run add-fd del-fd ;; receive-messages
      receive-events
    ;;   render-headers
      make-response;;  render-http-headers
;;       render-mod-lisp-headers
      ))

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
