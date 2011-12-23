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

(in-package :core-server)

;; +-------------------------------------------------------------------------
;; | Http Application
;; +-------------------------------------------------------------------------

;; --------------------------------------------------------------------------
;; HTTP Constants
;; --------------------------------------------------------------------------
(defvar +continuation-query-name+ "k" "Query key for continuations")
(defvar +session-query-name+ "s" "Query key for sessions")
(defvar +session-timeout+ (* 12 3600)
  "Session timeout in milisecons, half day")
(defvar +invalid-session+ "invalid-session-id")
(defvar +invalid-continuation-id "invalid-continuation-id")

;; --------------------------------------------------------------------------
;; HTTP Special Variables
;; --------------------------------------------------------------------------
(defvar +context+ nil "A special variable that holds HTTP context")
(defvar +k+ nil "A special variable that holds current continuation")

;; --------------------------------------------------------------------------
;; HTTP Session
;; --------------------------------------------------------------------------
(defclass http-session ()
  ((id :reader session.id :initarg :id :initform (random-string 8))
   (continuations :reader session.continuations
		  :initform (make-hash-table :test #'equal :synchronized t)) 
   (timestamp :accessor session.timestamp :initform (get-universal-time))
   (data :accessor session.data
	 :initform (make-hash-table :test #'equal
				    :synchronized t))))

(defprint-object (self http-session :identity t :type t)
  (format t "~A" (session.id self)))

(defun make-new-session (&optional (id (random-string 8)))
  "HTTP Session Constructor"
  (make-instance 'http-session :id id))

(defmethod find-session-id ((request http-request))
  "Returns session id string provided in request query or by cookie that has set before"
  (or (uri.query (http-request.uri request) +session-query-name+)
      (aif (http-request.cookie request +session-query-name+)
	   (cookie.value it))))

(defmethod find-continuation ((session http-session) id)
  "Returns the continuation associated with 'id'"
  (aif (gethash id (session.continuations session))
       (return-from find-continuation (values it session))))

(defmacro update-session (key val &optional (session '(context.session +context+)))
  "Update a session variable."
  `(setf (gethash ,key (session.data ,session)) ,val))

(defmacro query-session (key &optional (session '(context.session +context+)))
  "Query a session variable."
  `(gethash ,key (session.data ,session)))

;; --------------------------------------------------------------------------
;; HTTP Context
;; --------------------------------------------------------------------------
(defclass http-context ();;    (core-cps-stream)
  ((request :accessor context.request :initarg :request :initform nil)
   (response :accessor context.response :initarg :response :initform nil)
   (session :accessor context.session :initarg :session :initform nil)
   (application :accessor context.application :initarg :application :initform nil)
   (continuation :accessor context.continuation :initform nil)
   (returns :accessor context.returns :initform nil)))

(defun make-new-context (application request response session)
  "Returns a new HTTP context having parameters provided"
  (make-instance 'http-context :application application :request request
		 :response response :session session))

(defmethod copy-context ((self http-context))
  "Returns a copy of the HTTP context 'self'"
  (let ((s (make-instance 'http-context)))
    (setf (slot-value s 'application) (s-v 'application)
 	  (slot-value s 'session) (s-v 'session)
 	  (slot-value s 'continuation) (s-v 'continuation))
     s))

(defmethod context.session ((self http-context))
  "Returns the session of the HTTP context 'self', creates if none exists"
  (acond
   ((slot-value self 'session)
    (setf (session.timestamp it) (get-universal-time))
    it)
   (t
    (let ((session (make-new-session)))
      (setf (context.session self) session)      
      (setf (gethash (session.id session)
		     (http-application.sessions (context.application self)))
	    session)
      session))))

(defmethod context.remove-action ((self http-context) k-url)
  (remhash k-url (session.continuations (context.session self))))

(defmethod context.remove-current-action ((self http-context))
  (context.remove-action +context+
			 (http-request.query (context.request self)
					     +continuation-query-name+)))

;; --------------------------------------------------------------------------
;; Methods that add "Session" Cookie to Response
;; --------------------------------------------------------------------------
(defmethod (setf context.session) :after ((session t) (self http-context))
  (prog1 session
    (http-response.add-cookie (context.response self)
			      (make-cookie +session-query-name+ ""
					   :comment "Core Server Session Cookie"
					   :max-age 0))))

(defmethod (setf context.session) :after ((session http-session) (self http-context))
  (prog1 session
    (http-response.add-cookie (context.response self)
			      (make-cookie +session-query-name+ (session.id session)
					   :comment "Core Server Session Cookie"))))


;;+--------------------------------------------------------------------------
;;| HTTP Application Metaclass
;;+--------------------------------------------------------------------------
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defclass http-application+ (class+)
    ((handlers :initarg :handlers :accessor http-application+.handlers
	       :initform nil
	       :documentation "A list that contains URLs that this application handles")
     (scanner-cache :initform (make-hash-table))))

  (defmethod validate-superclass ((class http-application+) (super standard-class)) t)
  (defmethod validate-superclass ((class standard-class) (super http-application+)) nil)

  (defmethod http-application+.handlers ((self http-application+))
    ;; (let ((cache (slot-value self 'scanner-cache)))
    ;;   (mapcar (lambda (handler)
    ;; 		(aif (gethash (car handler) cache)
    ;; 		     (cons (car handler) it)
    ;; 		     (cons (car handler)
    ;; 			   (setf (gethash (car handler) cache)
    ;; 				 (cl-ppcre:create-scanner (cdr handler))))))
    ;; 	      (uniq (nreverse
    ;; 		     (reduce #'append
    ;; 			     (mapcar (rcurry #'slot-value 'handlers)
    ;; 				     (filter (lambda (a) (if (typep a 'http-application+) a))
    ;; 					     (class-superclasses self)))))
    ;; 		    :key #'car)))
    (uniq (nreverse
	   (copy-list
	    (reduce #'append
		    (mapcar (rcurry #'slot-value 'handlers)
			    (filter (lambda (a) (if (typep a 'http-application+) a))
				    (class-superclasses self))))))
	  :key #'car))

  (defmethod add-handler ((application+ http-application+) method-name url)
    (setf (slot-value application+ 'handlers)
	  (cons (cons method-name url)
		(remove-handler application+ method-name))))

  (defmethod remove-handler ((application+ http-application+) method-name)
    (prog1 (setf (slot-value application+ 'handlers)
		 (remove method-name (slot-value application+ 'handlers) :key #'car))
      (remhash method-name (slot-value application+ 'scanner-cache))))

  (defmethod class+.ctor ((application+ http-application+))
    nil))

;;+--------------------------------------------------------------------------
;;| HTTP Application
;;+--------------------------------------------------------------------------
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defclass http-application (web-application)
    ((sessions :accessor http-application.sessions
	       :initform (make-hash-table :test #'equal
					  :synchronized t)
	       :documentation "A hash-table that holds sessions"))
    ;; (:default-initargs :directory nil)
    (:documentation "HTTP Application Class")
    (:metaclass http-application+)))

(defmethod print-object ((self http-application) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (if (typep self 'server)
	(format stream "FQDN:\"~A\" is ~A running"
		(web-application.fqdn self)
		(if (status self) "" "*not*"))
	(format stream "FQDN:\"~A\"" (web-application.fqdn self)))))

(defmethod start ((self http-application))
  (clrhash (slot-value self 'sessions)))

(defmethod snapshot ((self http-application))
  (clrhash (slot-value self 'sessions))
  (call-next-method self))

(defmethod reset-sessions ((self http-application))
  (clrhash (slot-value self 'sessions)))

;; +-------------------------------------------------------------------------
;; | Root Http Application (App to serve at /)
;; +-------------------------------------------------------------------------
(defclass+ root-http-application-mixin ()
  ())

;; --------------------------------------------------------------------------
;; defapplication Macro: Just adds http-application+ metaclass
;; --------------------------------------------------------------------------
(defmacro defapplication (name supers slots &rest rest)
  (let ((dispatchers (reduce #'append
                             (mapcar (rcurry #'slot-value 'handlers)
                                     (filter
				      (lambda (a)
					(if (typep a 'http-application+)
					    a))
				      (mapcar #'find-class supers))))))
    `(defclass+ ,name ,supers
       ,slots
       ,@rest
       (:metaclass http-application+)
       ;; (:handlers ,@dispatchers)
       )))

;; +-------------------------------------------------------------------------
;; | HTTP Application Interface
;; +-------------------------------------------------------------------------
(defmethod web-application.serve-url ((self http-application) (req t))
  (format nil "/~A/TESTREQUEST" (web-application.fqdn self)))

(defmethod web-application.base-url ((self http-application)
				     (eq null))
  (format nil "/~A/" (web-application.fqdn self)))

(defmethod web-application.base-url ((self http-application)
				     (req http-request))
  (format nil "/~A/" (web-application.fqdn self)))

(defmethod web-application.serve-url ((self http-application)
				      (req http-request))
  (format nil "/~A/~A" (web-application.fqdn self)
	  (with-core-stream (s "")
	    (mapc #'(lambda (path)
		      (char! s core-server::+uri-path-seperator+)
		      (string! s (car path))
		      (mapc #'(lambda (path)
				(core-server::char!
				 s core-server::+uri-segment-seperator+)
				(core-server::string! s path))
			    (cdr path)))
		  (core-server::uri.paths (http-request.uri req)))	    
	    (return-stream s))))

(defmethod find-session ((application http-application) id)
  "Returns the session associated with 'id'"
  (aif (gethash id (http-application.sessions application))
       (values it application)))

(defmethod map-session (lambda (application http-application))
  (mapcar (lambda (k v)
	    (funcall lambda k v))
	  (hash-table-keys (slot-value application 'sessions))
	  (hash-table-values (slot-value application 'sessions))))

(defmethod render-404 ((application http-application)
		       (request http-request) (response http-response))
  "Override this method to have custom 404 page for your application."
  (render-404 (application.server application) request response))

(defmethod render-file ((application http-application)
			(request http-request) (response http-response))
  "Default directory handler which serves static files"
  (let ((htdocs-path (web-application.htdocs-pathname application))
	(paths (let ((tmp (or (uri.paths (http-request.uri request))
			      '(("")))))
		 (if (equal (caar tmp) "")
		     '(("index.html"))
		     tmp))))

    ;; HTDOCS Not found
    (if (or (null htdocs-path) (not (probe-file htdocs-path)))
	(return-from render-file nil))
    
    (let* ((file-and-ext (pathname (caar (last paths))))
	   (path (append '(:relative) (mapcar #'car (butlast paths))))
	   (output (http-response.stream response))
	   (abs-path (merge-pathnames
		      (merge-pathnames (make-pathname :directory path)
				       file-and-ext)
		      htdocs-path))
	   (mime-type (mime-type abs-path)))

      ;; File Not Found
      (if (not (probe-file abs-path))
	  (return-from render-file nil))

      ;; Directory Request, we do not serve it, yet. 
      (if (cl-fad:directory-exists-p abs-path)
	  (return-from render-file nil))

      (http-response.set-content-type response (split "/" mime-type))
      
      (with-open-file (input abs-path :element-type '(unsigned-byte 8)
			     :direction :input)
	(let ((seq (make-array (file-length input)
			       :element-type 'unsigned-byte)))
	  (read-sequence seq input)
	  (write-stream output seq)))
      t)))

(defmethod dispatch ((self http-application) (request http-request)
		     (response http-response))
  "Dispatch 'request' to 'self' application with empty 'response'"
  (let ((session (gethash (find-session-id request)
			  (http-application.sessions self)))
	(k-arg (uri.query (http-request.uri request)
			  +continuation-query-name+)))
    (acond
     ((and session (gethash k-arg (session.continuations session)))
      (log-me (application.server self) 'http-application	
	      (format nil "fqdn: ~A, k-url: ~A"
		      (web-application.fqdn self)
		      (uri.query (http-request.uri request)
				 +continuation-query-name+)))
      (or (funcall it request response) t))
     ((and session k-arg)
      (log-me (application.server self) 'http-application
	      (format nil "fqdn: ~A, invalid k-url: ~A"
		      (web-application.fqdn self)
		      (http-request.uri request)))
      (render-404 self request response))
     ((any #'(lambda (handler)
	       (aif (caar (uri.paths (http-request.uri request)))
		    (let ((uri (make-uri :paths
					 (uri.paths
					  (http-request.uri request)))))
		      (and (cl-ppcre:scan-to-strings
			    (cl-ppcre:create-scanner (cdr handler))
			    (with-core-stream (s "")
			      (uri! s uri)
			      (return-stream s)))
			   (car handler)))))
	   (reverse (http-application+.handlers (class-of self))))
      (log-me (application.server self) 'http-application
	      (format nil "fqdn: ~A, static-handler:: ~A"
		      (web-application.fqdn self)
		      (http-request.uri request)))
      (or (funcall it self (make-new-context self request response session))
	  t))
     ((render-file self request response)
      (log-me (application.server self) 'http-application
	      (format nil "fqdn: ~A, static-url: ~A"
		      (web-application.fqdn self)
		      (http-request.uri request)))
      t)
     (t
      (render-404 self request response)))))

(defmethod gc ((self http-application))
  "Garbage collector for HTTP application, removes expired sessions/continuations"
  (let* ((sessions (http-application.sessions self)))
    (mapc (rcurry #'remhash sessions)
	  (let (expired)
	    (maphash #'(lambda (k v)
			 (when (> (- (get-universal-time)
				     (session.timestamp v))
				  +session-timeout+)
			   (push k expired)))
		     sessions)
	    expired))))

(defmethod dispatch :around ((application http-application)
			     (request http-request) (response http-response))
  (when (> (random 100) 40)
    (gc application))
  
  (call-next-method))

;; --------------------------------------------------------------------------
;; Overriden get-directory method: This allows us to use default
;; database directory for database driven http applications.
;; --------------------------------------------------------------------------
(defmethod database.directory ((application http-application))
  (ensure-directories-exist
   (merge-pathnames
    (make-pathname :directory (list :relative "var"
				    (web-application.fqdn application) "db"))
    (bootstrap::home))))

;; +-------------------------------------------------------------------------
;; | HTTP Macros
;; +-------------------------------------------------------------------------
(defmacro with-query (queries request &body body)
  "Executes 'body' while binding 'queries' from 'request'"
  `(let ,(mapcar #'(lambda (p)
		     `(,(car p)
			(or (uri.query (http-request.uri ,request) ,(cadr p))
			    ,(caddr p))))
		 queries)
     ,@body))

(defmacro with-context (context &body body)
  "Executes 'body' with HTTP context bind to 'context'"
  `(with-call/cc
     (let ((+context+ ,context))
       (declare (special +context+))
       ,@body)))

;; --------------------------------------------------------------------------
;; defhandler Macro: Defines a static url handler
;; --------------------------------------------------------------------------
(defmacro defhandler (url ((application application-class) &rest queries)
		      &body body)
  "Defines an entry point/url handler to the application-class"
  (let ((handler-symbol (intern (string-upcase url))))
    (assert (stringp url))
    (with-unique-names (context)
      (setf context (intern (symbol-name context)))
      `(progn
	 (eval-when (:load-toplevel :compile-toplevel :execute)
	   (add-handler (find-class ',application-class)
			',handler-symbol ,url))
	 (defmethod ,handler-symbol
	     ((,application ,application-class) (,context http-context))
	   (prog1 (with-context ,context
		    (with-html-output (http-response.stream
				       (context.response +context+))
		      (with-query ,queries (context.request ,context)
			,@body)))
	     (setf (context.request ,context) nil
		   (context.response ,context) nil)))))))

(defmacro defhandler/js (url ((application application-class) &rest queries)
			 &body body)
  `(defhandler ,url ((,application ,application-class) ,@queries)
     (javascript/suspend
      (lambda (stream)
	(let ,(mapcar (lambda (a)
			`(,a (json-deserialize ,a)))
		      (mapcar #'car queries))
	  (with-js ,(mapcar #'car queries) stream
	    ,@body)
	  nil)))))

(defmacro defurl (application regexp-url queries &body body)
  "Backward compat macro"
  (let ((class-name (class-name (class-of (symbol-value application)))))
    `(defhandler ,regexp-url ((application ,class-name)
			      (context http-context) ,@queries)
       ,@body)))

;; +-------------------------------------------------------------------------
;; | CPS Style Web Framework
;; +-------------------------------------------------------------------------
(defun escape (&rest values)
   (funcall (apply (arnesi::toplevel-k) values))
   (break "You should not see this, call/cc must be escaped already."))

(defmacro send/suspend (&body body)
  "Saves current continuation, executes 'body', terminates."
  (with-unique-names (result)
    `(let ((,result (multiple-value-list
		     (let/cc k
		       (setf (context.continuation +context+) k)
		       (with-html-output (http-response.stream
					  (context.response +context+))
			 ,@body)
		       (setf (context.response +context+) nil
			     (context.request +context+) nil)
		       (escape (reverse (context.returns +context+)))
		       (break "send/suspend failed.")))))
       (setf (context.request +context+) (context.request (car ,result))
	     (context.response +context+) (context.response (car ,result))
	     (context.continuation +context+) nil)
       (apply #'values (cdr ,result)))))

(defmacro send/forward (&body body)
  (with-unique-names (result)
    `(progn
       (clrhash (session.continuations (context.session +context+)))
       (let ((,result (multiple-value-list (send/suspend ,@body))))
	 (send/suspend
	   (setf (http-response.status-code (context.response +context+))
		 '(301 . "Moved Permanently"))
	   (http-response.add-response-header
	    (context.response +context+)
	    'location
	    (action/url ()
	      (answer (apply #'values ,result)))))))))

(defun/cc send/redirect (url)
  (let ((response (context.response +context+)))
    (setf (http-response.status-code response) '(302 . "Found"))
    (http-response.add-response-header response 'location url)
    nil))

(defmacro send/finish (&body body)
  `(with-html-output (http-response.stream (context.response +context+))
     (clrhash (session.continuations (context.session +context+)))
     ,@body))

(defvar +action-hash-override+ nil)

(defmacro action/hash (parameters &body body)
  "Registers a continuation at run time and binds 'parameters' while
executing 'body'"
  (with-unique-names (name context)
    `(if +context+
	 (let* ((,name (or +action-hash-override+
			   (format nil "act-~A" (make-unique-random-string 8))))
		(,context (copy-context +context+))
		(kont (lambda (req rep &optional ,@(mapcar #'car parameters))
			(assert (not (null req)))
			(assert (not (null rep)))
			(let ((+context+ ,context))
			  (setf (context.request +context+) req
				(context.response +context+) rep)			
			  (with-query ,(mapcar (lambda (param)
						 (reverse
						  (cons (car param)
							(reverse param))))
					       parameters) (context.request +context+)
			    (with-html-output (http-response.stream (context.response +context+))
			      ,@body))))))	   
	   (prog1 ,name	   
	     (setf (gethash ,name (session.continuations (context.session ,context))) kont)
	     (setf (context.returns +context+)
		     (cons (cons ,name (lambda ,(mapcar #'car parameters)
					 (funcall kont
						  (make-instance 'http-request
								 :uri (make-instance 'uri))
						  (make-response *core-output*)
						  ,@(mapcar #'car parameters))))
			   (remove-if #'(lambda (x) (equal x ,name)) (context.returns ,context) :key #'car)))))
	 "invalid-function-hash")))

(defmacro function/hash (parameters &body body)
  "Registers a continuation at macro-expansion time and binds
'parameters' while executing 'body'"
  (with-unique-names (context)
    (let ((name (format nil "fun-~A" (make-unique-random-string 3))))
      `(if +context+	   
	   (let* ((,context (copy-context +context+))
		  (kont (let ((+context+ ,context))
			  (lambda (req rep &optional ,@(mapcar #'car parameters))			    
			    (setf (context.request +context+) req
				  (context.response +context+) rep)
			    (with-query ,(mapcar (lambda (param)
						   (reverse
						    (cons (car param)
							  (reverse param))))
						 parameters) (context.request +context+)
			      (with-html-output (http-response.stream (context.response +context+))
				,@body))))))
	     (prog1 ,name
	       (setf (gethash ,name (session.continuations (context.session ,context))) kont)
	       (setf (context.returns ,context)
		     (cons (cons ,name (lambda ,(mapcar #'car parameters)
					 (funcall kont
						  (make-instance 'http-request
								 :uri (make-instance 'uri))
						  (make-response *core-output*)
						  ,@(mapcar #'car parameters))))
			   (remove-if #'(lambda (x) (equal x ,name)) (context.returns ,context) :key #'car)))))
	   "invalid-action-hash"))))

(defmacro function/url (parameters &body body)
  "Returns URI representation of function/hash"
  `(format nil "?~A:~A$~A:~A"
	  +session-query-name+ (if +context+
				   (session.id (context.session +context+))
				   +invalid-session+)
	  +continuation-query-name+ (function/hash ,parameters ,@body)))

(defmacro action/url (parameters &body body)
  "Returns URI representation of action/hash"
  `(format nil "?~A:~A$~A:~A"
	  +session-query-name+ (if +context+
				   (session.id (context.session +context+))
				   +invalid-session+)
	  +continuation-query-name+ (action/hash ,parameters ,@body)))

(defmacro answer (&rest values)
  "Continues from the continuation saved by action/hash or function/hash"
  `(if (null (context.continuation +context+))
       (error "Surrounding send/suspend not found, can't answer")
       (kall (context.continuation +context+) +context+ ,@values)))

(defmacro answer/dispatch (to &rest arguments)
  `(answer (list ,to ,@arguments)))

(defmacro answer/url (parameters)
  `(action/url ,parameters
     (answer (list ,@(mapcar #'car parameters)))))

(defun/cc javascript/suspend (lambda)
  "Javascript version of send/suspend, sets content-type to
application/javascript"
  (http-response.set-content-type (context.response +context+)
				  '("application" "javascript" ("charset" "UTF-8")))
  (send/suspend
    (prog1 nil
      (funcall lambda
	       (if (application.debug (context.application +context+))
		   (make-indented-stream
		    (http-response.stream (context.response +context+)))
		   (make-compressed-stream
		    (http-response.stream (context.response +context+))))))))

(defmacro json/suspend (&body body)
  "Json version of send/suspend, sets content-type to text/json"
  `(progn
     (http-response.set-content-type (context.response +context+)
				     '("text" "json" ("charset" "UTF-8")))
     (send/suspend ,@body)))

(defmacro xml/suspend (&body body)
  "Xml version of send/suspend, sets content-type to text/xml"
  `(progn
     (http-response.set-content-type (context.response +context+)
				     '("text" "xml" ("charset" "UTF-8")))
     (send/suspend ,@body)))

(defmacro css/suspend (&body body)
  "Css version of send/suspend, sets content-type to text/css"
  `(progn
     (http-response.set-content-type (context.response +context+)
				     '("text" "css" ("charset" "UTF-8")))
     (send/suspend ,@body)))

;; +-------------------------------------------------------------------------
;; | HTTP Application Testing Utilities
;; +-------------------------------------------------------------------------
(defmacro with-test-context ((context-var uri application) &body body)
  "Executes 'body' with context bound to 'context-var'"
  `(let ((,context-var (make-new-context ,application
			 (make-instance 'http-request :uri ,uri)
			 (make-response (make-indented-stream *core-output*))
			 nil)))
     ,@body))

(defun kontinue (number result &rest parameters)
  "Continues a contination saved by function/hash or action/hash"
  (let ((fun (cdr (nth number result))))
    (if (null fun)
	(error "Continuation not found, please correct name.")
	(apply fun parameters))))

(defun test-url (url application)
  "Conventional macro to test urls wihout using a browser. One may
provide query parameters inside URL as key=value"
  (assert (stringp url))
  (let ((uri (uri? (make-core-stream url))))    
    (dispatch application
	      (make-instance 'http-request
			     :uri uri
			     :stream *core-output*)
	      (make-response *core-output*))))

(defparameter +etag-key+ (string-to-octets "gLZntebnfM" :utf-8))
(defmacro with-cache (etag-key &body body)
  `(progn
     (let ((response (context.response +context+))
	   (timestamp ,etag-key)
	   (request (context.request +context+)))       
       (labels ((calculate-etag ()
		  (hmac +etag-key+ (format nil "~A" timestamp) :sha1))
		(add-cache-headers ()
		  ;; Date:Sat, 05 Nov 2011 22:05:05 GMT
		  ;; ETag: "8eb52-e2b-4b0dbed652d80"
		  (http-response.remove-header response 'pragma)
		  (http-response.remove-header response 'cache-control)
		  (http-response.remove-header response 'expires)
		  (http-response.add-general-header response 'date (get-universal-time))
		  (http-response.add-response-header response 'etag (cons (calculate-etag) nil))
		  (http-response.add-entity-header response 'expires (+ (get-universal-time) 50000))
		  (http-response.add-entity-header response 'last-modified timestamp)
		  ;; (let* ((agent (http-request.header request 'user-agent))
		  ;; 	 (str (cdr (assoc 'string agent)))
		  ;; 	 (msie (if (search "MSIE" str) t)))
		  ;;   (if msie
		  ;; 	(http-response.add-general-header response 'cache-control
		  ;; 					  (list ;; 'public 'must-revalidate 'no-transform (cons 'max-age 0)
		  ;; 					   'no-cache))
		  ;; 	(http-response.add-general-header response 'cache-control
		  ;; 					  (list ;; 'private 'must-revalidate 'no-transform
		  ;; 					   (cons 'max-age 50000)))))
		  (http-response.add-general-header response 'cache-control
		  				    (list 'public ;; 'must-revalidate 'no-transform
							  (cons 'max-age 50000))))
		(render-response ()
		  (add-cache-headers)
		  ,@body))
	 ;; If-Modified-Since:Thu, 03 Nov 2011 22:15:34 GMT
	 ;; If-None-Match: "8eb50-b16-4b0dbed652d80"
	 (let* ((date (http-request.header request 'if-modified-since))
		(etag (http-request.header request 'if-none-match)))
	   (cond
	     ((and timestamp (> timestamp 0) etag)
	      (if (equal (calculate-etag) (caar etag))
		  (prog1 nil
		    (add-cache-headers)
		    (setf (http-response.status-code response)
		  	  (core-server::make-status-code 304)))
		  (render-response)))
	     (t (render-response))))))))

(defparameter +library.core-timestamp+ (get-universal-time))
(defhandler "library.core" ((self http-application))
  (javascript/suspend
   (lambda (s)
     (flet ((foo () +library.core-timestamp+))
       (with-cache (foo)
     	 (core-server::core-library! s))))))

(defhandler "multipart.core" ((self http-application) (action "action")
			      (hash "__hash"))
  (let* ((uri (uri? (make-core-stream (json-deserialize action))))
	 (action-s (uri.query uri +session-query-name+))
	 (action-k (uri.query uri +continuation-query-name+))
	 (paths (uri.paths uri))
	 (stream (make-core-stream "")))
    (write-stream stream (json-deserialize action))
    (labels ((commit (hash context)
	       (let* ((uri-full (uri?
				 (make-core-stream (return-stream stream))))
		      (req (context.request context))
		      (rep (context.response context))
		      (current-uri (http-request.uri req)))

		 (setf (uri.paths current-uri)
		       (if (equal (web-application.fqdn self)
				  (car (uri.paths uri)))
			   (cdr (uri.paths uri))
			   (uri.paths uri)))
		 
		 (setf (uri.queries current-uri)
		       (cons (cons "__hash" hash) (uri.queries uri-full)))
		 
		 (escape (dispatch self req rep))))
	     (multipart-action (hash)
	       (javascript/suspend
		(lambda (s)
		  (let ((k-url
			 (action/url ((data "data") (commit "commit")
				      (hash "__hash"))
			   (context.remove-current-action +context+)
			   (cond
			     (commit (commit hash +context+))
			     (t
			      (write-stream stream (subseq data 1 (- (length data) 1)))
			      (multipart-action (json-deserialize hash)))))))
		    (with-js (k-url hash) s
		      (apply (slot-value window hash) this (list k-url))))))))
      (multipart-action (json-deserialize hash)))))


;; (defapplication test1 (http-application)
;;   ()
;;   (:default-initargs :fqdn "eben.core.gen.tr" :admin-email "zoo"))

;; (defhandler "index.core" ((app test1) (context http-context) (abc "abc") (def "def"))
;;   (<:div abc def))

;; (defvar *e (make-instance 'test1))
;; (register *server* *e)

;; TODO: what if /../../../etc/passwd ? filter those.
;; (defun directory-handler (context)
;;   "Default directory handler which serves static files"
;;   (let ((htdocs-path (web-application.htdocs-pathname (context.application context)))
;; 	(paths (let ((tmp (or (uri.paths (http-request.uri (context.request context))) '(("")))))
;; 		 (if (equal (caar tmp) "")
;; 		     '(("index.html"))
;; 		     tmp))))
;;     ;; 404 when htdocs not found
;;     (if (or (null htdocs-path) (not (cl-fad:file-exists-p htdocs-path)))
;; 	(render-404 (context.application context) (context.request context) (context.response context))
;; 	(let* ((file-and-ext (pathname (caar (last paths))))
;; 	       (path (append '(:relative) (mapcar #'car (butlast paths))))
;; 	       (output (http-response.stream (context.response context)))
;; 	       (abs-path (merge-pathnames
;; 			  (merge-pathnames (make-pathname :directory path) file-and-ext)
;; 			  htdocs-path))
;; 	       (mime-type (mime-type abs-path))) 
;; 	  (if (and (cl-fad:file-exists-p abs-path)
;; 		   (not (cl-fad:directory-exists-p abs-path)))
;; 	      (progn
;; 		(setf (cdr (assoc 'content-type (http-response.entity-headers (context.response context))))
;; 		      (split "/" mime-type))
;; 		(with-open-file (input abs-path :element-type '(unsigned-byte 8) :direction :input)
;; 		  (let ((seq (make-array (file-length input) :element-type 'unsigned-byte)))
;; 		    (read-sequence seq input)
;; 		    (write-stream output seq))))
;; 	      (render-404 (context.application context) (context.request context) (context.response context)))))))

;; (defmacro defhandler (name queries &body body)
;;   (with-unique-names (context)
;;     `(defun ,name (,context)
;;        (with-context ,context
;; 	 (with-html-output (http-response.stream (context.response ,context))
;; 	   (with-query ,queries (context.request ,context)	     
;; 	     ,@body))
;; 	 (setf (context.request ,context) nil
;; 	       (context.response ,context) nil))
;;        t)))

;; (defmacro defurl (application regexp-url queries &body body)
;;   "Defines a new entry point to 'application' having url
;; 'regexp-url'. 'queries' are bounded while executing 'body'"
;;   `(register-url ,application ,regexp-url
;; 		 (lambda (context)		   
;; 		   (with-context context
;; 		     (with-html-output (http-response.stream (context.response +context+))
;; 		       (with-query ,queries (context.request +context+)
;; 			 ,@body))
;; 		     (setf (context.request +context+) nil
;; 			   (context.response +context+) nil)
;; 		     t))))

;; (defmethod register-handler ((application http-application) regexp-url lambda)
;;   "Registers a new 'regexp-url' to application to execute 'lambda'
;; when requested"
;;   (setf (application.handlers application)
;; 	(cons (list regexp-url (cl-ppcre:create-scanner regexp-url) lambda)
;; 	      (unregister-url application regexp-url))))

;; (defmethod unregister-handler ((self http-application) regexp-url)
;;   "Removes 'regexp-url' from applications handlers"
;;   (setf (application.handlers self)
;; 	(remove regexp-url (application.handlers self) :test #'equal :key #'car)))

;; (defmethod dispatch ((self http-application) (request http-request) (response http-response))
;;   "Dispatch 'request' to 'self' application with empty 'response'"
;;   (when (> (random 100) 40)
;;     (gc self))

;;   (let ((session (gethash (find-session-id request) (application.sessions self))))
;;     (acond
;;      ((and session (gethash (uri.query (http-request.uri request) +continuation-query-name+)
;; 			    (session.continuations session)))      
;;       (log-me (application.server self) 'http-application		
;; 	      (format nil "fqdn: ~A, k-url: ~A"
;; 		      (web-application.fqdn self)
;; 		      (uri.query (http-request.uri request) +continuation-query-name+)))
;;       (funcall it request response))
;;      ;; ((any #'(lambda (url)
;; ;; 	       (aif (caar (uri.paths (http-request.uri request)))
;; ;; 		    (and (cl-ppcre:scan-to-strings (cadr url) it :sharedp t) url)))
;; ;; 	   (application.urls self))      
;; ;;       (log-me (application.server self) 'http-application
;; ;; 	      (format nil "fqdn: ~A, dyn-url: ~A" (web-application.fqdn self)
;; ;; 		      (http-request.uri request)))
;; ;;       (funcall (caddr it) (make-new-context self request response session)))
;;      ((any #'(lambda (handler)
;; 	       (aif (caar (uri.paths (http-request.uri request)))
;; 		    (and (cl-ppcre:scan-to-strings (cdr handler) it :sharedp t)
;; 			 (car handler))))
;; 	   (application+.handlers (class-of self)))
;;       (log-me (application.server self) 'http-application
;; 	      (format nil "fqdn: ~A, static-handler:: ~A" (web-application.fqdn self)
;; 		      (http-request.uri request)))
;;       (funcall it self (make-new-context self request response session)))
;;      ((directory-handler (make-new-context self request response session))
;;       (log-me (application.server self) 'http-application
;; 	      (format nil "fqdn: ~A, static-url: ~A" (web-application.fqdn self)
;; 		      (http-request.uri request)))
;;       t)
;;      (t
;;       (render-404 self request response)))))

;; (defun make-dispatcher (regexp-string handler)
;;   "Returns a new dispatcher list having url 'regexp-string' and lambda
;; 'handler'"
;;   (list regexp-string (cl-ppcre:create-scanner regexp-string) handler))

;;    #:http-application
;;    #:find-session
;;    #:query-session
;;    #:update-session
;;    #:find-continuation
;;    #:with-context ;; helper for defurl
;;    #:with-query	  ;; helper macro
;;    #:defurl
;;    #:defhandler
;;    #:register-url
;;    #:unregister-url
;;    #:make-dispatcher
;;    #:find-url
;;    #:http-session
;;    #:session
;;    #:session.id
;;    #:session.timestamp
;;    #:session.continuations
;;    #:session.data
;;    #:make-new-session
;;    #:http-context
;;    #:context.request
;;    #:context.response
;;    #:context.session
;;    #:context.session-boundp
;;    #:context.application
;;    #:context.continuation
;;    #:context.returns
;;    #:+context+
;;    #:+html-output+
;;    #:make-new-context
;;    #:copy-context
;;    #:request
;;    #:response
;;    #:send/suspend
;;    #:send/forward
;;    #:send/finish
;;    #:javascript/suspend
;;    #:json/suspend
;;    #:xml/suspend
;;    #:css/suspend
;;    #:function/hash
;;    #:action/hash
;;    #:function/url
;;    #:action/url
;;    #:answer
;;    #:answer/dispatch
;;    #:dispatch
;;    #:kontinue
;;    #:test-url
   
