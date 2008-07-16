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

;;+----------------------------------------------------------------------------
;;| HTTP Application
;;+----------------------------------------------------------------------------
;;
;; This file contains HTTP application implementation.
;;
(defvar +continuation-query-name+ "k"
  "Query key for continuations")

(defvar +session-query-name+ "s"
  "Query key for sessions")

(defvar +session-timeout+ (* 10 60 1000)
  "Session timeout in milisecons")

(defvar +context+ nil
  "A special variable that holds HTTP context")

(defvar +k+ nil
  "A special variable that holds current continuation")

(defmacro with-query (queries request &body body)
  "Executes 'body' while binding 'queries' from 'request'"
  `(let ,(mapcar #'(lambda (p)
		     `(,(car p)
			(or (uri.query (http-request.uri ,request) ,(cadr p))
			    ,(caddr p))))
		 queries)
     ,@body))

(defmethod find-session ((self http-application) id)
  "Returns the session associated with 'id'"
  (gethash id (application.sessions self)))

(defmethod find-continuation ((self http-application) id)
  "Returns the continuation associated with 'id'"
  (maphash #'(lambda (k session)
	       (declare (ignorable k))
	       (aif (gethash id (continuations session))
		    (return-from find-continuation (values it session))))
	   (application.sessions self)))

(defmacro with-context (context &body body)
  "Executes 'body' with HTTP context bind to 'context'"
  `(with-call/cc
     (let ((+context+ ,context))
       (declare (special +context+))
       ,@body)))

(defmacro defurl (application regexp-url queries &body body)
  "Defines a new entry point to 'application' having url
'regexp-url'. 'queries' are bounded while executing 'body'"
  `(register-url ,application ,regexp-url
		 (lambda (context)		   
		   (with-context context
		     (with-html-output (http-response.stream (response +context+))
		       (with-query ,queries (request +context+)
			 ,@body))))))

(defmethod register-url ((self http-application) regexp-url lambda)
  "Registers a new 'regexp-url' to application to execute 'lambda'
when requested"
  (setf (application.urls self)
	(append (unregister-url self regexp-url)
		(list (list regexp-url (cl-ppcre:create-scanner regexp-url) lambda)))))

(defun make-dispatcher (regexp-string handler)
  "Returns a new dispatcher list having url 'regexp-string' and lambda
'handler'"
  (list regexp-string (cl-ppcre:create-scanner regexp-string) handler))

(defmethod unregister-url ((self http-application) regexp-url)
  "Removes 'regexp-url' from applications handlers"
  (setf (application.urls self)
	(remove regexp-url (application.urls self) :test #'equal :key #'car)))

(defmethod find-url ((self http-application) regexp-url)
  "Returns associated dispatcher with 'regexp-url'"
  (aif (any #'(lambda (url)
		(and (cl-ppcre:scan-to-strings (cadr url) regexp-url :sharedp t) url))
	    (application.urls self))
       (caddr it)))

(defun make-new-session ()
  "Returns a new empty session"
  (make-instance 'http-session))

(defun make-new-context (application request response session)
  "Returns a new HTTP context having parameters provided"
  (make-instance 'http-context
		 :application application :request request
		 :response response :session session))

(defmethod copy-context ((self http-context))
  "Returns a copy of the HTTP context 'self'"
  ;; (let ((s (copy-core-stream self)))
;;     (change-class s (find-class 'http-context))
;;     (setf (slot-value s 'application) (s-v 'application)
;; 	  (slot-value s 'session) (s-v 'session)
;; 	  (slot-value s 'continuation) (s-v 'continuation))
;;     s)
  (let ((s (make-instance 'http-context)))
    (setf (slot-value s 'application) (s-v 'application)
 	  (slot-value s 'session) (s-v 'session)
 	  (slot-value s 'continuation) (s-v 'continuation))
     s))

(defmethod session ((self http-context))
  "Return the session of the HTTP context 'self', creates if none exists"
  (let ((s (slot-value self 'session)))
    (if (null s)
	(setf s (make-new-session)
	      (slot-value self 'session) s
	      (gethash (id s) (application.sessions (application self))) s)
	(prog1 s (setf (timestamp s) (get-universal-time))))))

(defmacro send/suspend (&body body)
  "Saves current continuation, executes 'body', terminates."
  (with-unique-names (result)
    `(let ((,result (multiple-value-list
		     (let/cc k
		       (setf (continuation +context+) k)
		       ;;     (format t "send/suspend called~%")
		       ;;     (checkpoint-stream/cc +context+ k)
		       (with-html-output (http-response.stream (response +context+))
			 ,@body)
		       ;;     (format t "send/suspend escaping~%")
		       ;;     (commit-stream/cc +context+ (rets +context+))
		       (escape (reverse (returns +context+)))
		       (break "send/suspend failed.")))))
       (setf (request +context+) (request (car ,result))
	     (response +context+) (response (car ,result)))
       (apply #'values (cdr ,result)))))

(defmacro function/hash (parameters &body body)
  "Registers a continuation at macro-expansion time and binds
'parameters' while executing 'body'"
  (with-unique-names (name context)
    `(if +context+
	 (let* ((,name (format nil "fun-~A" (random-string 3)))
		(,context +context+)
		(kont (lambda (req rep &optional ,@(mapcar #'car parameters))
			(setf +context+ (copy-context ,context)
			      (request +context+) req
			      (response +context+) rep)
			(with-query ,(mapcar (lambda (param)
					       (reverse
						(cons (car param)
						      (reverse param))))
					     parameters) (request +context+)
			  (with-html-output (http-response.stream (response +context+))
			    ,@body)))))	   
	   (prog1 ,name	   
	     (setf (gethash ,name (continuations (session +context+))) kont)
	     (pushnew (cons ,name (lambda ,(mapcar #'car parameters)
				    (funcall kont
					     (make-instance 'http-request
							    :uri (make-instance 'uri))
					     (make-response *core-output*)
					     ,@(mapcar #'car parameters))))
		      (returns +context+))))
	 "invalid-function-hash")))

(defmacro action/hash (parameters &body body)
  "Registers a continuation at run time and binds 'parameters' while
executing 'body'"
  (with-unique-names (context)
    (let ((name (format nil "act-~A" (random-string 8))))
      `(if +context+	   
	   (let* ((,context +context+)
		  (kont (lambda (req rep &optional ,@(mapcar #'car parameters))
			  (setf +context+ (copy-context ,context)
				(request +context+) req
				(response +context+) rep)
			  (with-query ,(mapcar (lambda (param)
						 (reverse
						  (cons (car param)
							(reverse param))))
					       parameters) (request +context+)
			    (with-html-output (http-response.stream (response +context+))
			      ,@body)))))
	     (prog1 ,name
	       (setf (gethash ,name (continuations (session +context+))) kont)
	       (pushnew (cons ,name (lambda ,(mapcar #'car parameters)
				      (funcall kont
					       (make-instance 'http-request
							      :uri (make-instance 'uri))
					       (make-response *core-output*)
					       ,@(mapcar #'car parameters))))
			(returns +context+))))
	   "invalid-action-hash"))))

(defmacro function/url (parameters &body body)
  "Returns URI representation of function/hash"
  `(format nil "?~A:~A$~A:~A"
	   +session-query-name+ (if +context+
				    (id (session +context+))
				    +invalid-session+)
	   +continuation-query-name+ (function/hash ,parameters ,@body)))

(defvar +invalid-session+ "invalid-session-id")

(defmacro action/url (parameters &body body)
  "Returns URI representation of action/hash"
  `(format nil "?~A:~A$~A:~A"
	   +session-query-name+ (if +context+
				    (id (session +context+))
				    +invalid-session+)
	   +continuation-query-name+ (action/hash ,parameters ,@body)))

(defmacro answer (&rest values)
  "Continues from the continuation saved by action/hash or function/hash"
  `(progn
;;      (mapcar #'(lambda (ret)
;; 		 (remhash (car ret) (continuations (session +context+))))
;; 	     (returns +context+))
     ;; (rewind-stream/cc +context+ (list ,@values))
     (if (null (continuation +context+))
	 (error "Surrounding send/suspend not found, can't answer")
	 (kall (continuation +context+) +context+ ,@values))))

(defun/cc javascript/suspend (lambda)
  "Javascript version of send/suspend, sets content-type to text/javascript"
  (send/suspend    
    (setf (cdr (assoc 'content-type (http-response.entity-headers (response +context+))))
	  '("text" "javascript" ("charset" "UTF-8")))
    (funcall lambda)))

(defun/cc json/suspend (lambda)
  "Json version of send/suspend, sets content-type to text/json"
  (send/suspend    
    (setf (cdr (assoc 'content-type (http-response.entity-headers (response +context+))))
	  '("text" "json" ("charset" "UTF-8")))
    (funcall lambda)))

(defun/cc xml/suspend (lambda)
  "Xml version of send/suspend, sets content-type to text/xml"
  (send/suspend    
    (setf (cdr (assoc 'content-type (http-response.entity-headers (response +context+))))
	  '("text" "xml" ("charset" "UTF-8")))
    (funcall lambda)))

(defmethod gc ((self http-application))
  "Garbage collector for HTTP application, removes expired sessions/continuations"
  (let* ((sessions (application.sessions self)))
    (mapc (rcurry #'remhash sessions)
	  (let (expired)
	    (maphash #'(lambda (k v)
			 (when (> (- (get-universal-time) (timestamp v)) +session-timeout+)
			   (push k expired)))
		     sessions)
	    expired))))

(defmethod dispatch ((self http-application) (request http-request) (response http-response))
  "Dispatch 'request' to 'self' application with empty 'response'"
  (when (> (random 100) 40)
    (gc self))
  (let ((session (gethash (uri.query (http-request.uri request) +session-query-name+)
			  (application.sessions self))))
    (acond
     ((and session (gethash (uri.query (http-request.uri request) +continuation-query-name+)
			    (continuations session)))      
      (log-me (application.server self) 'http-application		
	      (format nil "fqdn: ~A, k-url: ~A"
		      (web-application.fqdn self)
		      (uri.query (http-request.uri request) +continuation-query-name+)))
      (funcall it request response))
     ((any #'(lambda (url)
	       (aif (caar (uri.paths (http-request.uri request)))
		    (and (cl-ppcre:scan-to-strings (cadr url) it :sharedp t) url)))
	   (application.urls self))      
      (log-me (application.server self) 'http-application
	      (format nil "fqdn: ~A, dyn-url: ~A" (web-application.fqdn self)
		      (http-request.uri request)))
      (funcall (caddr it) (make-new-context self request response session)))
     (t
      (log-me (application.server self) 'http-application
	      (format nil "fqdn: ~A, static-url: ~A" (web-application.fqdn self)
		      (http-request.uri request)))
      (directory-handler (make-new-context self request response session))))
    t))

(defmacro with-test-context ((context-var uri application) &body body)
  "Executes 'body' with context bound to 'context-var'"
  `(let ((,context-var (make-new-context ,application
					 (make-instance 'http-request :uri ,uri)
					 (make-response *core-output*)
					 nil)))
     ,@body))

(defun kontinue (&rest args)
  "Continues a contination saves by function/hash or action/hash"
  (if (functionp (car args))
      (apply (car args)	     
	     (cons (make-instance 'http-request :uri (make-instance 'uri))
		   (cons (make-response *core-output*) (rest args))))
      (destructuring-bind (number result &rest parameters) args
	  (let ((fun (cdr (nth number result))))
	    (if (null fun)
		(error "Continuation not found, please correct name.")
		(apply fun parameters))))))

(defmacro test-url (url application)
  "Conventional macro to test urls wihout using a browser. One may
provide query parameters inside URL as key=value"
  `(let ((uri (uri? (make-core-stream ,url))))
     (funcall (find-url ,application (caar (uri.paths uri)))
	      (make-new-context ,application
				(make-instance 'http-request :uri uri)
				(make-response *core-output*)
				nil))))

;; TODO: what if /../../../etc/passwd ? filter those.
(defun directory-handler (context)
  "Default directory handler which serves static files"
  (let ((htdocs-path (web-application.htdocs-pathname (application context)))
	(paths (let ((tmp (or (uri.paths (http-request.uri (request context))) '(("")))))
		 (if (equal (caar tmp) "")
		     '(("index.html"))
		     tmp))))
    ;; 404 when htdocs not found
    (if (or (null htdocs-path) (not (cl-fad:file-exists-p htdocs-path)))
	(render-404 (request context) (response context))
	(let* ((file-and-ext (pathname (caar (last paths))))
	       (path (append '(:relative) (mapcar #'car (butlast paths))))
	       (output (http-response.stream (response context)))
	       (abs-path (merge-pathnames
			  (merge-pathnames (make-pathname :directory path) file-and-ext)
			  htdocs-path))
	       (mime-type (mime-type abs-path))) 
	  (if (and (cl-fad:file-exists-p abs-path)
		   (not (cl-fad:directory-exists-p abs-path)))
	      (progn
		(setf (cdr (assoc 'content-type (http-response.entity-headers (response context))))
		      (split "/" mime-type))
		(with-open-file (input abs-path :element-type '(unsigned-byte 8) :direction :input)
		  (let ((seq (make-array (file-length input) :element-type 'unsigned-byte)))
		    (read-sequence seq input)
		    (write-stream output seq))))
	      (render-404 (request context) (response context)))))))
