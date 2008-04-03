(in-package :core-server)

(defvar +continuation-query-name+ "k")
(defvar +session-query-name+ "s")
(defvar +context+ nil)
(defvar +html-output+ nil)
(defvar +k+ nil)

(defmacro with-query (queries request &body body)
  `(let ,(mapcar #'(lambda (p)
		     `(,(car p) (or (uri.query (http-request.uri ,request) ,(cadr p))
				    ,(caddr p))))
		 queries)
     ,@body))

(defclass http-application (web-application)
  ((urls :accessor application.urls :initarg :urls :initform '())
   (sessions :accessor application.sessions :initform (make-hash-table :test #'equal))))

(defmethod print-object ((self http-application) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "FQDN:\"~A\" is ~Arunning." (web-application.fqdn self)
	    (if (status self) "" "*not* "))))

(defmethod find-session ((self http-application) id)
  (gethash id (application.sessions self)))

(defmethod find-continuation ((self http-application) id)
  (maphash #'(lambda (k session)
	       (aif (gethash id (continuations session))
		    (return-from find-continuation (values it session))))
	   (application.sessions self)))

(defmacro with-context (context &body body)
  `(with-call/cc
     (let ((+context+ ,context))
       (declare (special +context+))
       ,@body)))

(defmacro defurl (application regexp-url queries &body body)
  `(register-url ,application ,regexp-url
		 (lambda (context)
		   (let ((+html-output+ (http-response.stream (response context))))
		     (declare (special +html-output+))
		     (with-context context		       
		       (with-query ,queries (request +context+)
			 ,@body))))))

(defmethod register-url ((self http-application) regexp-url lambda)  
  (setf (application.urls self)
	(append (unregister-url self regexp-url)
		(list (list regexp-url (cl-ppcre:create-scanner regexp-url) lambda)))))

(defun make-dispatcher (regexp-string handler)
  (list regexp-string (cl-ppcre:create-scanner regexp-string) handler))

(defmethod unregister-url ((self http-application) regexp-url)
  (setf (application.urls self)
	(remove regexp-url (application.urls self) :test #'equal :key #'car)))

(defmethod find-url ((self http-application) regexp-url)
  (aif (any #'(lambda (url)
		(and (cl-ppcre:scan-to-strings (cadr url) regexp-url :sharedp t) url))
	    (application.urls self))
       (caddr it))
;;;   (caddr (find regexp-url (application.urls self) :test #'equal :key #'car))
  )

(defclass http-session ()
  ((id :reader id :initform (random-string 8))
   (continuations :reader continuations :initform (make-hash-table :test #'equal)) 
   (timestamp :accessor timestamp :initform (get-universal-time))))

(defun make-new-session ()
  (make-instance 'http-session))

(defclass http-context (core-cps-stream)
  ((request :accessor request :initarg :request :initform nil)
   (response :accessor response :initarg :response :initform nil)
   (session :accessor session :initarg :session :initform nil)
   (application :accessor application :initarg :application :initform nil)
   (continuation :accessor continuation :initform nil)
   (returns :accessor returns :initform nil)))

(defun make-new-context (application request response session)
  (make-instance 'http-context
		 :application application
		 :request request
		 :response response
		 :session session))

(defmethod copy-context ((self http-context))
  (let ((s (copy-core-stream self)))
    (change-class s (find-class 'http-context))
    (setf (slot-value s 'application) (s-v 'application)
	  (slot-value s 'session) (s-v 'session)
	  (slot-value s 'continuation) (s-v 'continuation))
    s))

(defmethod session ((self http-context))
  (let ((s (slot-value self 'session)))
    (if (null s)
	(setf s (make-new-session)
	      (slot-value self 'session) s
	      (gethash (id s) (application.sessions (application self))) s)
	(setf (timestamp s) (get-universal-time)))
    s))

(defmacro send/suspend (&body body)
  `(prog1
     (let/cc k
       (setf (continuation +context+) k)
       (format t "send/suspend called~%")
       ;;     (checkpoint-stream/cc +context+ k)
       ,@body
       (format t "send/suspend escaping~%")
       ;;     (commit-stream/cc +context+ (rets +context+))
;;       (describe +context+)
       (escape (returns +context+))
       (break "send/suspend failed."))
;;     (describe +context+)
     ))

(defmacro function/hash (parameters &body body)
  (with-unique-names (context kont name)
    `(let ((,name (format nil "fun-~A" (random-string 3)))
	   (,context (copy-context +context+)))
       (let ((,kont (lambda (http-request http-response &optional ,@(mapcar #'car parameters))
		     (let ((+context+ ,context))
		       (setf (request +context+) http-request (response +context+) http-response)
		       (with-query ,(mapcar #'(lambda (param) (reverse (cons (car param) (reverse param))))
					    parameters) (request +context+)
			 ,@body)))))
	 (prog1 ,name	   
	   (setf (gethash ,name (continuations (session ,context))) ,kont)
	   (pushnew (cons ,name (lambda ,(mapcar #'car parameters)
				  (apply ,kont
					 (make-instance 'http-request :uri (make-instance 'uri))
					 (make-response) (list ,@(mapcar #'car parameters)))))
		    (returns +context+)))))))

(defmacro action/hash (parameters &body body)
  (with-unique-names (context kont)
    (let ((name (format nil "act-~A" (random-string 8))))
      `(let ((,context (copy-context +context+)))
	 (let ((,kont (lambda (http-request http-response &optional ,@(mapcar #'car parameters))
			(let ((+context+ ,context))
			  (setf (request +context+) http-request (response +context+) http-response)
			  (with-query ,(mapcar #'(lambda (param) (reverse (cons (car param) (reverse param))))
					       parameters) (request +context+)
			    ,@body)))))
	   (prog1 ,name
	     (unless (gethash ,name (continuations (session ,context)))
	       (setf (gethash ,name (continuations (session ,context))) ,kont))
	     (pushnew (cons ,name (lambda ,(mapcar #'car parameters)
				    (apply ,kont
					   (make-instance 'http-request :uri (make-instance 'uri))
					   (make-response) (list ,@(mapcar #'car parameters)))))
		      (returns +context+))))))))

(defmacro function/url (parameters &body body)
  `(format nil "?~A=~A&~A=~A"
	   +session-query-name+ (id (session +context+))
	   +continuation-query-name+ (function/hash ,parameters ,@body)))

(defmacro action/url (parameters &body body)
  `(format nil "?~A=~A&~A=~A"
	   +session-query-name+ (id (session +context+))
	   +continuation-query-name+ (action/hash ,parameters ,@body)))

(defmacro answer (&rest values)
  `(progn
;;      (mapcar #'(lambda (ret)
;; 		 (remhash (car ret) (continuations (session +context+))))
;; 	     (returns +context+))
     ;; (rewind-stream/cc +context+ (list ,@values))
     (apply #'kall (continuation +context+) (list ,@values))))

(defun/cc javascript/suspend (lambda)
  (send/suspend    
    (setf (cdr (assoc 'content-type (http-response.entity-headers (response +context+))))
	  '("text" "javascript" ("charset" "UTF-8")))
    (funcall lambda)))

(defun/cc json/suspend (lambda)
  (send/suspend    
    (setf (cdr (assoc 'content-type (http-response.entity-headers (response +context+))))
	  '("text" "json" ("charset" "UTF-8")))
    (funcall lambda)))

(defun/cc xml/suspend (lambda)
  (send/suspend    
    (setf (cdr (assoc 'content-type (http-response.entity-headers (response +context+))))
	  '("text" "xml" ("charset" "UTF-8")))
    (funcall lambda)))

(defmethod gc ((self http-application))
  (let* ((session-timeout (* 10 60 1000))
	 (sessions (application.sessions self)))
    ;; bu gc her dispatch'te mi caliscak?
    ;; %40 ihtimalle calisabilir

    ;; tell me who is expired!
    (mapc (rcurry #'remhash sessions)
	  (let (expired)
	    (maphash #'(lambda (k v)
			 (when (> (- (get-universal-time) (timestamp v)) session-timeout) (push k expired)))
		     sessions)
	    expired))))

(defmethod dispatch ((self http-application) (request http-request) (response http-response))
  (when (> (random 100) 40)
    (gc self))
  (let ((session (gethash (uri.query (http-request.uri request) +session-query-name+)
			  (application.sessions self))))
    (acond
     ((and session (gethash (uri.query (http-request.uri request) +continuation-query-name+)
			    (continuations session)))
      (prog1 t
	(format t "Find k-url:~A ~A~%"
		(uri.query (http-request.uri request) +continuation-query-name+)
		it)
	(funcall it request response)))
     ((any #'(lambda (url)
	       (aif (caar (uri.paths (http-request.uri request)))
		    (and (cl-ppcre:scan-to-strings (cadr url) it :sharedp t) url)))
	   (application.urls self))
      (prog1 t      
	(funcall (caddr it) (make-new-context self request response session))))
     (t (prog1 t
	  (directory-handler (make-new-context self request response session)))))))

(defun kontinue (&rest args)
  (if (functionp (car args))
      (apply (car args)
	     (cons (make-instance 'http-request :uri (make-instance 'uri))
		   (cons (make-response) (rest args))))
      (destructuring-bind (number result &rest parameters) args
	  (let ((fun (cdr (nth number result))))
	    (if (null fun)
		(error "Continuation not found, please correct name.")
		(apply fun parameters))))))

;; (aif (gethash (uri.query uri +continuation-query-name+)
;; 		   (application.continuations ,application))
;; 	  (funcall it (make-instance 'http-request :uri uri) (make-response) ,@parameters)
(defmacro test-url (url application &rest parameters)
  `(let ((uri (uri? (make-core-stream ,url))))
     (funcall (find-url ,application (caar (uri.paths uri)))
	      (make-new-context ,application
				(make-instance 'http-request :uri uri)
				(make-response *core-output*)
				nil))))
;;)

;; Override YACLmL
;; (defun yaclml::emit-princ (&rest items)
;;   "Princ items to *yaclml-stream*"
;;   (mapc #'(lambda (item)
;; 	    (awhen (and item (yaclml::string-value-of item))
;; 	      (if (typep *yaclml-stream* 'core-stream)
;; 		  (string! *yaclml-stream* it)
;; 		  (princ it *yaclml-stream*))))
;;    items)
;;   nil)

;; (defun emit-html (&rest items)
;;   "Like EMIT-PRINC but escapes html chars in item."
;;   (mapc
;;    #'(lambda (item)
;;        (awhen (and item (yaclml::string-value-of item))
;; 	 (if (typep *yaclml-stream* 'core-stream)
;; 	     (string! *yaclml-stream* (escape-as-html it))
;; 	     (princ (escape-as-html it) *yaclml-stream*))))
;;    items)
;;   nil)

;; (deftag <:js (&body body)
;;   `(<:ai
;;     (js::js*
;;      ,@body) ~%))

;; (defun print-+k+ ()
;;   (loop for i in +k+
;;        for j from 0
;;        do (format t "~%~D:~A" j i)))


;; TODO: what if /../../../etc/passwd ? filter those.
(defun directory-handler (context)
  (let ((htdocs-path (web-application.htdocs-pathname (application context)))
	(paths (uri.paths (http-request.uri (request context)))))
    ;; 404 when htdocs not found
    (when (not (cl-fad:file-exists-p htdocs-path))
      (render-404 (request context) (response context)))
    ;; index.html when root requested
    (when (string= (caar paths) "")
      (setf (caar paths) "index.html"))
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
	  (render-404 (request context) (response context))))))
