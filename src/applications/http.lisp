(in-package :core-server)

(defvar +continuation-query-name+ "k")
(defvar +session-query-name+ "s")
(defvar +context+ nil)
(defvar +k+ nil)

(defmacro with-query (queries request &body body)
  `(let ,(mapcar #'(lambda (p)
		     `(,(car p) (or (uri.query (http-request.uri ,request) ,(cadr p))
				    ,(caddr p))))
		 queries)
     ,@body))

(defclass http-application (web-application)
  ((urls :accessor application.urls :initform '())
   (sessions :accessor application.sessions :initform (make-hash-table :test #'equal))))

(defmethod find-session ((self http-application) id)
  (gethash id (application.sessions self)))

(defmethod find-continuation ((self http-application) id)
  (maphash #'(lambda (k session)
	       (aif (gethash id (continuations session))
		    (return-from find-continuation (values it session))))
	   (application.sessions self)))

(defmacro defurl (application regexp-url queries &body body)
  `(register-url ,application ,regexp-url
		 (lambda (context)
		   (with-call/cc
		     (let ((+context+ context))
		       (declare (special +context+))
		       (with-query ,queries (request +context+)
			 ,@body))))))

(defmethod register-url ((self http-application) regexp-url lambda)  
  (setf (application.urls self)
	(cons (list regexp-url (cl-ppcre:create-scanner regexp-url) lambda)
	      (unregister-url self regexp-url))))

(defmethod unregister-url ((self http-application) regexp-url)
  (setf (application.urls self)
	(remove regexp-url (application.urls self) :test #'equal :key #'car)))

(defmethod find-url ((self http-application) regexp-url)
  (caddr (find regexp-url (application.urls self) :test #'equal :key #'car)))

(defclass http-session ()
  ((id :reader id :initform (random-string 8))
   (continuations :reader continuations :initform (make-hash-table :test #'equal)) 
   (timestamp :accessor timestamp :initform (get-universal-time))))

(defun make-new-http-session ()
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

(defmethod copy-context ((self http-context) request response)
  (let ((s (copy-core-stream self)))
    (change-class s (find-class 'http-context))
    (setf (slot-value s 'request) request
	  (slot-value s 'response) response
	  (slot-value s 'application) (s-v 'application)
	  (slot-value s 'session) (s-v 'session)
	  (slot-value s 'continuation) (s-v 'continuation))
    s))

(defmethod session ((self http-context))
  (let ((s (slot-value self 'session)))
    (if (null s)
	(setf s (make-new-http-session)
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
       (break "send/suspend commit failed."))
;;     (describe +context+)
     ))

(defmacro function/hash (parameters &body body)
  (with-unique-names (context kont name)
    `(let ((,context +context+)
	   (,name (format nil "fun-~A" (random-string 3))))
       (let ((,kont (lambda (request reponse &optional ,@(mapcar #'car parameters))
		      (let ((+context+ (copy-context ,context request response)))
			(with-query ,(mapcar #'(lambda (param) (reverse (cons (car param) (reverse param))))
					     parameters) request
			  ,@body)))))
	 (prog1 ,name	   
	   (setf (gethash ,name (continuations (session ,context))) ,kont)
	   (pushnew (cons ,name (lambda ,(mapcar #'car parameters)
				  (apply ,kont
					 (make-instance 'http-request :uri (make-instance 'uri))
					 (make-response) (list ,@(mapcar #'car parameters)))))
		    (returns ,context)))))))

(defmacro action/hash (parameters &body body)
  (with-unique-names (context kont)
    (let ((name (format nil "act-~A" (random-string 8))))
      `(let ((,context (copy-context +context+ nil nil)))
	 (let ((,kont (lambda (request reponse &optional ,@(mapcar #'car parameters))
			(let ((+context+ ,context ;;				(copy-context ,context request response)
				))
			  (setf (request +context+) request
				(response +context+) response)
			  (with-query ,(mapcar #'(lambda (param) (reverse (cons (car param) (reverse param))))
					       parameters) request
			    ,@body)))))
	   (prog1 ,name
	     (unless (gethash ,name (continuations (session ,context)))
	       (setf (gethash ,name (continuations (session ,context))) ,kont))
	     (pushnew (cons ,name (lambda ,(mapcar #'car parameters)
				    (apply ,kont
					   (make-instance 'http-request :uri (make-instance 'uri))
					   (make-response) (list ,@(mapcar #'car parameters)))))
		      (returns ,context))))))))

(defmacro function/url (parameters &body body)
  `(format nil "?~A=~A&~A=~A"
	   +session-query-name+ (id (session +context+))
	   +continuation-query-name+ (function/hash ,parameters ,@body)))

(defmacro action/url (parameters &body body)
  `(format nil "?~A=~A&~A=~A"
	   +session-query-name+ (id (session +context+))
	   +continuation-query-name+ (action/hash ,parameters ,@body)))

(defmacro answer (&rest values)
  `(let ((values (list ,@values)))
     ;; (rewind-stream/cc +context+ (list ,@values))
     (apply #'kall (continuation +context+) (list ,@values))))

(defmethod dispatch ((self http-application) (request http-request) (response http-response))
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
	       (aif (caadr (uri.paths (http-request.uri request)))
		    (and (cl-ppcre:scan-to-strings (cadr url) it :sharedp t) url)))
	   (application.urls self))
      (prog1 t      
	(funcall (caddr it) (make-new-context self request response session)))))))

(defun kontinue (&rest args)
  (if (functionp (car args))
      (apply (car args)
	     (make-instance 'http-request :uri (make-instance 'uri))
	     (make-response) (rest args))
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
				(make-response)
				nil))))
;;)

;; Override YACLmL
(defun yaclml::emit-princ (&rest items)
  "Princ items to *yaclml-stream*"
  (mapc #'(lambda (item)
	    (awhen (and item (yaclml::string-value-of item))
	      (if (typep *yaclml-stream* 'core-stream)
		  (string! *yaclml-stream* it)
		  (princ it *yaclml-stream*))))
   items)
  nil)

(defun emit-html (&rest items)
  "Like EMIT-PRINC but escapes html chars in item."
  (mapc
   #'(lambda (item)
       (awhen (and item (yaclml::string-value-of item))
	 (if (typep *yaclml-stream* 'core-stream)
	     (string! *yaclml-stream* (escape-as-html it))
	     (princ (escape-as-html it) *yaclml-stream*))))
   items)
  nil)

;; (defun print-+k+ ()
;;   (loop for i in +k+
;;        for j from 0
;;        do (format t "~%~D:~A" j i)))
