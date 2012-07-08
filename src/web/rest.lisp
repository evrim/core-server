(in-package :core-server)

;; -------------------------------------------------------------------------
;; Rest API
;; -------------------------------------------------------------------------

;; -------------------------------------------------------------------------
;; Rest+ Meta Class
;; -------------------------------------------------------------------------
(defclass+ rest+ (component+)
  ((url :initarg :url)
   (authenticate :initarg :authenticate :initform nil)
   (data-class :initarg :data-class :initform nil)
   (prefix :initarg :prefix :initform nil)))

(defmethod validate-superclass ((class rest+) (super component+)) t)
(defmethod validate-superclass ((class component+) (super rest+)) t)

(defmethod rest+.data+ ((self rest+))
  (class+.find (slot-value self 'data-class)))

(defmethod rest+.index ((self rest+))
  (with-slots (data-class) self
    (let* ((data+ (class+.find data-class))
	   (index (car (class+.indexes data+)))
	   (index-definition (slot-definition-to-plist index))
	   (index-name (getf index-definition :name))
	   (index-type (getf index-definition :type))
	   (index-initarg (getf index-definition :initarg)))
      (list index-name index-type index-initarg))))

(defmethod component+.local-morphism ((class rest+) name self args body)
  (let ((class-name (class-name class)))
    `(progn
       (class+.add-method (find-class+ ',class-name) ',name 'local
			  '((,self ,class-name) ,@args))
       (defmethod ,name ((,self ,class-name) ,@args)
	 (let ((application (secure.application ,self)))
	   ,@body)))))

(defmethod rest+.list ((self rest+))
  (let* ((data+ (rest+.data+ self))
	 (name (class+.list-function self (rest+.prefix self))))
    `(defmethod/local ,name ((self ,(class-name self)))
       (,(class+.list-function data+) application))))

(defmethod rest+.find ((self rest+))
  (let* ((data+ (rest+.data+ self))
	 (name (class+.find-function self (rest+.prefix self))))
    (destructuring-bind (index-name index-type index-initarg) (rest+.index self)
      (declare (ignore index-name))
      `(defmethod/local ,name ((self ,(class-name self)) (key ,index-type))
	 (,(class+.find-function data+) application ,index-initarg key)))))

(defmethod rest+.add ((self rest+))
  (let* ((data+ (rest+.data+ self))
	 (name (class+.add-function self (rest+.prefix self)))
	 (slots (mapcar #'slot-definition-name
			(filter (lambda (slot) (slot-definition-export slot))
				(class+.local-slots data+))))
	 (ctor-lambda-list (class+.ctor-lambda-list data+ t))
	 (lambda-list (filter (lambda (a) (member (car a) slots))
			      ctor-lambda-list))
	 (arguments (class+.ctor-keyword-arguments data+ lambda-list)))
    (destructuring-bind (index-name index-type index-initarg) (rest+.index self)
      (declare (ignore index-type))
      `(defmethod/local ,name ((self ,(class-name self))
						     &key ,@lambda-list)
	 (if (,(class+.find-function data+) application ,index-initarg ,index-name)
	     (jobject :error "zaten var haci")
	     (apply ',(class+.add-function data+) application
		    (append (list :owner (secure.owner self)
				  :group (user.group (secure.owner self)))
			    ,arguments)))))))

(defmethod rest+.update ((self rest+))
  (let* ((data+ (rest+.data+ self))
	 (name (class+.update-function self (rest+.prefix self)))
	 (slots (mapcar #'slot-definition-name
			(filter (lambda (slot) (slot-definition-export slot))
				(class+.local-slots data+))))
	 (ctor-lambda-list (class+.ctor-lambda-list data+ t))
	 (lambda-list (filter (lambda (a) (member (car a) slots))
			      ctor-lambda-list))
	 (arguments (class+.ctor-keyword-arguments data+ lambda-list)))
    (destructuring-bind (index-name index-type index-initarg) (rest+.index self)
      (declare (ignore index-name))
      `(defmethod/local ,name ((self ,(class-name self))
							(key ,index-type)
							&key ,@lambda-list)
	 (aif (,(class+.find-function data+) application ,index-initarg key)
	      (apply ',(class+.update-function data+) application (cons it ,arguments))
	      (jobject :error "not found haci"))))))

(defmethod rest+.delete ((self rest+))
  (let* ((data+ (rest+.data+ self))
	 (name (class+.delete-function self (rest+.prefix self))))
    (destructuring-bind (index-name index-type index-initarg) (rest+.index self)
      (declare (ignore index-name))
      `(defmethod/local ,name ((self ,(class-name self)) (key ,index-type))
	 (aif (,(class+.find-function data+) application ,index-initarg key)
	      (,(class+.delete-function data+) application it)
	      (jobject :error "not found haci"))))))

;; -------------------------------------------------------------------------
;; defrest-crud: Macro that defines crud interface
;; -------------------------------------------------------------------------
(defmacro defrest-crud (rest-class)
  (let ((rest+ (class+.find rest-class)))
    `(progn ,(rest+.list rest+)
	    ,(rest+.find rest+)
	    ,(rest+.add rest+)
	    ,(rest+.update rest+)
	    ,(rest+.delete rest+))))

;; -------------------------------------------------------------------------
;; Abstract Rest
;; -------------------------------------------------------------------------
(defclass+ abstract-rest (secure-object)
  ((offset :initform 0 :accessor rest.offset :host local)
   (size :initform 100 :accessor rest.size :host local))
  (:metaclass rest+)
  (:default-intiargs
   :levels '(abstract-rest/anonymous abstract-rest/user)
   :permissions ((other . 1) (anonymous . 0))))

(defmethod rest.serialize ((self abstract-rest) (object t)) object)
(defmethod rest.deserialize ((self abstract-rest) (object t)) object)
(defmethod rest.get-method ((self abstract-rest) http-method key)
  (error "Implement (REST.GET-METHOD ~A)" (class-name (class-of self))))

(defmethod rest.method-call ((self abstract-rest) method-name args)
  (let ((method (class+.find-local-method (class-of self) method-name)))
    (when (null method)
      (warn "Method (~A ~A) not found." method-name (class-name (class-of self)))
      (return-from rest.method-call nil))

    (flet ((find-argument (name)
	     (aif (find (symbol-to-js name) args :key #'car :test #'string=)
		  (rest.deserialize self (json-deserialize (cdr it))))))
      (let* ((arguments (walk-lambda-list (cdddr method) nil nil :allow-specializers t))
	     (arguments (reduce (lambda (acc arg)
				  (with-slots (name) arg
				    (typecase arg
				      (keyword-function-argument-form
				       (cons (make-keyword name)
					     (cons (find-argument name) acc)))
				      (specialized-function-argument-form
				       (cons (find-argument name) acc))
				      (rest-function-argument-form
				       (append (find-argument name) acc)))))
				(nreverse arguments) :initial-value nil)))
	(with-call/cc (apply (car method) self arguments))))))


(defmethod rest.handle ((self abstract-rest) http-method key args)
  (let ((method (rest.get-method self http-method key)))
    ;; (authorize (rest.application self)
    ;; 	       (secure.owner self)
    ;; 	       (rest.serialize self (rest.method-call self method args)))
    (rest.serialize self (rest.method-call self method
					   (cons (cons "key" key) args)))))

;; -------------------------------------------------------------------------
;; Anonymous Abstract Rest
;; -------------------------------------------------------------------------
(defclass+ abstract-rest/anonymous (abstract-rest)
  ())

(defmethod rest.get-method ((self abstract-rest/anonymous) http-method key)
  (destructuring-bind (rest-list rest-find ig1 ig2 ig3 ig4)
      (class+.crud-functions (class-of self))
    (declare (ignore ig1 ig2 ig3 ig4))
    (cond
      ((eq http-method 'get)
       (if (or (null key) (equal "" key)) 
	   rest-list
	   rest-find))
      (t (error "Method ~A is unknown." http-method)))))

;; -------------------------------------------------------------------------
;; Admin Abstract Rest
;; -------------------------------------------------------------------------
(defclass+ abstract-rest/user (abstract-rest)
  ())

(defmethod rest.get-method ((self abstract-rest/user) http-method key)
  (destructuring-bind (rest-list rest-find ignr rest-add rest-update rest-delete)
      (class+.crud-functions (class-of self))
    (declare (ignore ignr))
    (cond
      ((eq http-method 'get) (if (or (null key) (equal "" key)) 
				 rest-list
				 rest-find))
      ((eq http-method 'post) rest-add)
      ((eq http-method 'put) rest-update)
      ((eq http-method 'delete) rest-delete)
      (t (error "Method ~A is unknown." http-method)))))

;; -------------------------------------------------------------------------
;; make-rest: Rest Constructor
;; -------------------------------------------------------------------------
(defun make-rest (class args &rest extra)
  (let* ((class+ (class+.find class))
	 (local-slots (class+.local-slots class+)))
    (apply #'make-instance class
	   (reduce (lambda (acc slot)
		     (with-slotdef (name initarg) slot
		       (aif (assoc (symbol-to-js name) args :test #'equal)
			    (cons initarg
				  (cons (json-deserialize (cdr it)) acc))
			    acc)))
		   local-slots :initial-value extra))))

(defmacro defrest-handler (name url authenticate-p)
  (let ((url (concat (or url (symbol-to-js name)) "\\/?(.*)")))
    `(progn
       ,(if authenticate-p `(defauth ,url http-application))
       (defhandler (,url key) ((self http-application))
	 (let* ((request (context.request +context+))
		(username (if (http-request.authenticated-p request)
			      (http-request.authenticated-user request)))
		(user (if username
			  (http-application.find-user self username)
			  (make-anonymous-user)))
		(args (uri.queries (http-request.uri request)))
		(instance (authorize self user
				     (make-rest ',name args
						:owner user
						:group (user.group user)
						:application self)))
		(result (rest.handle instance (http-request.method request)
				     key args)))
	   (component/suspend result))))))

;; -------------------------------------------------------------------------
;; defrest: Rest Macro
;; -------------------------------------------------------------------------
(defmacro defrest (name supers slots &rest rest)
  (flet ((get-param (name) (cadr (assoc name rest)))
	 (make-method (key) (intern (format nil "~A.~A" name key))))
    (let* ((data-class (or (get-param :class) (error "Provide :class")))
	   (auth (or (get-param :authenticate) nil))
	   (metaclass (or (get-param :metaclass) 'rest+))	   
	   (url (or (get-param :url) (symbol-to-js name)))
	   (rest (remove-if-member '(:metaclass :class :authenticate :url)
				   rest :key #'car))
	   (anonymous-class (intern (format nil "~A/ANONYMOUS" name)
				    (symbol-package name)))
	   (user-class (intern (format nil "~A/USER" name)
			       (symbol-package name))))
      `(progn
	 (defclass+ ,name (,@supers abstract-rest)
	   ,slots
	   (:data-class . ,data-class)
	   (:authenticate . ,auth)
	   (:metaclass ,metaclass)
	   (:url . ,url)
	   (:prefix . ,name)
	   (:default-initargs :levels '(,anonymous-class ,user-class))
	   ,@rest)
	 (defclass+ ,anonymous-class (,@supers secure-object/authorized
					       abstract-rest/anonymous)
	   ((secure-object :host lift :type ,name))
	   (:metaclass rest+)
	   (:prefix . ,name)
	   (:data-class . ,data-class))
	 (defclass+ ,user-class (,@supers secure-object/authorized
					  abstract-rest/user)
	   ((secure-object :host lift :type ,name))
	   (:prefix . ,name)
	   (:metaclass rest+)
	   (:data-class . ,data-class))
	 (defrest-handler ,name ,url ,auth)
	 (defrest-crud ,anonymous-class)
	 (defrest-crud ,user-class)
	 ;; (defrest-crud/lift ,anonymous-class ,name)
	 ;; (defrest-crud/lift ,user-class ,name)
	 (find-class ',name)))))

(deftrace rest+ '(rest+.list rest+.add rest+.find rest+.delete rest+.update))

;; -------------------------------------------------------------------------
;; <rest namespace: A Rest Client
;; -------------------------------------------------------------------------
(defcommand <rest:list (http)
  ())

(defcommand <rest:add (http)
  ((args :host local :initform nil))
  (:default-initargs :method 'post))

(defmethod run ((self <rest:add))
  (mapcar (lambda (arg)
	    (destructuring-bind (a b) arg
	      (http.add-post self a b)))
	  (s-v 'args))
  (call-next-method self))

(defclass+ rest-key-mixin ()
  ((key :host local :initform (error "Provide :key"))))

(defmethod http.setup-uri ((self rest-key-mixin))
  (let ((uri (call-next-method self)))
    (prog1 uri
      (with-slots (paths) uri
	(with-slots (key) self
	  (setf paths (if (equal (caar paths) "")
			  (list (list key))
			  (append paths (list (list key))))))))))

(defcommand <rest:find (rest-key-mixin http)
  ())

(defcommand <rest:update (rest-key-mixin http)
  ((args :host local :initform nil))
  (:default-initargs :method 'put))

(defmethod run ((self <rest:update))
  (mapcar (lambda (arg)
	    (destructuring-bind (a b) arg
	      (http.add-post self a b)))
	  (s-v 'args))
  (call-next-method self))

(defcommand <rest:delete (rest-key-mixin http)
  ()
  (:default-initargs :method 'delete))

;; Core Server: Web Application Server

;; Copyright (C) 2006-2012  Metin Evrim Ulu

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




;; Garbage below
;; (defmethod/cc rest.handle ((self abstract-rest) ())
;;   (flet ((get-local-method (http-method key)
;; 	   ))
;;     (let* ((request (context.request +context+))
;; 	   (method (or method (http-request.method request)))
;; 	   (object (authorize self (query-session :user)
;; 			      (make-instance ,'name)))
;; 	   (args (cons (cons "key" key)
;; 		       (uri.queries (http-request.uri request))))
;; 	   (result (component.serialize object
;; 		     (component.method-call object local-method args))))
	     
;;       (component/suspend result))))

;; (defmacro defrest-crud (rest-class data-class authenticate)
;;   (let* ((class+ (class+.find rest-class))
;; 	 (data+ (class+.find data-class))
;; 	 (index (car (class+.indexes data+)))
;; 	 (index-definition (slot-definition-to-plist index))
;; 	 (index-name (getf index-definition :name))
;; 	 (index-type (getf index-definition :type))
;; 	 (index-initarg (getf index-definition :initarg))
;; 	 (slots (mapcar #'slot-definition-name
;; 			(filter (lambda (slot) (slot-definition-export slot))
;; 				(class+.local-slots data+))))
;; 	 (ctor-lambda-list (class+.ctor-lambda-list data+ t))
;; 	 (lambda-list (filter (lambda (a) (member (car a) slots))
;; 			      ctor-lambda-list))
;; 	 (arguments (class+.ctor-keyword-arguments class+ lambda-list)))
;;     (destructuring-bind (list find query add update delete)
;; 	(class+.crud-functions class+)
;;       (declare (ignore query))
;;       (destructuring-bind (crud-list crud-find crud-query crud-add
;; 				     crud-update crud-delete)
;; 	  (class+.crud-functions data+)
;; 	(declare (ignore crud-query))
;; 	`(progn
;; 	   (defmethod/local ,list ((self ,rest-class))
;; 	     (,crud-list application))
;; 	   (defmethod/local ,find ((self ,rest-class) (key ,index-type))
;; 	     (,crud-find application ,index-initarg key))
;; 	   (defmethod/local ,add ((self ,rest-class) &key ,@lambda-list)
;; 	     (if (,crud-find application ,index-initarg ,index-name)
;; 		 (jobject :error "zaten var haci")
;; 		 (apply ',crud-add application
;; 			,(if authenticate
;; 			     `(append ,arguments
;; 			       ,(if authenticate
;; 				    `(list :owner (secure.owner self)
;; 					   :group (user.group
;; 						   (secure.owner self)))))
;; 			     arguments))))
;; 	   (defmethod/local ,update ((self ,rest-class) (key ,index-type)
;; 				     &key ,@lambda-list)
;; 	     (aif (,crud-find application ,index-initarg key)
;; 		  (apply ',crud-update application (cons it ,arguments))
;; 		  (jobject :error "not found haci")))
;; 	   (defmethod/local ,delete ((self ,rest-class) (key ,index-type))
;; 	     (aif (,crud-find application ,index-initarg key)
;; 		  (,crud-delete application it)
;; 		  (jobject :error "not found haci"))))))))

;; (defmacro defrest-handler (name url auth)
;;   (let ((url (concat (or url (symbol-to-js name)) "\\/?(.*)")))
;;     `(progn
;;        ,(if auth `(defauth ,url http-application))
;;        (defhandler (,url key) ((self http-application))
;; 	 (flet ((%make-component (args)
;; 		  ,(if auth
;; 		       `(let* ((request (context.request +context+))
;; 			       (username (http-request.authenticated-user request))
;; 			       (user (http-application.find-user self username))
;; 			       (component (make-component-from-args
;; 					   ',name args
;; 					   :owner user
;; 					   :group (user.group user))))
;; 			  ;; (authorize self user component)
;; 			  component)
;; 		       `(make-component-from-args ',name args))))
;; 	   (let* ((request (context.request +context+))		  
;; 		  (args (if key
;; 			    (cons (cons "key" key)
;; 				  (uri.queries (http-request.uri request)))
;; 			    (uri.queries (http-request.uri request))))
;; 		  (component (%make-component args)) 
;; 		  (method (rest.get-method component
;; 					   (http-request.method request)
;; 					   key)))	     
;; 	     (let ((result (component.serialize component
;; 			     (component.method-call component method args))))
	     
;; 	       (component/suspend result))))))))


;; (defmacro defrest-crud/lift (target-class source-class)
;;   (let ((source (class+.find source-class))
;; 	(target (class+.find target-class)))
;;     (destructuring-bind (lst find query add update delete) (class+.crud-functions
;; 							    source)
;;       (declare (ignore query))
;;       `(progn
;; 	 ,@(mapcar
;; 	    (lambda (method)
;; 	      (let ((method (class+.find-local-method source method)))
;; 		(when method
;; 		  (destructuring-bind (name type &rest args) method
;; 		    (declare (ignore type))
;; 		    `(defmethod/lift ,name
;; 			 ((,(caar args) ,(class-name target)) ,@(cdr args)))))))
;; 	    (list lst find add update delete))))))