(in-package :core-server)

;; -------------------------------------------------------------------------
;; Rest API
;; -------------------------------------------------------------------------
(defclass+ rest+ (component+)
  ((url :initarg :url)
   (authenticate :initarg :authenticate :initform nil)))

(defcomponent abstract-rest (secure-object)
  ((offset :initform 0 :accessor rest.offset :host local)
   (size :initform 100 :accessor rest.size :host local))
  (:metaclass rest+))

(defun make-component-from-args (class args &rest extra)
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

(defmethod/cc rest.get-method ((self abstract-rest) http-method key)
  (destructuring-bind (rest-list rest-find rest-query rest-add rest-update
				 rest-delete)
      (class+.crud-functions (class-of self))
    (declare (ignore rest-query))
    (cond
      ((eq http-method 'get) (if (or (null key) (equal "" key)) 
				 rest-list
				 rest-find))
      ((eq http-method 'post) rest-add)
      ((eq http-method 'put) rest-update)
      ((eq http-method 'delete) rest-delete)
      (t (error "Method ~A is unknown." http-method)))))

(defmacro defrest-handler (name url auth)
  (let ((url (concat (or url (symbol-to-js name)) "\\/?(.*)")))
    `(progn
       ,(if auth `(defauth ,url http-application))
       (defhandler (,url key) ((self http-application))
	 (flet ((%authorize (component)
		  ,(if auth
		       `(authorize self (query-session :user) component)
		       `component)))
	   (let* ((request (context.request +context+))		  
		  (args (if key
			    (cons (cons "key" key)
				  (uri.queries (http-request.uri request)))
			    (uri.queries (http-request.uri request))))
		  (component (make-component-from-args
			      ',name args :owner (make-anonymous-user)
			      :group (make-anonymous-group)))
		  (component ,(if auth
				  `(%authorize component)
				  'component))
		  (method (rest.get-method component
					   (http-request.method request)
					   key)))	     
	     (let ((result (component.serialize component
			     (component.method-call component method args))))
	     
	       (component/suspend result))))))))

(defmacro defrest-crud (rest-class data-class)
  (let* ((class+ (class+.find rest-class))
	 (data+ (class+.find data-class))
	 (index (car (class+.indexes data+)))
	 (index-definition (slot-definition-to-plist index))
	 (index-name (getf index-definition :name))
	 (index-type (getf index-definition :type))
	 (index-initarg (getf index-definition :initarg))
	 (slots (mapcar #'slot-definition-name
			(filter (lambda (slot) (slot-definition-export slot))
				(class+.local-slots data+))))
	 (ctor-lambda-list (class+.ctor-lambda-list data+ t))
	 (lambda-list (filter (lambda (a) (member (car a) slots))
			      ctor-lambda-list))
	 (arguments (class+.ctor-keyword-arguments class+ lambda-list)))
    (destructuring-bind (list find query add update delete)
	(class+.crud-functions class+)
      (declare (ignore query))
      (destructuring-bind (crud-list crud-find crud-query crud-add
				     crud-update crud-delete)
	  (class+.crud-functions data+)
	(declare (ignore crud-query))
	`(progn
	   (defmethod/local ,list ((self ,rest-class))
	     (,crud-list application))
	   (defmethod/local ,find ((self ,rest-class) (key ,index-type))
	     (,crud-find application ,index-initarg key))
	   (defmethod/local ,add ((self ,rest-class)
				  &key ,@lambda-list)
	     (if (,find self ,index-name)
		 (jobject :error "zaten var haci")
		 (apply ',crud-add application ,arguments)))
	   (defmethod/local ,update ((self ,rest-class) (key ,index-type)
				     &key ,@lambda-list)
	     (aif (,find self key)
		  (apply ',crud-update application (cons it ,arguments))
		  (jobject :error "not found haci")))
	   (defmethod/local ,delete ((self ,rest-class) (key ,index-type))
	     (aif (,find self key)
		  (,crud-delete application it)
		  (jobject :error "not found haci"))))))))

;; -------------------------------------------------------------------------
;; Rest Macro
;; -------------------------------------------------------------------------
(defmacro defrest (name supers slots &rest rest)
  (flet ((get-param (name) (cadr (assoc name rest)))
	 (make-method (key) (intern (format nil "~A.~A" name key))))
    (let* ((metaclass (or (get-param :metaclass) 'rest+))
	   (auth (or (get-param :authenticate) nil))
	   (data-class (or (get-param :class) (error "Provide :class")))
	   (rest (remove-if-member '(:metaclass) rest :key #'car)))
      `(progn
	 (defcomponent ,name (,@supers abstract-rest)
	   ,slots
	   (:metaclass ,metaclass)
	   ,@rest)
	 (defrest-handler ,name ,(get-param :url) ,auth)
	 (defrest-crud ,name ,data-class)
	 (find-class ',name)))))

;; -------------------------------------------------------------------------
;; Rest Client
;; -------------------------------------------------------------------------
(defclass+ rest-key-mixin ()
  ())

(defmethod http.setup-uri ((self rest-key-mixin))
  (let ((uri (call-next-method self)))
    (prog1 uri
      (with-slots (paths) uri
	(with-slots (key) self
	  (setf paths (if (equal (caar paths) "")
			  (list (list key))
			  (append paths (list (list key))))))))))

(defcommand <rest:add (http)
  ((args :host local :initform nil))
  (:default-initargs :method 'post))

(defmethod run ((self <rest:add))
  (mapcar (lambda (arg)
	    (destructuring-bind (a b) arg
	      (http.add-post self a b)))
	  (s-v 'args))
  (call-next-method self))

(defcommand <rest:find (rest-key-mixin http)
  ((key :host local :initform (error "Provide :key"))))

(defcommand <rest:list (http)
  ())

(defcommand <rest:update (rest-key-mixin http)
  ((key :host local :initform (error "Provide :key"))))

(defcommand <rest:delete (rest-key-mixin http)
  ((key :host local :initform (error "Provide :key"))))

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
