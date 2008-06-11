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

;; Component Procotol
(defclass component ()
  ((local-args :accessor component.local-args :initarg :local-args :initform '())
   (remote-args :accessor component.remote-args :initarg :remote-args :initform '())
   (application :accessor application :initarg :application :initform nil)))

(defmethod application ((self component))
  (or (s-v 'application) (application +context+)))

(defgeneric/cc send/component (component)
  (:documentation "Send component to remote."))

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defun proxy-method-name (name)
    (intern (string-upcase (format nil "~A-proxy" name)) (find-package :core-server)))

  (defun proxy-getter-name (name)
    (intern (string-upcase (format nil "get-~A" name)) (find-package :core-server)))

  (defun proxy-setter-name (name)
    (intern (string-upcase (format nil "set-~A" name)) (find-package :core-server))))

(defmacro defmethod/local (name ((self class-name) &rest args) &body body)    
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (register-local-method-for-class ',class-name ',name))
     (defgeneric/cc ,name (,class-name ,@args))
     (defgeneric/cc ,(proxy-method-name name) (,class-name))
     (defmethod/cc ,(proxy-method-name name) ((,self ,class-name))
       `(lambda ,',args
	  (return
	    (funcall
	     ,(action/url ,(mapcar (lambda (arg) (list arg (js::symbol-to-js arg))) args)
		(let ,(mapcar (lambda (arg) `(,arg (json-deserialize ,arg))) args)
		  (json/suspend
		   (lambda ()
		     (json! (http-response.stream (response +context+))
			    (apply (symbol-function ',name) (list ,self ,@args)))))))
	     ,',(if args		    
		    (cons 'create (reduce #'append
					  (mapcar (lambda (arg)
						    `(,(make-keyword arg) ,arg))
						  args)
					  :initial-value nil)))))))
     (defmethod/cc ,name ((,self ,class-name) ,@args) ,@body)))

(defmacro defmethod/remote (name ((self class-name) &rest args) &body body)
  (let ((arg-names (arnesi:extract-argument-names args :allow-specializers t)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (register-remote-method-for-class ',class-name ',name))
       (defgeneric/cc ,name (,class-name ,@args))
       (defgeneric/cc ,(proxy-method-name name) (,class-name))
       (defmethod/cc ,(proxy-method-name name) ((,self ,class-name))	      
	 `(lambda ,',arg-names
	    ,',(cons 'progn body)))
       (defmethod/cc ,name ((,self ,class-name) ,@args)
	 (javascript/suspend
	  (lambda ()
	    (<:js
	     `(funcall ,(action/url ((result "result")) (answer (json-deserialize result)))
		       (create :result (serialize (,',name this ,,@args)))))))))))

(defun serialize-to-parenscript (type object)
  (ecase type
    (primitive object)
    (object
     (etypecase object
       (list
	`(create ,@(if (and (listp (car object)) (not (null (caar object))))
		       (reduce (lambda (acc atom)
				 (cons (make-keyword (car atom))
				       (cons (cdr atom) acc)))
			       object :initial-value nil)
		       object)))
       (hash-table
	`(create ,@(let (acc)
                     (maphash (lambda (k v) (push (list (make-keyword k) v) acc)) object)
		     (reduce #'append acc :initial-value nil))))))
    (array `(array ,@object))))

(defmethod/cc send/component ((self component))
  (let ((class-name (class-name (class-of self))))
    (flet ((local-slots ()
	     (reduce (lambda (acc slot)
		       (cons (make-keyword slot) (cons 'null acc)))
		     (reverse (mapcar (compose #'car #'ensure-list)
				      (local-slots-of-class class-name)))
		     :initial-value nil))
	   (remote-slots ()
	     (reduce (lambda (acc slot)
		       (cons (make-keyword slot)
			     (cons (serialize-to-parenscript
				    (client-type-of-slot class-name slot)
				    (if (slot-boundp self slot)
					(slot-value self slot)))
				   acc)))
		     (reverse (mapcar (compose #'car  #'ensure-list)
				      (remote-slots-of-class class-name)))
		     :initial-value nil))
	   (local-methods ()
	     (reduce (lambda (acc method)
		       (cons (make-keyword method)
			     (cons (funcall
				    (symbol-function
				     (proxy-method-name method)) self)
				   acc)))
		     (local-methods-of-class class-name)
		     :initial-value nil))
	   (remote-methods ()
	     (reduce (lambda (acc method)
		       (cons (make-keyword method)
			     (cons (funcall (symbol-function
					     (proxy-method-name method)) self) acc)))
		     (remote-methods-of-class class-name)
		     :initial-value nil)))
      (send/ctor self (remote-slots) (local-methods) (remote-methods)))))

(defmethod/cc send/ctor ((self component) remote-slots local-methods remote-methods)
  (<:js
   `(setf ,(class-name (class-of self))
	  (lambda (,@(reduce (lambda (acc slot)
			       (if (keywordp slot)
				   (cons (intern (symbol-name slot)) acc)
				   acc))
			     remote-slots :initial-value nil))
	    (setf this.prototype (create ;; ,@(local-slots)
				  ,@remote-slots
				  ,@local-methods ,@remote-methods))
	    ,@(reduce (lambda (acc slot)
			(if (keywordp slot)
			    (cons `(if (not (= "undefined" (typeof ,(intern (symbol-name slot)))))
				       (setf (slot-value this.prototype ',(intern (symbol-name slot)))
					     ,(intern (symbol-name slot)))) acc)
			    acc))
		      remote-slots :initial-value nil)
	    (return this.prototype)))))

(defmacro defcomponent (name supers slots &rest rest)
  (multiple-value-bind (slots new-rest) (register-class name supers slots rest)
    `(prog1 (defclass ,name (,@supers component)
	      ,slots
	      (:default-initargs ,@(alist-to-plist (default-initargs-of-class name)))
	      ,@(remove :default-initargs new-rest :key #'car))
       (defun ,name (&key ,@(local-slots-of-class name) ,@(remote-slots-of-class name))
	 (apply #'make-instance ',name (list ,@(ctor-arguments name))))
       ,@(mapcar (lambda (slot)
		   `(progn
		      (defmethod/local ,(proxy-getter-name (car slot)) ((self ,name))
			(slot-value self ',(car slot)))
		      (defmethod/local ,(proxy-setter-name (car slot)) ((self ,name) value)
			(setf (slot-value self ',(car slot)) value))))
		 (mapcar #'ensure-list (local-slots-of-class name)))
       ,@(mapcar (lambda (slot)
		   `(progn
		      (defmethod/remote ,(proxy-getter-name (car slot)) ((self ,name))
			(return (slot-value this ',(car slot))))
		      (defmethod/remote ,(proxy-setter-name (car slot)) ((self ,name) value)			
			(setf (slot-value this ',(car slot)) value)
			(return (slot-value this ',(car slot))))))
		 (mapcar #'ensure-list (remote-slots-of-class name))))))

(defun/cc dojo (&optional base-url (debug nil) (prevent-back-button 'false)
		(css (reduce (lambda (acc a)
			       (cons (concatenate
				      'string +dojo-path+ ".." a)
				     acc))
			     (reverse
			      '("/dijit/themes/dijit.css"
				"/dijit/themes/tundra/tundra.css"
				"/dojox/widget/Toaster/Toaster.css"))
			     :initial-value '("http://node1.core.gen.tr/coretal/style/coretal.css"))))
  (<:js
   `(progn
      (defun load-javascript (url)
	  (let ((request nil))
	    (cond
	      (window.*x-m-l-http-request ;; Gecko
	       (setf request (new (*x-m-l-http-request))))
	      (window.*active-x-object ;; Internettin Explorer
	       (setf request (new (*active-x-object "Microsoft.XMLHTTP")))))
	    (if (= null request)
		(throw (new (*error "Cannot Load Javascript, -core-server 1.0"))))
	    (setf req request)
	    (request.open "GET" url false)
	    (request.send null)
	    (if (= 200 request.status)
		(return (eval (+ "{" request.response-text "}"))))
	    (throw (new (*error (+ "Cannot load javascript:" url " -core-server 1.0"))))))
      (defun load-css (url)
	(let ((link (document.create-element "link")))
	  (setf link.href url
		link.rel "stylesheet"
		link.type "text/css")
	  (.append-child (aref (document.get-elements-by-tag-name "head") 0)
			 link)
	  (return link)))
      (defun init-core-server ()
	(when (= "undefined" (typeof dojo))
	  (setf dj-config (create :base-url ,+dojo-path+ :is-debug ,debug
				  :prevent-back-button-fix ,prevent-back-button
;;				  :dojo-iframe-history-url "./resources/iframe_history.html"
				  ))      
	  (dolist (src (array "bootstrap.js" "loader.js" "hostenv_browser.js" "loader_xd.js"))	
	    (load-javascript (+ ,+dojo-path+ "_base/_loader/" src)))
	  (load-javascript (+ ,+dojo-path+ "_base.js")))
	(setf base-url ,(if (and +context+ base-url)
			    (format nil "/~A/~A"
				    (web-application.fqdn (application +context+))
				    base-url)))
	(dojo.require "dojo.back")
	(dojo.back.init)
	,@(mapcar (lambda (c) `(load-css ,c)) css)
	(dojo.add-on-load
	 (lambda ()
	   (setf document.body.class-name (+ document.body.class-name " tundra")))))
      (defun serialize (value) (return (dojo.to-json value)))
      (defun funcall (url parameters retry-count)
	(let (result)
	  (debug "server.funcall " url)
	  (when (dojo.is-object parameters)
	    (doeach (param parameters)
		    (setf (slot-value parameters param)
			  (serialize (slot-value parameters param)))))
	  (dojo.xhr-post
	   (create :url (+ base-url url)
		   :handle-as "text"
		   :sync t
		   :timeout 10
		   :content parameters
		   :load (lambda (json args)
;;			   (debug json)
			   (setf result (eval (+ "{" json "}"))))
		   :error (lambda (err args)
			    (if (= err.status 500)				
				(if (= "undefined" (typeof retry-count))
				    (return (funcall url parameters 5))
				    (if (> retry-count 0)
					(return (funcall url parameters (- retry-count 1)))))
				(throw (new (*error (+ "Funcall error: " url ", " err))))))))
	  (return result)))
      (init-core-server))))

;; (eval-when (:execute :compile-toplevel :load-toplevel)
;;   (defvar +component-registry+ (make-hash-table :test #'equal)))

;; (defun reduce-class-tree (name type)
;;   (let ((lst))
;;     (mapcar (lambda (atom)
;; 	      (pushnew atom lst
;; 		       :key #'(lambda (a) (if (atom a) a (car a)))
;; 		       :test #'eq))
;; 	    (reduce #'append
;; 		    (mapcar (lambda (atom)
;; 			      (getf (gethash (class-name atom) +component-registry+) type))
;; 			    (cons (find-class name) (class-superclasses (find-class name))))
;; 		    :initial-value nil))
;;     lst))

;; (defun local-methods-of-class (name) (reduce-class-tree name :local-methods))

;; (defun remote-methods-of-class (name) (reduce-class-tree name :remote-methods))

;; (defun local-slots-of-class (name)
;;   (reduce-class-tree name :local-args))

;; (defun remote-slots-of-class (name)
;;   (reduce-class-tree name :remote-args))

;; (defun client-type-for-slot (name slot)
;;   (any #'(lambda (atom)
;; 	   (cdr (assoc slot (getf (gethash (class-name atom) +component-registry+) :client-types))))
;;        (cons (find-class name) (class-superclasses (find-class name)))))

;; (defun add-local-method-for-class (name method-name)
;;   (setf (getf (gethash name +component-registry+) :local-methods)
;; 	(cons method-name
;; 	      (remove method-name
;; 		      (getf (gethash name +component-registry+) :local-methods)))))

;; (defun add-remote-method-for-class (name method-name)
;;   (setf (getf (gethash name +component-registry+) :remote-methods)
;; 	(cons method-name
;; 	      (remove method-name
;; 		      (getf (gethash name +component-registry+) :remote-methods)))))

;; (defmacro defcomponent (name supers slots &rest default-initargs)
;;   (labels ((class-default-initargs (class)
;; 	     (getf (gethash class +component-registry+) :default-initargs))
;; 	   (class-superclasses (class)
;; 	     (cons class
;; 		   (reduce #'append
;; 			   (mapcar #'class-superclasses
;; 				   (getf (gethash class +component-registry+) :supers)))))
;; 	   (filter-slot (slot-def)
;; 	     (when (or (eq 'local (getf (cdr slot-def) :host))
;; 		       (eq 'both  (getf (cdr slot-def) :host)))
;; 	       (unless (getf (cdr slot-def) :initarg)
;; 		 (setf (getf (cdr slot-def) :initarg) (make-keyword (car slot-def)))))
;; 	     (unless (getf (cdr slot-def) :accessor)
;; 	       (setf (getf (cdr slot-def) :accessor) (car slot-def)))
;; 	     (remf (cdr slot-def) :host)
;; 	     (remf (cdr slot-def) :client-type)
;; 	     slot-def)
;; 	   (remote-slot (acc slot-def)
;; 	     (if (or (eq 'remote (getf (cdr slot-def) :host))
;; 		     (eq 'both   (getf (cdr slot-def) :host)))
;; 		 (cons (list (car slot-def) (getf (cdr slot-def) :initform)) acc)
;; 		 acc))
;; 	   (local-slot (acc slot-def)
;; 	     (if (or (eq 'local (getf (cdr slot-def) :host))
;; 		     (eq 'both  (getf (cdr slot-def) :host)))
;; 		 (cons (list (car slot-def) (getf (cdr slot-def) :initform)) acc)
;; 		 acc))
;; 	   (local-args (slotz)
;; 	     (let ((args (append
;; 			  (nreverse (reduce #'local-slot slotz :initial-value nil))
;; 			  (reduce #'(lambda (acc super)
;; 				      (append acc (getf (gethash super +component-registry+)
;; 							:local-args)))
;; 				  (reduce #'append (mapcar #'class-superclasses supers))
;; 				  :initial-value nil)))
;; 		   (super-args
;; 		    (reduce #'append (mapcar #'class-default-initargs supers))))
;; 	       (setf args		     
;; 		     (reduce
;; 		      #'(lambda (acc arg)
;; 			  (let ((value ;; (cadr (assoc (car arg) super-args :test #'string=))
;; 				 (getf super-args (make-keyword (car arg)))
;; 				  ))
;; 			    (if value
;; 				(cons (list (car arg) value) acc)
;; 				(cons arg acc))))
;; 		      args :initial-value nil))
;; 	       (reduce #'(lambda (acc arg)
;; 			   (pushnew arg acc :key #'car :test #'equal)
;; 			   acc)
;; 		       (reduce #'(lambda (acc arg)
;; 				   (let ((value (getf (cdar default-initargs) (make-keyword (car arg)))))
;; 				     (if value
;; 					 (cons (list (car arg) value) acc)
;; 					 (cons arg acc))))
;; 			       args :initial-value nil)
;; 		       :initial-value nil)))
;; 	   (remote-args (slotz)
;; 	     (let ((args (append
;; 			  (nreverse (reduce #'remote-slot slotz :initial-value nil))
;; 			  (reduce #'(lambda (acc super)
;; 				      (append acc (getf (gethash super +component-registry+)
;; 							:remote-args)))
;; 				  (reduce #'append (mapcar #'class-superclasses supers))
;; 				  :initial-value nil)))
;; 		   (super-args
;; 		    (reduce #'append (mapcar #'class-default-initargs supers))))
;; 	       (setf args		     
;; 		     (reduce
;; 		      #'(lambda (acc arg)
;; 			  (let ((value ;; (cadr (assoc (car arg) super-args :test #'string=))
;; 				 (getf super-args (make-keyword (car arg)))
;; 				  ))
;; 			    (if value
;; 				(cons (list (car arg) value) acc)
;; 				(cons arg acc))))
;; 		      args :initial-value nil))
;; 	       (reduce #'(lambda (acc arg)
;; 			   (pushnew arg acc :key #'car :test #'eq))
;; 		       (reduce #'(lambda (acc arg)
;; 				   (let ((value (getf (cdar default-initargs)
;; 						      (make-keyword (car arg)))))
;; 				     (if value
;; 					 (cons (list (car arg) value) acc)
;; 					 (cons arg acc))))
;; 			       args :initial-value nil)
;; 		       :initial-value nil)))
;; 	   (function-key-args (slotz)
;; 	     (reduce #'(lambda (acc slot-def)			 
;; 			 (cons (make-keyword (car slot-def))
;; 			       (cons (car slot-def) acc)))
;; 		     (append (remote-args slotz) (local-args slotz)) :initial-value nil))
;; 	   (filter-default-initargs (lst)
;; 	     (nreverse (reduce #'(lambda (acc item)
;; 				   (if (or (eq item :default-initargs)
;; 					   (eq item :local-args)
;; 					   (eq item :remote-args))
;; 				       acc
;; 				       (cons item acc)))
;; 			       lst :initial-value nil)))
;; 	   (client-type (slot)
;; 	     (cons (car slot) (or (getf (cdr slot) :client-type) 'primitive))))    
;;     `(prog1
;; 	 (eval-when (:compile-toplevel :load-toplevel :execute)
;; 	   (export ',name (find-package ,(package-name (symbol-package name))))
;; 	   (setf (getf (gethash ',name +component-registry+) :supers) ',supers
;; 		 (getf (gethash ',name +component-registry+) :default-initargs) ',(cdar default-initargs)
;; 		 (getf (gethash ',name +component-registry+) :local-args) ',(local-args slots)
;; 		 (getf (gethash ',name +component-registry+) :remote-args) ',(remote-args slots)
;; 		 (getf (gethash ',name +component-registry+) :client-types) ',(mapcar #'client-type slots))
;; 	   (defclass ,name (,@supers component)
;; 	     ,(mapcar #'filter-slot (copy-tree slots))
;; 	     (:default-initargs ,@(filter-default-initargs (car default-initargs)))
;; 	     ,@(cdr default-initargs)))
;;        (defun ,(intern (string-upcase name) (symbol-package name))
;; 	   (&key ,@(local-args slots) ,@(remote-args slots))
;; 	 (apply #'make-instance ',name (list ,@(function-key-args slots))))
;;        ,@(mapcar (lambda (slot)
;; 		   `(progn
;; 		      (defmethod/local ,(proxy-getter-name (car slot)) ((self ,name))
;; 			(slot-value self ',(car slot)))
;; 		      (defmethod/local ,(proxy-setter-name (car slot)) ((self ,name) value)
;; 			(setf (slot-value self ',(car slot)) value))))
;; 		 (local-args slots))
;;        ,@(mapcar (lambda (slot)
;; 		   `(progn
;; 		      (defmethod/remote ,(proxy-getter-name (car slot)) ((self ,name))
;; 			(return (slot-value this ',(car slot))))
;; 		      (defmethod/remote ,(proxy-setter-name (car slot)) ((self ,name) value)			
;; 			(setf (slot-value this ',(car slot)) value)
;; 			(return (slot-value this ',(car slot))))))
;; 		 (remote-args slots)))))
