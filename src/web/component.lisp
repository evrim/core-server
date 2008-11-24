;; +----------------------------------------------------------------------------
;; | Component Framework (Lisp->Browser Functor)
;; +----------------------------------------------------------------------------
(in-package :core-server)

;; +----------------------------------------------------------------------------
;; | Component Metaclass
;; +----------------------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass component+ (class+)
    ((%ctor-timestamp :initform 0))))

;; +----------------------------------------------------------------------------
;; | Component Class
;; +----------------------------------------------------------------------------
(defclass+ component ()
  ((url :host remote)
   (service-name :host remote :initarg :service-name :initform nil))
  (:metaclass component+)
  (:documentation "Base component class"))

(defmethod shared-initialize :after ((self component) slots &key &allow-other-keys)
  (if (or (not (slot-boundp self 'id)) (null (slot-value self 'id)))
      (setf (slot-value self 'id) (random-string 5)))
  (if (typep self 'xml)
      (setf (xml.children self)
	    (cons 
	     (<:script :type "text/javascript"
		       (with-call/cc		
			 (lambda (stream)
			   (let ((stream (make-indented-stream stream)))
			     (component! stream self)
			     (when (typep self 'xml)
			       (write-stream stream
					     (js*
					       `(progn
						  (new
						   (,(class-name (class-of self))
						     (create)
						     (document.get-element-by-id ,(slot-value self 'id))))))))))))
	     (xml.children self))))
  self)

(defmethod component.application ((self component))
  "Returns application associated with this component."
  (context.application +context+))

;; +----------------------------------------------------------------------------
;; | defmethod/local macro: Defines a local method
;; +----------------------------------------------------------------------------
(defmacro defmethod/local (name ((self class-name) &rest args) &body body)
  (let ((metaclass (class-name (class-of (find-class class-name))))
	(proxy (intern (format nil "~A/JS" name)))
	(js-method-name (intern (format nil ".~A" name))))
    `(progn
       (class+.add-method (find-class+ ',class-name) ',name 'local '((,self ,class-name) ,@args))
       (eval-when (:load-toplevel :compile-toplevel :execute)
	 (defjsmacro ,name (&rest args) `(,',js-method-name ,@args)))
       (defmethod ,proxy ((self ,metaclass) k)
	 `(lambda ,',args
	    (this.funkall ,k
	     (create
	      ,@',(nreverse
		   (reduce0 (lambda (acc arg)
			      (cons arg (cons (make-keyword arg) acc)))
			    (extract-argument-names args :allow-specializers t)))))))
       (defmethod/cc ,name ((,self ,class-name) ,@args) ,@body))))

;; +----------------------------------------------------------------------------
;; | defmethod/remote macro: Defines a remote method
;; +----------------------------------------------------------------------------
(defmacro defmethod/remote (name ((self class-name) &rest args) &body body)
  (let ((metaclass (class-name (class-of (find-class class-name))))
	(proxy (intern (format nil "~A/JS" name)))
	(js-method-name (intern (format nil ".~A" name)))
	(call-next-method-p (any (lambda (application-form)
				   (eq 'call-next-method (operator application-form)))
				 (ast-search-type (walk-form `(lambda () ,@body))
						  (list 'application-form)))))
    `(progn
       (class+.add-method (find-class+ ',class-name) ',name 'remote '((,self ,class-name) ,@args))
       (eval-when (:load-toplevel :compile-toplevel :execute)
	 (defjsmacro ,name (&rest args) `(,',js-method-name ,@args)))
       (defmethod ,proxy ((self ,metaclass) k)
	 (declare (ignore k))
	 ,(if call-next-method-p
	      ``(lambda ,',args
		  (let ((,',self (or ,',self this)))
		    (flet ((call-next-method () ,@(cddr (call-next-method))))
		      ,@',body)))
	      ``(lambda ,',args
		  (let ((,',self this))
		    ,@',body))))
       (defmethod/cc ,name ((,self ,class-name) ,@args)
	 (let ((hash (action/url ((result "result"))
		       (answer (json-deserialize result)))))
	   (javascript/suspend
	    (lambda (stream)
	      (with-js (hash) stream
		(this.funkall hash (create :result (serialize (,name self ,@args))))))))))))

;; ----------------------------------------------------------------------------
;; defcomponent-accessors Macro: Defines remote and local accessors
;; for slots of a component
;; ----------------------------------------------------------------------------
(defmacro defcomponent-accessors (class-name slots)
  (flet ((reader (name) (intern (format nil "GET-~A" name) (symbol-package name)))
	 (writer (name) (intern (format nil "SET-~A" name) (symbol-package name))))    
    `(progn
       ,@(reduce0 (lambda (acc slot)
		    (flet ((method-type (host)
			     (ecase host
			       (local 'defmethod/local)
			       ((or both remote) 'defmethod/remote))))
		      (let ((name (car slot))
			    (host (cdr slot)))			
			(append acc
			 `((,(method-type host) ,(reader name) ((self ,class-name))
			     (slot-value self ',name))
			   (,(method-type host) ,(writer name) ((self ,class-name) value)
			     (setf (slot-value self ',name) value))
			   (defmacro/js ,name (self)
			     `(,',(reader name) ,self))
			   (defsetf/js ,name (value self)
			     `(,',(writer name) ,self ,value)))))))
		  slots))))

;; +----------------------------------------------------------------------------
;; | defcomponent Macro: Defines a new component
;; +----------------------------------------------------------------------------
(defmacro defcomponent (name supers slots &rest rest)
  (let* ((metaclass (intern (format nil "~A+" name)))
	 (metasupers (uniq
		      (append
		       (mapcar (compose #'class-name #'class-of #'find-class) supers)
		       (list 'component+))))
	 (dom-classes (filter (lambda (a) (typep a 'xml+))
				   (mapcar #'find-class supers)))
	 (tag (any (lambda (a) (if (not (null a)) a))
		   (mapcar #'xml+.tag dom-classes)))
	 (namespace (any (lambda (a) (if (not (null a)) a))
			 (mapcar #'xml+.namespace dom-classes)))
	 (attributes (any (lambda (a) (if (not (null a)) a))
			  (mapcar #'xml+.attributes dom-classes))))
    `(progn
       (eval-when (:load-toplevel :execute :compile-toplevel)
	 (defclass ,metaclass ,metasupers
	   ()
	   (:default-initargs
	    :tag (list ,tag)
	     :namespace (list ,namespace)
	     :attributes (list ',attributes))))
       (defclass+ ,name (,@supers component)
	 ,slots
	 ,@rest
	 (:metaclass ,metaclass))
       (defcomponent-accessors ,name ,(mapcar (lambda (slot)
						(cons (car slot)
						      (or (getf (cdr slot) :host)
							  'local)))
					      slots))
       (find-class+ ',name))))

;; ----------------------------------------------------------------------------
;; This around method allows us to compile constructor evertime class
;; definition changed.
;; ----------------------------------------------------------------------------
(defmethod/cc component! ((stream core-stream) (component component))
  (error "This component! method should not be called."))

(defmethod/cc component! :around ((stream core-stream) (component component))
  (if  (> (slot-value (class-of component) '%timestamp)
	  (slot-value (class-of component) '%ctor-timestamp))      
       (let ((name (class-name (class-of component))))
	 (format *standard-output* "Compiling constructor for ~A.~%" name)
	 (eval (component+.ctor (class-of component)))
	 (setf (slot-value (class-of component) '%ctor-timestamp)
	       (get-universal-time))
	 (component! stream component))
       (call-next-method stream component)))

(defmethod component+.ctor ((component component+))
  (let* ((class-name (class-name component))
	 (class+ (find-class+ class-name))
	 (k-urls (mapcar (lambda (m) (declare (ignore m)) (gensym))
			 (class+.local-methods class+)))
	 (remote-slots (mapcar (lambda (slot)
				 (with-slotdef (name reader initarg) slot
				   (list name (intern (symbol-name (gensym)))
					 reader (intern (symbol-name initarg)))))
			       (class+.remote-slots class+)))
	 (dom-class (any (lambda (class) (if (typep class 'xml+) class))
			 (cdr (class+.superclasses class+)))))
    `(progn
       ;; ----------------------------------------------------------------------------
       ;; Component Internal Render Method 
       ;; ----------------------------------------------------------------------------
       (defmethod %component! ((stream core-stream) (component ,class-name) &rest k-urls)
	 (with-accessors (,@(mapcar (lambda (slot) (list (cadr slot) (caddr slot)))
				    remote-slots)) component
	   (destructuring-bind (,@k-urls) k-urls
	     (with-js (,@k-urls ,@(mapcar #'cadr remote-slots)) stream
	       ;; ----------------------------------------------------------------------------
	       ;; Constructor
	       ;; ----------------------------------------------------------------------------
	       (defun ,class-name (properties to-extend)
		 (if (null to-extend)
		     (setf to-extend ,(if dom-class
					  `(document.create-element ,(symbol-to-js (or (xml+.tag dom-class)
										       (class-name dom-class))))
					  `(new (*object)))))

		 (let ((prototype
			(create
			 ;; ----------------------------------------------------------------------------
			 ;; Remote Slot Initial Values
			 ;; ----------------------------------------------------------------------------
			 ,@(reduce0 (lambda (acc slot)
				      (cons (make-keyword (car slot))
					    (cons (cadr slot) acc)))
				    remote-slots)
	       
			 ;; ----------------------------------------------------------------------------
			 ;; Remote Methods
			 ;; ----------------------------------------------------------------------------
			 ,@(reduce0 (lambda (acc method)
				      (let ((proxy (intern (format nil "~A/JS" (car method)))))
					(cons (make-keyword (car method))
					      (cons (funcall proxy class+ nil) acc ))))
				    (class+.remote-methods class+))
			 
			 ;; ----------------------------------------------------------------------------
			 ;; Local Methods
			 ;; ----------------------------------------------------------------------------
			 ,@(reduce0 (lambda (acc method)
				      (destructuring-bind (method . k-url) method
					(let ((proxy (intern (format nil "~A/JS" (car method)))))
					  (cons (make-keyword (car method))
						(cons (funcall proxy class+ k-url) acc )))))
				    (mapcar #'cons (class+.local-methods class+) k-urls)))))
		   
		   (doeach (i prototype)
		     (setf (slot-value to-extend i) (slot-value prototype i))))
		 
		   (when (not (null properties))
		     (doeach (i properties)
		       (setf (slot-value to-extend i) (slot-value properties i))))
		 
		   (when (typep to-extend.init 'function)
		     (to-extend.init))

		   to-extend)))))
       
       ;; ----------------------------------------------------------------------------
       ;; Component Constructor Renderer
       ;; ----------------------------------------------------------------------------
       (defmethod/cc component! ((stream core-stream) (component ,class-name))
	 (if (and +context+ (context.request +context+))
	     (setf (slot-value component 'url)
		   (format nil "/~A/~A"
			   (web-application.fqdn (context.application +context+))
			   (apply #'concatenate 'string (flatten (uri.paths (http-request.uri (context.request +context+))))))))
	 
	 
	 (let ,(mapcar (lambda (method k-url)
			 (let ((method-args (extract-argument-names (cdddr method)
								    :allow-specializers t)))
			   `(,k-url (action/url ,(reduce0
						  (lambda (acc arg)
						    (cons (list arg (symbol-to-js (symbol-name arg)))
							  acc))
						  method-args)
				      (json/suspend
					(lambda (stream)
					  (json! stream
						 (,(car method) component
						   ,@(mapcar (lambda (arg)
							       `(json-deserialize ,arg))
							     method-args)))))))))
		       (class+.local-methods class+) k-urls)
	   (%component! stream component ,@k-urls))))))

;; ----------------------------------------------------------------------------
;; Default Funkall Method for Components
;; ----------------------------------------------------------------------------
(defmethod/remote funkall ((self component) action arguments)
  (funcall (+ (slot-value self 'url) action) arguments))

(defmethod write-stream :after ((stream html-stream) (object component))
;;   (if (typep object 'xml) (call-next-method))
;; ;;   (break stream)
;;   (write-stream stream
;; 		(<:script :type "text/javascript"
;; 	      (with-call/cc		
;; 		(lambda (stream)
;; 		  (let ((stream (make-indented-stream stream)))
;; 		    (component! stream object)
;; 		    (when (typep object 'xml)
;; 		      (write-stream stream
;; 			(js*
;; 			  `(progn
;; 			     (new
;; 			      (,(class-name (class-of object))
;; 				(create)
;; 				(document.get-element-by-id ,(slot-value object 'id)))))))))))))
  )


;; (defcomponent abc ()
;;   ())

;; (defmethod/local test-1 ((self abc) a b c)
;;   (list a b c))

;; (defmethod/remote test-1 ((self abc) a b c)
;;   (list a b c))

;; (defcomponent def (abc)
;;   ())

;; (defmethod/remote test-1 ((self def) a b c)
;;   (list c b a)
;;   (call-next-method))

;; (defclass a ()
;;   ())

;; (defclass b (a)
;;   ())

;; (defmethod abcdef ((self a) b c)
;;   (list self b c))

;; (defmethod abcdef ((self b) b c)
;;   (if (next-method-p)
;;       (describe 'zoo)
;;       (list c b self)))

;; (defmethod foo123 ((self abc+))
;;   (list 'abc+))

;; (defmethod foo123 ((self def+))
;;   (if (next-method-p)
;;       (call-next-method)
;;       (list 'def+)))

;; ;; ----------------------------------------------------------------------------
;; ;; defmethod/local macro: Defines a local method
;; ;; ----------------------------------------------------------------------------
;; (defmacro defmethod/local (name ((self class-name) &rest args) &body body)
;;   "Defines a local method and corresponding remote (javascript) proxy named 'name!'"
;;   (assert (not (null (find-class+ class-name))) nil
;; 	  "Class+ ~A not found while defining defmethod/local ~A." class-name name)
;;   (with-unique-names (stream k)
;;     (let ((proxy (intern (format nil "~A!" name) (find-package :tr.gen.core.server)))
;; 	  (js-method-name (intern (format nil ".~A" name))))    
;;       `(progn
;; 	 (class+.add-method (find-class+ ',class-name) ',name 'local '((,self ,class-name) ,@args))
;; 	 (defmethod ,proxy ((,stream core-stream) (,self ,class-name) ,k)	   
;; 	   (with-js (,k) ,stream	     
;; 	     (lambda ,args
;; 	       (this.funkall ,k
;; 			     (create
;; 			      ,@(nreverse
;; 				 (reduce0 (lambda (acc arg)
;; 					    (cons arg (cons (make-keyword arg) acc)))
;; 					  (extract-argument-names args :allow-specializers t))))))))
;; 	 (eval-when (:load-toplevel :compile-toplevel :execute)
;; 	   (defjsmacro ,name (&rest args)
;; 	     `(,',js-method-name ,@args)))
;; ;;; 	 (export ',name)
;; 	 (defmethod/cc ,name ((,self ,class-name) ,@args) ,@body)))))

;; ;; ----------------------------------------------------------------------------
;; ;; defmethod/remote macro: Defines a remote method
;; ;; ----------------------------------------------------------------------------
;; (defmacro defmethod/remote (name ((self class-name) &rest args) &body body)
;;   "Defines a remote (javascript) method and corresponding local (lisp) proxy"
;;   (assert (not (null (find-class+ class-name))) nil
;; 	  "Class+ ~A not found while defining defmethod/remote ~A." class-name name)
;;   (with-unique-names (stream hash)
;;     (let ((proxy (intern (format nil "~A!" name) (find-package :tr.gen.core.server)))
;; 	  (js-method-name (intern (format nil ".~A" name))))
;;       `(progn
;; 	 (class+.add-method (find-class+ ',class-name) ',name 'remote '((,self ,class-name) ,@args))	 
;; 	 (defmethod ,proxy ((,stream core-stream) (,self ,class-name))
;; 	   (with-js () ,stream
;; 	     (lambda ,args
;; 	       (let ((,self this))
;; 		 ,@body))))
;; 	 (eval-when (:load-toplevel :compile-toplevel :execute)
;; 	   (defjsmacro ,name (&rest args)
;; 	     `(,',js-method-name ,@args)))
;; ;;; 	 (export ',name)
;; 	 (defmethod/cc ,name ((,self ,class-name) ,@args)
;; 	   (let ((,hash (action/url ((result "result"))
;; 			  (answer (json-deserialize result)))))
;; 	     (javascript/suspend
;; 	      (lambda (,stream)
;; 		(with-js (,hash) ,stream
;; 		  (this.funkall ,hash
;; 		   (create :result (serialize (,name self ,@args)))))))))))))

;; ;; ----------------------------------------------------------------------------
;; ;; defmethod/both macro: Defines a method in both worlds
;; ;; ----------------------------------------------------------------------------
;; (defmacro defmethod/both (name ((self class-name) &rest args) &body body)
;;   (assert (not (null (find-class+ class-name))) nil
;; 	  "Class+ ~A not found while defining defmethod/remote ~A." class-name name)
;;   (with-unique-names (stream)
;;     (let ((proxy (intern (format nil "~A!" name) (find-package :tr.gen.core.server))))
;;       `(progn
;; 	 (class+.add-method (find-class ',class-name) ',name 'both '((,self ,class-name) ,@args))	 
;; 	 (defmethod ,proxy ((,stream core-stream) (,self ,class-name))
;; 	   (with-js () ,stream
;; 	     ,(unwalk-form
;; 	       (fix-javascript-methods
;; 		(walk-js-form `(lambda ,args ,@body))
;; 		self))))
;; 	 (export ',name)
;; 	 (defmethod/cc ,name ((,self ,class-name) ,@args)
;; 	   ,@body)))))

;; ;; +----------------------------------------------------------------------------
;; ;; | Component Metaclass
;; ;; +----------------------------------------------------------------------------
;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (defclass component+ (class+)
;;     ((%ctor-timestamp :initform 0))))

;; ;; +----------------------------------------------------------------------------
;; ;; | Component Class
;; ;; +----------------------------------------------------------------------------
;; (defclass+ component ()
;;   ((url :host remote)
;;    (service-name :host remote :initarg :service-name :initform nil))
;;   (:metaclass component+)
;;   (:documentation "Base component class"))

;; (defmethod shared-initialize :after ((self component) slot-vals &key &allow-other-keys)
;;   (if (and +context+ (context.request +context+))
;;       (setf (slot-value self 'url)
;; 	    (format nil "/~A/~A"
;; 		    (web-application.fqdn (context.application +context+))
;; 		    (apply #'concatenate 'string (flatten (uri.paths (http-request.uri (context.request +context+))))))))

;;   (if (null (s-v 'service-name))
;;       (setf (s-v 'service-name) (symbol-to-js (class-name (class-of self))))))

;; ;; ----------------------------------------------------------------------------
;; ;; defcomponent-accessors Macro: Defines remote and local accessors
;; ;; for slots of a component
;; ;; ----------------------------------------------------------------------------
;; (defmacro defcomponent-accessors (class-name)
;;   (flet ((reader (name) (intern (format nil "GET-~A" name) (symbol-package name)))
;; 	 (writer (name) (intern (format nil "SET-~A" name) (symbol-package name))))
;;     (let ((class+ (find-class+ class-name)))
;;       `(progn
;; 	 ,@(mapcar (lambda (slot)
;; 		     (with-slotdef (name) slot
;; 		       `(progn
;; 			  (defmethod/local ,(reader name) ((self ,class-name))
;; 			    (slot-value self ',name))
;; 			  (defmethod/local ,(writer name) ((self ,class-name) value)
;; 			    (setf (slot-value self ',name) value)))))
;; 		   (filter (lambda (slot) (not (string= (slot-definition-name slot) 'id)))
;; 			   (class+.local-slots class+)))
;; 	 ,@(mapcar (lambda (slot)
;; 		     (with-slotdef (name) slot
;; 		       `(progn
;; 			  (defmethod/remote ,(reader name) ((self ,class-name))
;; 			    (slot-value self ',name))
;; 			  (defmethod/remote ,(writer name) ((self ,class-name) value)
;; 			    (setf (slot-value self ',name) value)))))
;; 		   (class+.remote-slots class+))))))

;; ;; ----------------------------------------------------------------------------
;; ;; defcomponent-ctor Macro: Defines remote constructor for a component
;; ;; ----------------------------------------------------------------------------
;; (defmethod component+.ctor ((component component+))
;;   (let* ((class-name (class-name component))
;; 	 (class+ (find-class+ class-name))
;; 	 (k-urls (mapcar (lambda (m) (declare (ignore m)) (gensym))
;; 			 (class+.local-methods class+)))
;; 	 (remote-slots (mapcar (lambda (slot)
;; 				 (with-slotdef (name reader initarg) slot
;; 				   (list name (intern (symbol-name (gensym)))
;; 					 reader (intern (symbol-name initarg)))))
;; 			       (class+.remote-slots class+)))
;; 	 (prototype `(slot-value ,class-name 'prototype)))
;;     `(progn
;;        ;; ----------------------------------------------------------------------------
;;        ;; Component Internal Render Method 
;;        ;; ----------------------------------------------------------------------------
;;        (defmethod %component! ((stream core-stream) (component ,class-name) &rest k-urls)
;; 	 (with-accessors (,@(mapcar (lambda (slot) (list (cadr slot) (caddr slot)))
;; 				    remote-slots)) component
;; 	   (destructuring-bind (,@k-urls) k-urls
;; 	     (with-js (,@k-urls ,@(mapcar #'cadr remote-slots)) stream
;; 	       ;; ----------------------------------------------------------------------------
;; 	       ;; Constructor
;; 	       ;; ----------------------------------------------------------------------------
;; 	       (defun ,class-name (,@(mapcar #'cadddr remote-slots))
;; 		 ,@(mapcar (lambda (slot)
;; 			     `(if (not (typep ,(cadddr slot) 'undefined))
;; 				  (setf (slot-value this ',(car slot)) ,(cadddr slot))))
;; 			   remote-slots)
;; 		 ;; (stream-escape (:init! component))
;; 		 (when (typep this.init 'function)
;; 		   (this.init))
		 
;; 		 this)
	     
;; 	       ;; ----------------------------------------------------------------------------
;; 	       ;; Prototype
;; 	       ;; ----------------------------------------------------------------------------
;; 	       (setf ,prototype (new (*object))
;; 		     (slot-value ,prototype 'constructor) ,class-name)

;; 	       ;; ----------------------------------------------------------------------------
;; 	       ;; Remote Slot Initial Values
;; 	       ;; ----------------------------------------------------------------------------
;; 	       ,@(mapcar (lambda (slot)
;; 			   `(setf (slot-value ,prototype ',(car slot)) ,(cadr slot)))
;; 			 remote-slots)
;; 	       ;; ----------------------------------------------------------------------------
;; 	       ;; Remote Methods
;; 	       ;; ----------------------------------------------------------------------------
;; 	       ,@(mapcar (lambda (method)
;; 			   (let ((proxy (intern (format nil "~A!" (car method)) :keyword)))
;; 			     `(setf (slot-value ,prototype ',(car method))
;; 				    (stream-escape (,proxy component)))))
;; 			 (class+.remote-methods class+))
	     
;; 	       ;; ----------------------------------------------------------------------------
;; 	       ;; Local Methods
;; 	       ;; ----------------------------------------------------------------------------
;; 	       ,@(mapcar (lambda (method k-url)
;; 			   (let ((proxy (intern (format nil "~A!" (car method)) :keyword)))
;; 			     `(setf (slot-value ,prototype ',(car method))
;; 				    (stream-escape (,proxy component ,k-url)))))
;; 			 (class+.local-methods class+) k-urls)))))
       
;;        ;; ----------------------------------------------------------------------------
;;        ;; Component Constructor Renderer
;;        ;; ----------------------------------------------------------------------------
;;        (defmethod/cc component! ((stream core-stream) (component ,class-name))
;; 	 (let ,(mapcar (lambda (method k-url)
;; 			 (let ((method-args (extract-argument-names (cdddr method)
;; 								    :allow-specializers t)))
;; 			   `(,k-url (action/url ,(reduce0
;; 						  (lambda (acc arg)
;; 						    (cons (list arg (symbol-to-js (symbol-name arg)))
;; 							  acc))
;; 						  method-args)
;; 				      (json/suspend
;; 					(lambda (stream)
;; 					  (json! stream
;; 						 (,(car method) component
;; 						   ,@(mapcar (lambda (arg)
;; 							       `(json-deserialize ,arg))
;; 							     method-args)))))))))
;; 		       (class+.local-methods class+) k-urls)
;; 	   (%component! stream component ,@k-urls))))))

;; ;; ----------------------------------------------------------------------------
;; ;; This around method allows us to compile constructor evertime class
;; ;; definition changed.
;; ;; ----------------------------------------------------------------------------
;; (defmethod/cc component! ((stream core-stream) (component component))
;;   (error "This component! method should not be called."))

;; (defmethod/cc component! :around ((stream core-stream) (component component))
;;   (if  (> (slot-value (class-of component) '%timestamp)
;; 	  (slot-value (class-of component) '%ctor-timestamp))      
;;        (let ((name (class-name (class-of component))))
;; 	 (format *standard-output* "Compiling constructor for ~A.~%" name)
;; 	 (setf (slot-value (class-of component) '%ctor-timestamp)
;; 	       (get-universal-time))
;; 	 (eval (component+.ctor (class-of component)))
;; 	 (component! stream component))
;;        (call-next-method stream component)))

;; ;; +----------------------------------------------------------------------------
;; ;; | defcomponent Macro: Defines a new component
;; ;; +----------------------------------------------------------------------------
;; (defmacro defcomponent (name supers slots &rest rest)
;;   `(progn     
;;      (defclass+ ,name (,@supers component)
;;        ,slots
;;        ,@rest
;;        (:metaclass component+))
;;      (defcomponent-accessors ,name)
;;      (find-class+ ',name)))

;; ;; (defmacalias defcomponent defhtml-component)

;; ;;-----------------------------------------------------------------------------
;; ;; Component Protocol
;; ;;-----------------------------------------------------------------------------
;; (defmethod component.application ((self component))
;;   "Returns application associated with this component."
;;   (context.application +context+))

;; (defmethod/remote to-json ((self component) object)
;;   (to-json object))

;; (defmethod/remote funkall ((self component) action arguments)
;;   (funcall (+ (slot-value self 'url) action) arguments))

;; ;; +----------------------------------------------------------------------------
;; ;; | HTML Component
;; ;; +----------------------------------------------------------------------------
;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (defclass html-component+ (component+)
;;     ()))

;; (defclass+ html-component (component)
;;   ((id :reader :get-id :initform (symbol-to-js (gensym "COMPONENT-"))))
;;   (:metaclass html-component+))

;; (defmethod dom-element! ((stream core-stream) (self html-component)
;; 			 &optional (indentation 0))
;;   (call-next-method)
;;   (dom-element! stream
;; 		(<:script :type "text/javascript"
;; 			  (lambda (stream)
;; 			    (with-call/cc
;; 			      (html-component! stream self))))
;; 		indentation))

;; (defmethod/cc html-component! ((stream core-stream) (component html-component))
;;   (error "This html-component! method should not be called."))

;; (defmethod/cc html-component! :around ((stream core-stream) (component html-component))
;;   (if  (> (slot-value (class-of component) '%timestamp)
;; 	  (slot-value (class-of component) '%ctor-timestamp))      
;;        (let ((name (class-name (class-of component))))
;; 	 (format *standard-output* "Compiling constructor for ~A.~%" name)
;; 	 (setf (slot-value (class-of component) '%ctor-timestamp)
;; 	       (get-universal-time))
;; 	 (eval (html-component+.ctor (class-of component)))
;; 	 (html-component! stream component))
;;        (call-next-method stream component)))

;; (defmethod/cc component! ((stream core-stream) (component html-component))
;;   (call-next-method))

;; ;; ;; -----------------------------------------------------------------------------
;; ;; ;; Component Tag Definition
;; ;; ;; -----------------------------------------------------------------------------
;; (defmacro defhtml-component-tag (name)  
;;   (let* ((class+ (find-class+ name))
;; 	 (dom-class (any (lambda (class) (if (typep class 'dom-element+) class))
;; 			 (class+.superclasses class+)))
;; 	 (dom-attributes (if dom-class (dom-element+.attributes dom-class) nil)))
;;     `(defun/cc ,name (&rest args)
;;        (multiple-value-bind (attributes children) (tag-attributes args)
;; 	 (destructuring-bind (&key ,@(union dom-attributes
;; 					    (class+.all-ctor-lambda-list class+)
;; 					    :key #'(lambda (a) (if (listp a) (car a) a)))) attributes	     
;; 	   (make-instance ',name
;; 			  ,@(class+.all-ctor-arguments class+)			    
;; 			  :attributes (filter (lambda (attr)
;; 						(not (null (cdr attr))))
;; 					      (list ,@(mapcar (lambda (attr)
;; 								`(cons ,(symbol-to-js attr) ,attr))
;; 							      dom-attributes)))
;; 			  :children (flatten children)))))))

;; (defmacro defhtml-component (name supers slots &rest rest)
;;   (let ((html-component? (reduce0 (lambda (acc class)
;; 				    (or acc
;; 					(member (find-class+ 'html-component)
;; 						(class+.superclasses (find-class+ class)))))
;; 				  supers))
;; 	(supers (uniq (append supers (list '<:div)))))
;;     `(progn
;;        (eval-when (:load-toplevel :compile-toplevel :execute)
;; 	 (defclass ,name (,@(if html-component?
;; 				supers
;; 				(cons 'html-component supers)))
;; 	   ,(mapcar (lambda (slot) (%fix-slot-definition name slot)) slots)
;; 	   ,@(%filter-rest (cons '(:metaclass html-component+) rest))))
;; ;;;      (export ',name)
;;        (defhtml-component-tag ,name)
;;        (defcomponent-accessors ,name)
;;        (find-class+ ',name))))

;; (defmethod html-component+.ctor ((component component+))
;;   (let* ((class-name (class-name component))
;; 	 (class+ (find-class+ class-name))
;; 	 (k-urls (mapcar (lambda (m) (declare (ignore m)) (gensym))
;; 			 (class+.local-methods class+)))
;; 	 (remote-slots (mapcar (lambda (slot)
;; 				 (with-slotdef (name reader initarg) slot
;; 				   (list name (intern (symbol-name (gensym)))
;; 					 reader (intern (symbol-name initarg)))))
;; 			       (class+.remote-slots class+)))
;; 	 (id (intern (symbol-name (gensym))))
;; 	 (prototype `($ ,id)))    
;;     `(progn
;;        ;; ----------------------------------------------------------------------------
;;        ;; Component Internal Render Method 
;;        ;; ----------------------------------------------------------------------------
;;        (defmethod %html-component! ((stream core-stream) (component ,class-name) &rest k-urls)
;; 	 (let ((stream (make-indented-stream stream)))
;; 	   (destructuring-bind (,@k-urls) k-urls
;; 	     (with-accessors (,@(mapcar (lambda (slot) (list (cadr slot) (caddr slot)))
;; 					remote-slots)) component
;; 	       (let ((,id (get-id component)))
;; 		 (with-js (,@(mapcar #'cadr remote-slots) ,@k-urls ,id) stream
;; 		   ;; ----------------------------------------------------------------------------
;; 		   ;; Remote Slot Initial Values
;; 		   ;; ----------------------------------------------------------------------------
;; 		   ,@(mapcar (lambda (slot)
;; 			       `(setf (slot-value ,prototype ',(car slot)) ,(cadr slot)))
;; 			     remote-slots)

	       
;; 		   ;; ----------------------------------------------------------------------------
;; 		   ;; Remote Methods
;; 		   ;; ----------------------------------------------------------------------------
;; 		   ,@(mapcar (lambda (method)
;; 			       (let ((proxy (intern (format nil "~A!" (car method)) :keyword)))
;; 				 `(setf (slot-value ,prototype ',(car method))
;; 					(stream-escape (,proxy component)))))
;; 			     (class+.remote-methods class+))
       
;; 		   ;; ----------------------------------------------------------------------------
;; 		   ;; Local Methods
;; 		   ;; ----------------------------------------------------------------------------
;; 		   ,@(mapcar (lambda (method k-url)
;; 			       (let ((proxy (intern (format nil "~A!" (car method)) :keyword)))
;; 				 `(setf (slot-value ,prototype ',(car method))
;; 					(stream-escape (,proxy component ,k-url)))))
;; 			     (class+.local-methods class+) k-urls)
		 
;; 		   (when (typep (slot-value ,prototype 'init) 'function)
;; 		     ;; (.remove (j-query (+ "#" ,id " > script")))
;; 		     (.init ,prototype))))))))
       
;;        ;; ----------------------------------------------------------------------------
;;        ;; Component Constructor Renderer
;;        ;; ----------------------------------------------------------------------------
;;        (defmethod/cc html-component! ((stream core-stream) (component ,class-name))
;; 	 (let ,(mapcar (lambda (method k-url)
;; 			 (let ((method-args (extract-argument-names (cdddr method)
;; 								    :allow-specializers t)))
;; 			   `(,k-url (action/url ,(reduce0
;; 						  (lambda (acc arg)
;; 						    (cons (list arg (symbol-to-js (symbol-name arg)))
;; 							  acc))
;; 						  method-args)
;; 				      (let ((result (,(car method) component
;; 						      ,@(mapcar (lambda (arg)
;; 								  `(json-deserialize ,arg))
;; 								method-args))))
;; 					(json/suspend
;; 					  (lambda (stream)
;; 					    (json! stream result))))))))
;; 		       (class+.local-methods class+) k-urls)
;; 	   (%html-component! stream component ,@k-urls))))))

;; (defmethod component+.ctor ((component html-component+))
;;   (let* ((class-name (class-name component))
;; 	 (class+ (find-class+ class-name))
;; 	 (k-urls (mapcar (lambda (m) (declare (ignore m)) (gensym))
;; 			 (class+.local-methods class+)))
;; 	 (remote-slots (mapcar (lambda (slot)
;; 				 (with-slotdef (name reader initarg) slot
;; 				   (list name (intern (symbol-name (gensym)))
;; 					 reader (intern (symbol-name initarg)))))
;; 			       (class+.remote-slots class+)))
;; 	 (dom-class (any (lambda (class) (if (typep class 'dom-element+) class))
;; 			 (class+.superclasses class+)))
;; 	 (dom-attributes (if dom-class (dom-element+.attributes dom-class) nil))
;; 	 (prototype `(slot-value ,class-name 'prototype)))
;;     `(progn
;;        ;; ----------------------------------------------------------------------------
;;        ;; Component Internal Render Method 
;;        ;; ----------------------------------------------------------------------------
;;        (defmethod %component! ((stream core-stream) (component ,class-name) &rest k-urls)
;; 	 (with-accessors (,@(mapcar (lambda (slot) (list (cadr slot) (caddr slot)))
;; 				    remote-slots)) component
;; 	   (destructuring-bind (,@k-urls) k-urls
;; 	     (with-js (,@k-urls ,@(mapcar #'cadr remote-slots)) stream
;; 	       ;; ----------------------------------------------------------------------------
;; 	       ;; Constructor
;; 	       ;; ----------------------------------------------------------------------------
;; 	       (defun ,class-name (properties to-extend)
;; 		 (if (null to-extend)
;; 		     (setf to-extend (document.create-element ,(symbol-to-js (class-name dom-class)))))

;; 		 (doeach (i ,prototype)
;; 		   (setf (slot-value to-extend i) (slot-value this i)))

;; 		 (when (not (null properties))
;; 		   (doeach (i properties)
;; 		     (setf (slot-value to-extend i) (slot-value properties i))))

;; 		 (when (typep to-extend.init 'function)
;; 		   (to-extend.init))

;; 		 to-extend)

;; 	       ;; ----------------------------------------------------------------------------
;; 	       ;; Remote Slot Initial Values
;; 	       ;; ----------------------------------------------------------------------------
;; 	       ,@(mapcar (lambda (slot)
;; 			   `(setf (slot-value ,prototype ',(car slot)) ,(cadr slot)))
;; 			 remote-slots)
;; 	       ;; ----------------------------------------------------------------------------
;; 	       ;; Remote Methods
;; 	       ;; ----------------------------------------------------------------------------
;; 	       ,@(mapcar (lambda (method)
;; 			   (let ((proxy (intern (format nil "~A!" (car method)) :keyword)))
;; 			     `(setf (slot-value ,prototype ',(car method))
;; 				    (stream-escape (,proxy component)))))
;; 			 (class+.remote-methods class+))
	     
;; 	       ;; ----------------------------------------------------------------------------
;; 	       ;; Local Methods
;; 	       ;; ----------------------------------------------------------------------------
;; 	       ,@(mapcar (lambda (method k-url)
;; 			   (let ((proxy (intern (format nil "~A!" (car method)) :keyword)))
;; 			     `(setf (slot-value ,prototype ',(car method))
;; 				    (stream-escape (,proxy component ,k-url)))))
;; 			 (class+.local-methods class+) k-urls)))))
       
;;        ;; ----------------------------------------------------------------------------
;;        ;; Component Constructor Renderer
;;        ;; ----------------------------------------------------------------------------
;;        (defmethod/cc component! ((stream core-stream) (component ,class-name))
;; 	 (let ,(mapcar (lambda (method k-url)
;; 			 (let ((method-args (extract-argument-names (cdddr method)
;; 								    :allow-specializers t)))
;; 			   `(,k-url (action/url ,(reduce0
;; 						  (lambda (acc arg)
;; 						    (cons (list arg (symbol-to-js (symbol-name arg)))
;; 							  acc))
;; 						  method-args)
;; 				      (json/suspend
;; 					(lambda (stream)
;; 					  (json! stream
;; 						 (,(car method) component
;; 						   ,@(mapcar (lambda (arg)
;; 							       `(json-deserialize ,arg))
;; 							     method-args)))))))))
;; 		       (class+.local-methods class+) k-urls)
;; 	   (%component! stream component ,@k-urls))))))

;; (defmethod component+.ctor ((component html-component+))
;;   (call-next-method))

;; (defhtml-component test7 (<:div)
;;   ((slotA :host local)))

;; (defcomponent test-component1 ()
;;   ())

;; (defmethod/local test-local/1 ((self test-component1) a b c)
;;   (list a b c))

;; (defmethod/remote test-remote/1 ((self test-component1) a b c)
;;   (list a b c))

;; (defcomponent test-component2 ()
;;   ((local-slot1 :host local)
;;    (remote-slot1 :host remote)))

;; (defmethod/local test-local/1 ((self test-component2) a b c)
;;   (list b c a))

;; (defmethod/remote test-local/2 ((self test-component2) a b c)
;;   (list b c a))

;; ;; (defctor test-component2
;; ;;   (alert "zoo"))

;; ;; ----------------------------------------------------------------------------
;; ;; HTML Output
;; ;; ----------------------------------------------------------------------------
;; (defmethod dom-element! ((stream core-stream) (self component)
;; 			            &optional (indentation 0))
;;   (if (typep self 'html-component)
;;       (call-next-method)
;;       (dom-element! stream
;; 		    (<:script :type "text/javascript"
;; 			      (lambda (stream)
;; 				(with-call/cc
;; 				  (component! stream self))))
;; 		    indentation)))

;; ----------------------------------------------------------------------------
;; Debug Component
;; ----------------------------------------------------------------------------
;; FIXME: Fix debug component
;; (defcomponent debug-component ()
;;   ())

;; (defmethod/local get-source-code ((self debug-component) function)
;;   (funcall (function (intern o)))) => "function () { return 1; }" ;

;; (defmethod/local set-result ((self debug-component) function parameters result)
;;   (setf (gethash function +function-table+) (list (cons funciton parameters) result)))

;; (defmethod/remote run-test ((self debug-component) function)
;;   (let ((fun (this.get-source-code(function))))
;;     (if fun
;; 	(this.set-result function nil (funcall fun)))))

;; (defun/javascript denemeA (str num) ("aycan" 1) ()
;;   (denemeB str (incf num)))

;; (defun/javascript denemeB (str num)
;;   (list str num))

;; record tanimla
;; record query, update
;; IPC

;; ----------------------------------------------------------------------------
;; HTML Component
;; ----------------------------------------------------------------------------
;; (defcomponent html-element (dom-element)
;;   ())

;; (defmethod/local render ((self html-element))
;;   nil)
;; (defctor (self html-component)
;;   (self.append-child (self.template)))

;; (defmethod/both template ((self html-component))
;;   (<:script :type "text/javascript"
;; 	    (lambda (stream)	      
;; 	      (ctor! stream self)))) ;; Default HTML

;; (defmethod shared-initialize :after ((self html-component) slot-names
;; 				     &key &allow-other-keys)
;;   (declare (ignore slot-names))
;;  ;;  (setf (dom.children self)
;; ;; 	(cons (<:script :type "text/javascript"
;; ;; 			;; :src (with-call/cc 
;; ;; ;; 			       (action/url ()
;; ;; ;; 				 (javascript/suspend
;; ;; ;; 				  (lambda (stream)
;; ;; ;; 				    (ctor! stream self)))))
;; ;; 			(let ((c (with-call/cc
;; ;; 				   (lambda (stream)
;; ;; 				     (ctor! (make-indented-stream stream) self)))))
;; ;; 			  (describe (list 'zoo c))
;; ;; 			  c)
;; ;; 			)
;; ;; 	      ;; (ensure-list (with-call/cc (template self))) 
;; ;; 		(dom.children self)))
;;   self)

;; ;;; (with-yaclml-output-to-string
;; ;;;     (<:div :id "56" :style "background-color:#FFF; color:#000"
;; ;;; 	   (<:p "This is render of html-element")
;; ;;; 	   (<:p "Aytek maraba!")))

;; (defmethod/cc send/ctor ((self html-element) remote-slots local-methods remote-methods)
;;   ;; (<:js
;; ;;    `(defun ,(class-name (class-of self)) ()
;; ;; 	(let ((o (create ,@remote-slots ,@local-methods ,@remote-methods))
;; ;; 	      (p (document.create-element o.tag)))
;; ;; 	  (doeach (property o)
;; ;; 	    (setf (slot-value p property) (slot-value o property)))
	  
;; ;; 	  (if (= "function" (typeof o.render))		
;; ;; 	      (setf p.inner-h-t-m-l (o.render)))

;; ;; 	  (setf this.prototype p)
;; ;; 	  (return p))))
;;   (error "fixme"))

;; (defcomponent div-element (html-element)
;;   ()
;;   (:default-initargs :tag "div"))

;; (defmethod/local render ((self div-element))
;;   (with-html-output (http-response.stream (response +context+))
;;     (<:div "hobaaa")))



;;------------------------------------------------------------------------------
;; Core Component
;;------------------------------------------------------------------------------
;; (defcomponent <core:core ()
;;   ((services :initform nil :host remote)
;;    (user :initform (query-session 'user) :host none)))

;; (defmethod/remote find-service ((self <core:core) name)
;;   (car (filter (lambda (s)
;; 		 (equal name (slot-value s 'service-name)))
;; 	       (slot-value self 'services))))

;; (defmethod/remote register-service ((self <core:core) fun)
;;   (setf (slot-value self 'services)
;; 	(cons fun (slot-value self 'services))))

;; (defmethod/local is-authenticated ((self <core:core))
;;   (and (s-v 'user) t))

;; (defmethod/local get-user ((self <core:core))
;;   (object->jobject (s-v 'user)))


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
