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
	(proxy (intern (format nil "~A/JS" name) (symbol-package name)))
	(js-method-name (intern (format nil ".~A" name) (symbol-package :name))))
    `(progn
       (class+.add-method (find-class+ ',class-name) ',name 'local '((,self ,class-name) ,@args))
       (eval-when (:load-toplevel :compile-toplevel :execute)
	 (setf (gethash ',name +javascript-cps-functions+) t
	       (gethash ',js-method-name +javascript-cps-functions+) t)
	 (defjsmacro ,name (&rest args) `(,',js-method-name ,@args)))
       (defmethod ,proxy ((self ,metaclass) k)
	 `(method ,',args	    
	    (funkall self ,k
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
	(proxy (intern (format nil "~A/JS" name) (symbol-package name)))
	(js-method-name (intern (format nil ".~A" name) (symbol-package name)))
	(call-next-method-p (any (lambda (application-form)
				   (eq 'call-next-method (operator application-form)))
				 (ast-search-type (walk-form `(lambda () ,@body))
						  (list 'application-form)))))
    `(progn
       (class+.add-method (find-class+ ',class-name) ',name 'remote '((,self ,class-name) ,@args))
       (eval-when (:load-toplevel :compile-toplevel :execute)
	 (setf (gethash ',name +javascript-cps-functions+) t
	       (gethash ',js-method-name +javascript-cps-functions+) t)
	 (defjsmacro ,name (&rest args) `(,',js-method-name ,@args)))
       (defmethod ,proxy ((self ,metaclass) k)
	 (declare (ignore k))
	 ,(if call-next-method-p
	      ``(method ,',args
		  (let ((,',self self))
		    (flet ((call-next-method () ,@(cddr (call-next-method))))
		      ,@',body)))
	      ``(method ,',args
		  (let ((,',self self))
		    ,@',body))))
       (defmethod/cc ,name ((,self ,class-name) ,@args)
	 (with-query ((hash "__hash")) (context.request +context+)
	   (let ((hash (json-deserialize (if (listp hash) (car hash) hash))))
	     (javascript/suspend
	      (lambda (stream)
		(let ((result (action/url ((result "result"))
				(answer (json-deserialize result)))))		  
		  (aif (and hash (intern (string-upcase hash)))
		       (with-js (result it ,@args) stream
			 (setf it
			       (with-call/cc
				 (lambda (self)
				   (funkall self result
					    (create :result (serialize (,name self ,@args))))))))
		       (with-js (result ,@args) stream
			 (with-call/cc
			   (lambda (self)
			     (funkall self result
				      (create :result (serialize (,name self ,@args)))))))))))))))))

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
		  (filter (lambda (slot)
			    (or (eq (cdr slot) 'remote)
				(eq (cdr slot) 'both)
				(eq (cdr slot) 'none)))
			  slots)))))

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
	       (with-call/cc
		 (lambda (properties to-extend)
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
					(let ((proxy (intern (format nil "~A/JS" (car method))
							     (symbol-package (car method)))))
					  (cons (make-keyword (car method))
						(cons (funcall proxy class+ nil) acc ))))
				      (class+.remote-methods class+))
			 
			   ;; ----------------------------------------------------------------------------
			   ;; Local Methods
			   ;; ----------------------------------------------------------------------------
			   ,@(reduce0 (lambda (acc method)
					(destructuring-bind (method . k-url) method
					  (let ((proxy (intern (format nil "~A/JS" (car method))
							       (symbol-package (car method)))))
					    (cons (make-keyword (car method))
						  (cons (funcall proxy class+ k-url) acc )))))
				      (mapcar #'cons (class+.local-methods class+) k-urls)))))

		     (extend prototype to-extend))

		   (when (typep properties 'object)
		     (extend properties to-extend))

		   (when (typep to-extend.init 'function)
		     (init to-extend))
		 
		   to-extend))))))
       
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
			   `(,k-url (action/url (,@(reduce0
						    (lambda (acc arg)
						      (cons (list arg (symbol-to-js arg))
							    acc))
						    method-args))
				      (let ((result (,(car method) component
						      ,@(mapcar (lambda (arg)
								  `(json-deserialize ,arg))
								method-args))))
					(with-query ((hash "__hash")) (context.request +context+)
					  (let ((stream (http-response.stream (context.response +context+)))
						(hash (json-deserialize (if (listp hash) (car hash) hash))))
					    (javascript/suspend
					     (lambda (stream)
					       (aif (and hash (intern (string-upcase hash)))
						    (with-js (result it) stream
						      (setf it (with-call/cc (lambda (self) result))))
						    (with-js (result) stream
						      (with-call/cc
							(lambda (self) result))))))
					    nil)))))))
		       (class+.local-methods class+) k-urls)
	   (%component! stream component ,@k-urls))))))

;; ----------------------------------------------------------------------------
;; Default Funkall Method for Components
;; ----------------------------------------------------------------------------
(defun/cc retval (a) a)
(defmethod/remote funkall ((self component) action args)
  (let ((retval (funcall-cc (+ (slot-value self 'url) action "$") args)))
    (let/cc k
      (if (typep retval 'function)
	  (retval self)
	  retval))))

(defmethod/remote upgrade ((self component) new-version)
  (new (new-version (create) self)))

(defmethod write-stream ((stream core-stream) (object component))
  (prog1 stream
    (with-call/cc
      (component! stream object))))

(defun find-component (name)
  (find (string-upcase name)
	(class-subclasses (find-class 'component))
	:test #'string=
	:key #'class-name))

;; +----------------------------------------------------------------------------
;; | Component Dispatcher
;; +----------------------------------------------------------------------------
(defhandler "component.*" ((application http-application) (component "component")
			   (hash "__hash"))
  (javascript/suspend
   (lambda (stream)
     (when (and (stringp hash) (> (length hash) 0))
       (string! stream "var ")
       (string! stream (json-deserialize hash))
       (string! stream " = "))
     (acond
      ((and (typep component 'string) (find-component (json-deserialize component)))
       (component! stream (make-instance it)))
      (t
       (with-js () stream
	 (lambda ()
	   (throw (new (*error "No Components Found - Core Server [http://labs.core.gen.tr]"))))))))))

;; +----------------------------------------------------------------------------
;; | Service Dispatcher
;; +----------------------------------------------------------------------------
(defhandler "service.*" ((application http-application) (component "service")
			 (hash "__hash"))
  (javascript/suspend
   (lambda (stream)
     (when (and (stringp hash) (> (length hash) 0))
       (string! stream "var ")
       (string! stream (json-deserialize hash))
       (string! stream " = "))
     (acond
       ((and (typep component 'string) (find-component (json-deserialize component)))
	(let ((component (query-session it)))
	  (if component
	      (component! stream component)
	      (component! stream (update-session it (make-instance it))))))
       (t
	(with-js () stream
	  (lambda ()
	    (throw (new (*error "No Services Found - Core Server [http://labs.core.gen.tr]"))))))))))



;; (defmethod write-stream ((stream html-stream) (object component))
;;     (call-next-method)
;;   ;; (if (typep object 'xml) (call-next-method))
;; ;;   (write-stream stream
;; ;; 		(<:script :type "text/javascript"
;; ;;                           (with-call/cc		
;; ;;                             (lambda (stream)
;; ;;                               (let ((stream (make-indented-stream stream)))
;; ;;                                 (component! stream object)
;; ;;                                 (when (typep object 'xml)
;; ;;                                   (write-stream stream
;; ;;                                                 (js*
;; ;;                                                   `(progn
;; ;;                                                      (new
;; ;;                                                       (,(class-name (class-of object))
;; ;;                                                         (create)
;; ;;                                                         (document.get-element-by-id ,(slot-value object 'id)))))))))))))
;;   )
