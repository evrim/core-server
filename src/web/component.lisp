;; +----------------------------------------------------------------------------
;; | Component Framework (Lisp->Browser Functor)
;; +----------------------------------------------------------------------------
(in-package :core-server)

;; +----------------------------------------------------------------------------
;; | Component Metaclass
;; +----------------------------------------------------------------------------
(defclass+ component+ (class+)
  ((%ctor-timestamp :initform 0)))

;; +----------------------------------------------------------------------------
;; | Component Class
;; +----------------------------------------------------------------------------
(defclass+ component ()
  ((url :host remote :documentation "The url that this component is served.")
   (instance-id :host none :initform (make-unique-random-string 8)))
  (:metaclass component+)
  (:documentation "Base component class"))

(defmethod component.application ((self component))
  "Returns application associated with this component."
  (context.application +context+))

(defmethod component.serialize ((self component) object)
  object)

(defmethod component.serialize ((self component) (object component))
  object)

(defmethod component.serialize ((self component) (object class+-instance))
  (let ((id (or (gethash object (component.object-cache self))
		(setf (gethash object (component.object-cache self))
		      (random-string 5)))))    
    (let ((object (object->jobject object)))
      (setf (slot-value object 'attributes)
	    (cons :instance-id (cons id (slot-value object 'attributes))))
      object)))

(defmethod component.deserialize ((self component) object)
  (json-deserialize object))

(defmethod component.serialize-slot ((self component) slot-name)
  (let ((slot (class+.find-slot (class-of self) slot-name)))
    (with-slotdef (reader) slot
      (funcall reader self))))

(defmethod component.action-hash ((self component) method)
  (format nil "~A-act~A" method (component.instance-id self)))

(eval-when (:compile-toplevel :load-toplevel)
  (defmethod component+.morphism-function-name ((self component+) name)
    (if (eq (find-package :common-lisp) (symbol-package name))
	(intern (format nil "~A/JS" name) (find-package :core-server))
	(intern (format nil "~A/JS" name) (symbol-package name))))

  (defmethod component+.codomain-function-name ((self component+) name)
    (if (eq (find-package :common-lisp) (symbol-package name))
	(intern (format nil ".~A" name) (find-package :core-server))
	(intern (format nil ".~A" name) (symbol-package name)))))

;; +----------------------------------------------------------------------------
;; | defmethod/local macro: Defines a local method
;; +----------------------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel)
  (defmethod component+.local-morphism ((class component+) name self args body)
    (let* ((class-name (class-name class))
	   (metaclass (class-name (class-of class)))
	   (morphism (component+.morphism-function-name class name))
	   (remote (component+.codomain-function-name class name)))
      `(progn
	 (class+.add-method (find-class+ ',class-name) ',name 'local '((,self ,class-name) ,@args))
	 (eval-when (:load-toplevel :compile-toplevel :execute)
	   (setf (gethash ',name +javascript-cps-functions+) t
		 (gethash ',remote +javascript-cps-functions+) t)
	   (defjsmacro ,name (&rest args) `(,',remote ,@args)))
	 (defmethod ,morphism ((self ,metaclass) k)
	   `(method ,',args	    
		    (funkall self ,k
			     (create
			      ,@',(nreverse
				   (reduce0 (lambda (acc arg)
					      (cons arg (cons (make-keyword arg) acc)))
					    (extract-argument-names args :allow-specializers t)))))))
	 (defmethod/cc ,name ((,self ,class-name) ,@args)
	   (let ((application (component.application ,self)))
	     ,@body))))))

(defmacro defmethod/local (name ((self class-name) &rest args) &body body)
  (component+.local-morphism (find-class class-name) name self args body))

;; +----------------------------------------------------------------------------
;; | defmethod/remote macro: Defines a remote method
;; +----------------------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod component+.remote-morphism ((class component+) name self args body)
    (let* ((class-name (class-name class))
	   (metaclass (class-name (class-of class)))
	   (functor (component+.morphism-function-name class name))
	   (remote (component+.codomain-function-name class name))
	   (call-next-method-p (any (lambda (application-form)
				      (eq 'call-next-method (operator application-form)))
				    (ast-search-type (walk-form `(lambda () ,@body))
						     (list 'application-form)))))
      `(progn
	 (class+.add-method (find-class+ ',class-name) ',name 'remote '((,self ,class-name) ,@args))
	 (eval-when (:load-toplevel :compile-toplevel :execute)
	   (setf (gethash ',name +javascript-cps-functions+) t
		 (gethash ',remote +javascript-cps-functions+) t)
	   (defjsmacro ,name (&rest args) `(,',remote ,@args)))
	 (defmethod ,functor ((self ,metaclass) k)
	   (declare (ignore k))
	   ,(if call-next-method-p
		``(method ,',args
			  (let ((call-next-method
				 (lambda (,',self ,@',args)
				   ,@(cddr (call-next-method self k)))))		    
			    ,@',body))
		``(method ,',args
			  (let ((,',self self))
			    ,@',body))))
	 (defmethod/cc ,name ((,self ,class-name) ,@args)
	   (let (,@(mapcar (lambda (arg) `(,arg (component.serialize ,self ,arg)))
			   (extract-argument-names args :allow-specializers t)))
	     (with-query ((hash "__hash")) (context.request +context+)
	       (let ((hash (json-deserialize (if (listp hash) (car hash) hash))))
		 (javascript/suspend
		  (lambda (stream)
		    (let ((result (action/url ((result "result"))
				    (answer (component.deserialize ,self result)))))		  
		      (if hash
			  (with-js (result hash ,@args) stream
			    (with-call/cc
			      (apply (slot-value window hash) window
				     (list (lambda (self)
					     (funkall self result
						      (create :result (,name self ,@args))))))))
			  (with-js (result ,@args) stream
			    (with-call/cc
			      (lambda (self)
				(funkall self result
					 (create :result (,name self ,@args))))))))))))))))))

(defmacro defmethod/remote (name ((self class-name) &rest args) &body body)
  (component+.remote-morphism (find-class class-name) name self args body))

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
			    (accessor (cadr slot))
			    (host (cddr slot)))			
			(append acc
				`((,(method-type host) ,(reader name) ((self ,class-name))
				    (slot-value self ',name))
				  (,(method-type host) ,(writer name) ((self ,class-name) value)
				    (setf (slot-value self ',name) value))
				  (eval-when (:compile-toplevel :execute :load-toplevel)
				    (defmacro/js ,accessor (self)
				      `(,',(reader name) ,self))
				    (defsetf/js ,accessor (value self)
				      `(,',(writer name) ,self ,value))))))))
		  (filter (lambda (slot)
			    (or (eq (cddr slot) 'remote)
				(eq (cddr slot) 'both)
				(eq (cddr slot) 'local)))
			  slots)))))

;; +----------------------------------------------------------------------------
;; | defcomponent Macro: Defines a new component
;; +----------------------------------------------------------------------------
(defmacro defcomponent (name supers slots &rest rest)
  (let* ((metaclass1 (cadr (assoc :metaclass rest)))
	 (metaclass (intern (format nil "~A+" name)))
	 (metasupers (remove 'class+
		       (uniq
			(append
			 (mapcar (compose #'class-name #'class-of #'find-class)
				 supers)
			 (list 'component+)))))
	 (dom-classes (filter (lambda (a) (typep a 'xml+))
			      (mapcar #'find-class supers)))
	 (tag (any (lambda (a) (if (not (null a)) a))
		   (mapcar #'xml+.tag dom-classes)))
	 (namespace (any (lambda (a) (if (not (null a)) a))
			 (mapcar #'xml+.namespace dom-classes)))
	 (attributes (any (lambda (a) (if (not (null a)) a))
			  (mapcar #'xml+.attributes dom-classes)))
	 (supers (if (member 'component supers)
		     supers
		     (reverse (cons 'component (reverse supers))))))
    `(progn
       ,(when (null metaclass1)
	 `(defclass+ ,metaclass ,metasupers
	    ()
	    (:default-initargs
	      :tag ,@(if tag (ensure-list tag) '(nil))
	      :namespace ,@(if namespace (ensure-list namespace) '(nil))
	      :attributes ',attributes)))
       (defclass+ ,name (,@supers)
	 ,(mapcar (lambda (a) ;; fixing accessors we do not use abc.def.
		    (let ((gee (copy-list a)))
		      (if (null (getf (cdr gee) :accessor))
			  (append a `(:accessor ,(car a)))
			  a)))
		  slots)
	 ,@rest
	 (:metaclass ,(or metaclass1 metaclass)))
       ;; (defjsmacro ,name (&rest properties)
       ;; 	 `(make-component ,',(symbol-to-js name) (jobject ,@properties) nil))
       (defcomponent-accessors ,name ,(mapcar
				       (lambda (slot)
					 (cons (car slot)
					       (cons (or (getf (cdr slot) :accessor)
							 (car slot))
						     (or (getf (cdr slot) :host)
							 'local))))
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
	 (dom-tag (any #'xml+.tag
		       (reverse
			(filter (lambda (class)
				  (if (typep class 'xml+) class))
				(cdr (class+.superclasses class+)))))))
    `(progn
       ;; ----------------------------------------------------------------------------
       ;; Component Internal Render Method 
       ;; ----------------------------------------------------------------------------
       (defmethod %component! ((stream core-stream) (component ,class-name) &rest k-urls)	 
	 (let (,@(mapcar (lambda (slot)
			   `(,(cadr slot) (component.serialize-slot component ',(car slot))))
			 remote-slots))
	   (destructuring-bind (,@k-urls) k-urls
	     (with-js (,@k-urls ,@(mapcar #'cadr remote-slots)) stream
	       ;; ----------------------------------------------------------------------------
	       ;; Constructor
	       ;; ----------------------------------------------------------------------------
	       (with-call/cc
		 (lambda (to-extend)
		   (let ((to-extend (or to-extend (new (*object))))
			 (slots
			  (jobject
			   ;; ----------------------------------------------------------------------------
			   ;; Remote Slot Initial Values
			   ;; ----------------------------------------------------------------------------
			   ,@(reduce0 (lambda (acc slot)
					(cond
					  ((eq (car slot) 'class)
					   (cons :class-name (cons (cadr slot) acc)))
					  ((member (car slot)
						   '(style onmouseup onmousemove
						     onclick className ondblclick
						     onmousedown onkeypress onkeydown
						     onkeyup dir lang dojoType open
						     onmouseover onmouseout))
					   acc)
					  (t
					   (cons (make-keyword (car slot)) (cons (cadr slot) acc)))))
				      remote-slots)))
			 (methods
			  (jobject
	       
			   ;; ----------------------------------------------------------------------------
			   ;; Remote Methods
			   ;; ----------------------------------------------------------------------------
			   ,@(reduce0 (lambda (acc method)
					(let ((proxy (intern (format nil "~A/JS" (car method))
							     (if (eq (symbol-package (car method))
								     (find-package :common-lisp))
								 (find-package :core-server)
								 (symbol-package (car method))))))
					  (cons (make-keyword (car method))
						(cons `(make-method ,(funcall proxy class+ nil)) acc))))
				      (remove 'destroy
					      (remove 'init (class+.remote-methods class+) :key #'car)
					      :key #'car))
			     
			   ;; ----------------------------------------------------------------------------
			   ;; Local Methods
			   ;; ----------------------------------------------------------------------------
			   ,@(reduce0 (lambda (acc method)
					(destructuring-bind (method . k-url) method
					  (let ((proxy (intern (format nil "~A/JS" (car method))
							       (if (eq (symbol-package (car method))
								       (find-package :common-lisp))
								   (find-package :core-server)
								   (symbol-package (car method))))))
					    (cons (make-keyword (car method))
						  (cons (funcall proxy class+ k-url) acc )))))
				      (mapcar #'cons (class+.local-methods class+) k-urls)))))

		     		     
		     ;; -------------------------------------------------------------------------
		     ;; Inject Methods to Instance
		     ;; -------------------------------------------------------------------------
		     (extend methods to-extend)

		     ;; -------------------------------------------------------------------------
		     ;; Inject Default Values Differentially
		     ;; -------------------------------------------------------------------------
		     (mapobject (lambda (k v)
				  (if (or (and (not (null v))
					       (or (eq "" (slot-value to-extend k))
						   (null (slot-value to-extend k))
						   (eq "undefined" (slot-value to-extend k))))
					  (eq "undefined" (typeof (slot-value to-extend k))))
				      (setf (slot-value to-extend k) v)))
				slots)

		     (let ((to-extend ,(if dom-tag
					   `(if (null (slot-value to-extend 'node-name))
						(extend to-extend
							(document.create-element
							 ,(symbol-to-js dom-tag)))
						to-extend)
					   'to-extend)))
		       (apply (make-method ,(funcall 'init/js class+ nil)) to-extend null)
		       (let ((destroy (slot-value to-extend 'destroy)))
			 (setf (slot-value to-extend 'destroy)
			       (compose-prog1-cc (make-method ,(funcall 'destroy/js class+ nil))
						 destroy)))
		       ;; (when (typep (slot-value to-extend 'init) 'function)
		       ;;   (init to-extend))
		 
		       to-extend))))))))
       
       ;; ----------------------------------------------------------------------------
       ;; Component Constructor Renderer
       ;; ----------------------------------------------------------------------------
       (defmethod/cc component! ((stream core-stream) (component ,class-name))
	 (when +context+
	   (setf (slot-value component 'url)
		 (web-application.serve-url (context.application +context+)
					    (context.request +context+)))) 

	 (let ,(mapcar (lambda (method k-url)
			 (let ((method-args (extract-argument-names (cdddr method)
								    :allow-specializers t)))
			   `(,k-url (let ((+action-hash-override+ (component.action-hash component ',(car method))))
				      (action/url (,@(reduce0
						      (lambda (acc arg)
							(cons (list arg (symbol-to-js arg))
							      acc))
						      method-args))
					(let ((result (,(car method) component
							,@(mapcar (lambda (arg)
								    `(component.deserialize component ,arg))
								  method-args))))
					  (with-query ((hash "__hash")) (context.request +context+)
					    (let ((stream (http-response.stream (context.response +context+)))
						  (hash (json-deserialize (if (listp hash) (car hash) hash))))
					      (javascript/suspend
					       (lambda (stream)
						 (if hash
						     (with-js (result hash) stream
						       (with-call/cc
							 (apply (slot-value window hash) window
								(list (lambda (self) result)))))
						     (with-js (result) stream
						       (with-call/cc
							 (lambda (self) result))))))
					      nil))))))))
		       (class+.local-methods class+) k-urls)
	   (%component! stream component ,@k-urls))))))

;; ----------------------------------------------------------------------------
;; Default Funkall Method for Components
;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :execute :load-toplevel)
  (setf (gethash 'call-next-method +javascript-cps-functions+) t)
  (setf (gethash 'method +javascript-cps-functions+) t))

(defmethod/remote init ((self component)) self)
(defmethod/remote destroy ((self component))
  (delete-slot self 'destroy)
  self)

(defmethod/remote funkall ((self component) action args)
  (let ((retval (funcall-cc (+ (slot-value self 'url) action "$") args)))
    (if (typep retval 'function)
    	(call/cc retval self) 
    	retval)))

(defmethod/remote upgrade-component ((self component) new-version)
  (call/cc new-version self)
  (suspend))

(defmethod/cc continue-component ((component component) &optional value)
  (javascript/suspend
   (lambda (stream)
     (let ((hash (uri.query (http-request.uri (context.request +context+))
			    "__hash")))
       (if (and (stringp hash) (> (length hash) 0))
	   (let ((hash (intern hash)))
	     (with-js (value hash component) stream
	       (with-call/cc
		 (apply (slot-value window hash) window
			(list (lambda (self) value)))))
	     (unintern hash))
	   (with-js (component) stream
	     (with-call/cc
	       (lambda (self) value))))))))

(defmethod/cc continue/js (value)
  (javascript/suspend
   (lambda (stream)
     (let ((hash (uri.query (http-request.uri (context.request +context+))
			    "__hash")))
       (cond
	 ((and (stringp hash) (> (length hash) 0))
	  (let ((hash (intern hash)))
	    (with-js (value hash) stream
	      (with-call/cc
		(apply (slot-value window hash) window
		       (list (lambda (self) value)))))
	    (unintern hash)))
	 ((typep value 'component)	  
	  (with-js (value) stream
	    (let ((c value))	      
	      (add-on-load
	       (lambda ()
		 (c null (lambda (a) a)))))))
	 (t
	  (with-js (value) stream
	    (with-call/cc
	      value
	      ;; (lambda (self) value)
	      ))))))))

(defmethod/remote answer-component ((self component) arg)
  ;; (console.debug (list "answering" arg))
  (let ((retval (slot-value self 'k)))
    (if (typep retval 'function)
	(apply retval self (array arg))
	(throw (new (*error "No k found for component"))))))

(defmethod/cc answer-component ((self component) arg)
  ;; (update-session 'controller (cdr (query-session 'controller)))
  (answer arg))

(defmethod/remote call-component ((self component))
  (let/cc k1
    (setf (slot-value self 'k) k1)
    (suspend)))

(defmethod/cc call-component ((component component))    
  (javascript/suspend
   (lambda (stream)
     (let ((hash (uri.query (http-request.uri (context.request +context+)) "__hash")))
       (if (and (stringp hash) (> (length hash) 0))
	   (let ((hash (intern hash)))
	     (with-js (hash component) stream
	       (with-call/cc
		 (apply (slot-value window hash) window
			(list (lambda (self) component)))))
	     (unintern hash))
	   (with-js (component) stream
	     ((lambda ()
		(let ((component component))
		  (component null window.k))))))))))

(defmethod write-stream ((stream core-stream) (object component))
  (prog1 stream
    (with-call/cc
      (component! stream object))))

(defmethod shared-initialize :after ((self component) slots &key &allow-other-keys)
  (if (member 'id (mapcar #'slot-definition-name (class+.slots (class-of self))))
      (if (or (not (slot-boundp self 'id)) (null (slot-value self 'id)))
	  (setf (slot-value self 'id) (random-string 5))))
  
  (if (typep self 'xml)
      (setf (xml.children self)
  	    (cons 
  	     (<:script :type "text/javascript"
  		       (with-call/cc		
  			 (lambda (stream)
  			   (let ((stream (make-indented-stream stream))
  				 (component self)
  				 (id (slot-value self 'id)))
  			     (with-js (component id) stream
  			       ((lambda ()
  				  (let ((ctor component))
  				    (ctor (document.get-element-by-id id) window.k)))))))))
  	     (xml.children self))))
  self)

(defmethod write-stream ((stream html-stream) (self component))
  (write-stream stream
		(<:script :type "text/javascript"
			  (with-call/cc		
			    (lambda (stream)
			      (let ((stream (make-indented-stream stream))
				    (component self))
				(with-js (component) stream
				  ((lambda ()
				     (let ((ctor component))
				       (ctor null window.k)))))))))))

(defmethod json! ((stream core-stream) (component component))
  (with-call/cc
    (component! stream component)))

(defun find-component (name)
  (car
   (find name
	 (mapcar #'cons
		 (class-subclasses (find-class 'component))
		 (mapcar (compose #'symbol-to-js #'class-name)
			 (class-subclasses (find-class 'component))))
	 :test #'string=
	 :key #'cdr)))


;; -------------------------------------------------------------------------
;; Singleton Component Mixin
;; -------------------------------------------------------------------------
(defcomponent singleton-component-mixin ()
  ())

(defmethod/local destroy-component-actions ((self singleton-component-mixin))
  (mapcar (lambda (method)
	    (remhash (component.action-hash self method)
		     (session.continuations (context.session +context+))))
	  (class+.local-methods (class-of self))))

(defmethod/remote destroy ((self singleton-component-mixin))
  (make-web-thread (lambda () (destroy-component-actions self)))
  (call-next-method self))

;; (defmethod/cc continue-component ((self component) &optional value)
;;   (prog1 nil
;;     (http-response.set-content-type (context.response +context+) '("text" "javascript" ("charset" "UTF-8")))
;;     (let ((stream (if (application.debug (context.application +context+))
;; 		      (make-indented-stream (http-response.stream (context.response +context+)))
;; 		      (make-compressed-stream (http-response.stream (context.response +context+)))))
;; 	  (hash (uri.query (http-request.uri (context.request +context+)) "__hash")))
;;       (if (and (stringp hash) (> (length hash) 0))
;; 	  (let ((hash (intern hash)))
;; 	    (with-js (hash value) stream
;; 	      (apply (slot-value window hash) window (list value))))
;; 	  (with-js (value) stream
;; 	    value)))))

;; ;; +----------------------------------------------------------------------------
;; ;; | Component Dispatcher
;; ;; +----------------------------------------------------------------------------
;; (defhandler "^component\.core" ((application http-application) (component "component")
;; 				(hash "__hash"))
;;   (javascript/suspend
;;    (lambda (stream)
;;      (when (and (stringp hash) (> (length hash) 0))
;;        (string! stream "var ")
;;        (string! stream (json-deserialize hash))
;;        (string! stream " = "))
;;      (acond
;;       ((and (typep component 'string) (find-component (json-deserialize component)))
;;        (component! stream (make-instance it)))
;;       (t
;;        (with-js () stream
;; 	 (lambda ()
;; 	   (throw (new (*error "No Components Found - Core Server [http://labs.core.gen.tr]"))))))))))

;; ;; +----------------------------------------------------------------------------
;; ;; | Service Dispatcher
;; ;; +----------------------------------------------------------------------------
;; (defhandler "^service\.core$" ((application http-application) (component "service")
;; 			       (hash "__hash"))
;;   (javascript/suspend
;;    (lambda (stream)
;;      (when (and (stringp hash) (> (length hash) 0))
;;        (string! stream "var ")
;;        (string! stream (json-deserialize hash))
;;        (string! stream " = "))
;;      (acond
;;        ((and (typep component 'string) (find-component (json-deserialize component)))
;; 	(let ((component (query-session it)))
;; 	  (if component
;; 	      (component! stream component)
;; 	      (component! stream (update-session it (make-instance it))))))
;;        (t
;; 	(with-js () stream
;; 	  (lambda ()
;; 	    (throw (new (*error "No Services Found - Core Server [http://labs.core.gen.tr]"))))))))))

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

;; (defmethod/cc replace-component ((component component) new-version)
;;   (javascript/suspend
;;    (lambda (stream)
;;      (let ((hash (uri.query (http-request.uri (context.request +context+)) "__hash")))
;;        (if (and (stringp hash) (> (length hash) 0))
;; 	   (let ((hash (intern hash)))
;; 	     (with-js (hash new-version) stream
;; 	       (apply (slot-value window hash) window
;; 		      (list (with-call/cc (lambda (self) (replace-component self new-version)))))))
;; 	   (with-js (component) stream
;; 	     component))))))

;; (defmethod/cc upgrade-component ((component component) new-version)
;;   (javascript/suspend
;;    (lambda (stream)
;;      (let ((hash (uri.query (http-request.uri (context.request +context+)) "__hash")))
;;        (if (and (stringp hash) (> (length hash) 0))
;; 	   (let ((hash (intern hash)))
;; 	     (with-js (hash new-version) stream
;; 	       (apply (slot-value window hash) window
;; 		      (list (with-call/cc (lambda (self) (replace-component self new-version)))))))
;; 	   (with-js (component) stream
;; 	     component))))))

;;;; HEK: dont want anymore -evrim.
;; (defmethod shared-initialize :after ((self component) slots &key &allow-other-keys)
;;   (if (member 'id (mapcar #'slot-definition-name (class+.slots (class-of self))))
;;       (if (or (not (slot-boundp self 'id)) (null (slot-value self 'id)))
;; 	  (setf (slot-value self 'id) (random-string 5))))
  
;;   (if (typep self 'xml)
;;       (setf (xml.children self)
;; 	    (cons 
;; 	     (<:script :type "text/javascript"
;; 		       (with-call/cc		
;; 			 (lambda (stream)
;; 			   (let ((stream (make-indented-stream stream)))
;; 			     (component! stream self)
;; 			     (when (typep self 'xml)
;; 			       (write-stream stream
;; 					     (js*
;; 					       `(progn
;; 						  (new
;; 						   (,(class-name (class-of self))
;; 						     (create)
;; 						     (document.get-element-by-id ,(slot-value self 'id))))))))))))
;; 	     (xml.children self))))
;;   self)

;; (component! stream self)
		     ;; (when (typep self 'xml)
			     ;;   (write-stream stream
			     ;; 		     (js*
			     ;; 		       `(progn
			     ;; 			  (new
			     ;; 			   (,(class-name (class-of self))
			     ;; 			     (create)
			     ;; 			     (document.get-element-by-id ,(slot-value self 'id))))))))
;; (defmethod component+.covariant-functor-name ((self component+) function-name)
;;   (intern (format nil "~A/JS" function-name)
;; 	  (if (eq (find-package :common-lisp) (symbol-package function-name))
;; 	      (find-package :core-server) (symbol-package function-name))))

;; (defmethod component+.codomain-function-name ((self component+) function-name)
;;   (intern (format nil ".~A" function-name) 
;; 	  (if (eq (find-package :common-lisp) (symbol-package function-name))
;; 	      (find-package :core-server) (symbol-package function-name))))

;; (defmethod component+.a ((domain component+) method component+ args body)
;;   `)

