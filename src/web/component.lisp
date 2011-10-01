;; +-------------------------------------------------------------------------
;; | Component Framework (Lisp->Browser Functor)
;; +-------------------------------------------------------------------------
(in-package :core-server)

;; +-------------------------------------------------------------------------
;; | Component Metaclass
;; +-------------------------------------------------------------------------
(defclass+ component+ (class+)
  ((%ctor-timestamp :initform 0)))

;; +-------------------------------------------------------------------------
;; | Component Class
;; +-------------------------------------------------------------------------
(defclass+ component ()
  ((url :host remote :documentation "The url that this component is served.")
   (instance-id :host none))
  (:metaclass component+)
  (:documentation "Base component class"))

(defmethod component.instance-id ((self component))
  (or (slot-value self 'instance-id)
      (setf (slot-value self 'instance-id)
	    (format nil "~A-~A"
		    (symbol-to-js
		     (string-replace-all
		      "/" "-"
		      (format nil "~A" (class-name (class-of self)))))
		    (random-string 8)))))

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

;; FIXME: -evrim.
(defmethod component.serialize-slot ((self component) slot-name)
  (slot-value self slot-name)
  ;; (let ((slot (class+.find-slot (class-of self) slot-name)))
  ;;   (with-slotdef (reader) slot
  ;;     (funcall reader self)))
  )

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

;; +-------------------------------------------------------------------------
;; | defmethod/local macro: Defines a local method
;; +-------------------------------------------------------------------------
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
	 (defmethod ,morphism ((self ,metaclass))
	   `(method ,',args	    
	      (funkall self (+ "?s:" server-session-id
			       "$k:" component-instance-id
			       "$method:" ,',(symbol-name name))
		 (create
		  ,@',(nreverse
		       (reduce0
			(lambda (acc arg)
			  (cons arg (cons (make-keyword arg) acc)))
			(extract-argument-names args
						:allow-specializers t)))))))
	 (defmethod/cc ,name ((,self ,class-name) ,@args)
	   (let ((application (component.application ,self)))
	     ,@body))))))

(defmacro defmethod/local (name ((self class-name) &rest args) &body body)
  (component+.local-morphism (find-class class-name) name self args body))

;; +-------------------------------------------------------------------------
;; | defmethod/remote macro: Defines a remote method
;; +-------------------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod component+.remote-morphism ((class component+) name self args body)
    (let* ((class-name (class-name class))
	   (metaclass (class-name (class-of class)))
	   (functor (component+.morphism-function-name class name))
	   (remote (component+.codomain-function-name class name))
	   (call-next-method-p (any (lambda (application-form)
				      (eq 'call-next-method
					  (operator application-form)))
				    (ast-search-type
				     (walk-form `(lambda () ,@body))
				     (list 'application-form)))))
      `(progn
	 (class+.add-method (find-class+ ',class-name) ',name 'remote '((,self ,class-name) ,@args))
	 (eval-when (:load-toplevel :compile-toplevel :execute)
	   (setf (gethash ',name +javascript-cps-functions+) t
		 (gethash ',remote +javascript-cps-functions+) t)
	   (defjsmacro ,name (&rest args) `(,',remote ,@args)))
	 (defmethod ,functor ((self ,metaclass))
	   ,(if call-next-method-p
		``(method ,',args
			  (let ((call-next-method
				 (lambda (,',self ,@',args)
				   ,@(cddr (call-next-method self)))))
			    ,@',body))
		``(method ,',args
			  (let ((,',self self))
			    ,@',body))))
	 ,(unless (eq name 'destroy)
	    `(defmethod/cc ,name ((,self ,class-name) ,@args)
	       (let (,@(mapcar (lambda (arg)
				 `(,arg (component.serialize ,self ,arg)))
			       (extract-argument-names args
						       :allow-specializers t)))
		 (with-query ((hash "__hash")) (context.request +context+)
		   (let ((hash (json-deserialize hash)))
		     (javascript/suspend
		      (lambda (stream)
			(let ((result (action/url ((result "result"))
					(context.remove-current-action +context+)
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
					     (create :result (,name self ,@args)))))))))))))))))))

(defmacro defmethod/remote (name ((self class-name) &rest args) &body body)
  (component+.remote-morphism (find-class class-name) name self args body))

;; --------------------------------------------------------------------------
;; defcomponent-accessors Macro: Defines remote and local accessors
;; for slots of a component
;; --------------------------------------------------------------------------
(defmacro defcomponent-accessors (class-name slots)
  (flet ((reader (name)
	   (intern (format nil "GET-~A" name) (symbol-package name)))
	 (writer (name)
	   (intern (format nil "SET-~A" name) (symbol-package name)))
	 (method-type (host)
	   (ecase host
	     (local 'defmethod/local)
	     ((or both remote) 'defmethod/remote))))    
    `(progn
       ,@(reduce0
	  (lambda (acc slot)
	    (let ((name (car slot))
		  (accessor (cadr slot))
		  (host (caddr slot))
		  (export (cdddr slot)))
	      (if export
		  (append acc
			  `((,(method-type host) ,(reader name) ((self ,class-name))
			      (slot-value self ',name))
			    (,(method-type host) ,(writer name) ((self ,class-name) value)
			      (setf (slot-value self ',name) value))
			    (eval-when (:compile-toplevel :execute :load-toplevel)
			      (defmacro/js ,accessor (self)
				`(,',(reader name) ,self))
			      (defsetf/js ,accessor (value self)
				`(,',(writer name) ,self ,value)))))
		  acc)))
	  (filter (lambda (slot) (eq (caddr slot) 'local))
		  slots))
       ,@(reduce0
	  (lambda (acc slot)
	    (let* ((name (car slot))
		   (accessor (cadr slot))
		   (macro1 `(defmacro/js ,accessor (self)
			      `(slot-value ,self ',',name)))
		   (macro2 `(defsetf/js ,accessor (value self)
			      `(setf (slot-value ,self ',',name) ,value))))
	      (cond
		((and (not (gethash accessor *javascript-macros*))
		      (not (gethash accessor *javascript-setf-macros*)))
		 (cons macro1 (cons macro2 acc)))
		((not (gethash accessor *javascript-macros*))
		 (cons macro1 acc))
		((not (gethash accessor *javascript-setf-macros*))
		 (cons macro2 acc))
		(t acc))))
	  (filter (lambda (slot)
		    (or (eq (caddr slot) 'both)
			(eq (caddr slot) 'remote)))
		  slots)))))

;; +-------------------------------------------------------------------------
;; | defcomponent Macro: Defines a new component
;; +-------------------------------------------------------------------------
(defmacro defcomponent (name supers slots &rest rest)
  (let* ((metaclass1 (cadr (assoc :metaclass rest)))
	 (metaclass (intern (format nil "~A+" name)))
	 (metasupers (remove 'class+
			     (uniq (append
				    (mapcar (compose #'class-name #'class-of
						     #'find-class)
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
       (defcomponent-accessors ,name
	   ,(mapcar
	     (lambda (slot)
	       (cons (car slot)
		     (cons (or (getf (cdr slot) :accessor)
			       (car slot))
			   (cons (or (getf (cdr slot) :host)
				     'local)
				 (if (member :export slot)
				     (getf (cdr slot) :export)
				     t)))))
	     slots))
       (find-class+ ',name))))

(defmethod/cc component.update-url ((self component))  
  (when +context+
    (setf (slot-value self 'url)
	  (web-application.serve-url (context.application +context+)
				     (context.request +context+)))))

(defmethod/cc component! ((stream core-stream) (component component))
  (component.update-url component)
  (cond
    ((null (remove '_destroy (class+.local-methods (class-of component))
		   :key #'car))
     (%component! stream component))
    (t
     (let ((+action-hash-override+ (component.instance-id component))
	   (k (if +context+
		  (context.continuation +context+)
		  "invalid-kontinuation")))       
       (let ((hash
	      (action/url ((method-name "method"))
		(let ((method (find (string-upcase method-name)
				    (class+.local-methods
				     (class-of component))
				    :test #'string= :key #'car)))
		  (cond
		    ((null method)
		     (warn "Method not found ~A.~A"
			   (component.instance-id component)
			   method-name)
		     nil)
		    (t
		     (let* ((args (mapcar
				   (lambda (arg)
				     (json-deserialize
				      (http-request.query
				       (context.request +context+)
				       (symbol-to-js arg))))
				   (mapcar (compose #'car #'ensure-list)
					   (cdddr method))))
			    (result (apply (car method) component args))
			    (hash (json-deserialize
				   (http-request.query
				    (context.request +context+)
				    "__hash"))))
		       (kall k +context+
			(javascript/suspend
			 (lambda (stream)
			   (if hash
			       (with-js (result hash) stream
				 (with-call/cc
				   (apply (slot-value window hash) window
					  (list (lambda (self) result)))))
			       (with-js (result) stream
				 (with-call/cc
				   (lambda (self) result))))))))))))))
	 (%component! stream component))))))

;; --------------------------------------------------------------------------
;; This around method allows us to compile constructor evertime class
;; definition changed.
;; --------------------------------------------------------------------------
(defcomponent cached-component ()
  ())

(defmethod/cc component! :around ((stream core-stream) (component component))
  (if  (> (slot-value (class-of component) '%timestamp)
	  (slot-value (class-of component) '%ctor-timestamp))      
       (let ((name (class-name (class-of component))))
	 (format *standard-output* "Compiling constructor for ~A.~%" name)
	 (if (typep component 'cached-component)
	     (eval (component+.ctor2 (class-of component)))
	     (eval (component+.ctor (class-of component))))
	 (setf (slot-value (class-of component) '%ctor-timestamp)
	       (get-universal-time))
	 (component! stream component))
       (call-next-method stream component)))

(defmethod %component! ((stream core-stream) (component+ component+))
  (if  (> (slot-value component+ '%timestamp)
	  (slot-value component+ '%ctor-timestamp))      
       (let ((name (class-name component+)))
	 (format *standard-output* "Compiling constructor for ~A.~%" name)
	 (eval (component+.ctor2 component+))
	 (setf (slot-value component+ 'core-server::%ctor-timestamp)
	       (get-universal-time))
	 (%component! stream component+))
       (%component! stream component+)))

(defvar +omitted-component-slots+
  '(style onmouseup onmousemove onclick className ondblclick
    onmousedown onkeypress onkeydown onkeyup dir lang dojoType open
    onmouseover onmouseout onfocus onchange onselect onblur))

(defmethod component+.ctor ((component component+))
  (labels ((remove-methods (names methods)
	     (if (null names)
		 methods
		 (remove-methods (cdr names)
				 (remove (car names) methods :key #'car)))))
    (let* ((class-name (class-name component))
	   (class+ (find-class+ class-name))	 	 
	   (local-methods (remove '_destroy (class+.local-methods class+)
				  :key #'car))
	   (remote-slots (mapcar (lambda (slot)
				   (with-slotdef (name reader initarg) slot
				     (list name
					   (intern (symbol-name (gensym)))
					   reader
					   (intern (symbol-name initarg)))))
				 (class+.remote-slots class+)))
	   (dom-tag (any #'xml+.tag
			 (reverse
			  (filter (lambda (class)
				    (if (typep class 'xml+) class))
				  (cdr (class+.superclasses class+)))))))
    
      ;; --------------------------------------------------------------------
      ;; Component Internal Render Method 
      ;; --------------------------------------------------------------------
      `(defmethod %component! ((stream core-stream) (component ,class-name))
	 (let ((component-instance-id (component.instance-id component))
	       (server-session-id (if +context+
				      (session.id (context.session +context+))
				      "unbound-session-id"))
	       (component-destroy-uri (if +context+
					  (format nil "~Adestroy.core"
						  (web-application.base-url
						   (component.application component)
						   (context.request +context+)))
					  "TEST-COMPONENT-DESTROY.core"))
	       ,@(mapcar (lambda (slot)
			   `(,(cadr slot)
			      (component.serialize-slot component
							',(car slot))))
			 remote-slots))
	   (declare (ignorable component-instance-id server-session-id
			       component-destroy-uri
			       ,@(mapcar #'cadr remote-slots)))
	   (with-js (component-instance-id server-session-id
					   component-destroy-uri
					   ,@(mapcar #'cadr remote-slots))
	       stream
	     ;; -------------------------------------------------------------
	     ;; Constructor
	     ;; -------------------------------------------------------------
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
					((member (car slot) +omitted-component-slots+)
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
				      (let* ((name (car method))
					     (proxy (component+.morphism-function-name class+ name)))
					(cons (make-keyword name)
					      (cons `(make-method ,(funcall proxy class+)) acc))))
				    (if (null local-methods)
					(remove-methods '(funkall destroy init)
							(class+.remote-methods class+))
					(remove-methods '(destroy init)
							(class+.remote-methods class+))))

		       
			 ;; ----------------------------------------------------------------------------
			 ;; Local Methods
			 ;; ----------------------------------------------------------------------------
			 ,@(reduce0 (lambda (acc method)
				      (let* ((name (car method))
					     (proxy (component+.morphism-function-name class+ name)))
					(cons (make-keyword name)
					      (cons `(make-method ,(funcall proxy class+)) acc))))
				    (remove-methods '(_destroy) local-methods)))))

		     		     
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
		     (setf (slot-value to-extend 'ctor) (slot-value arguments 'callee))
		     (apply (make-method ,(funcall 'init/js class+)) to-extend null)
		     
		     ,(if (null (remove-methods '(_destroy) local-methods))			  
			  `(setf (slot-value to-extend 'destroy)
				 (compose-prog1-cc (make-method ,(funcall 'destroy/js class+))
						   (slot-value to-extend 'destroy)))
			  `(setf (slot-value to-extend 'destroy)
				 (compose-prog1-cc (make-method ,(funcall 'destroy/js class+))
						   (slot-value to-extend 'destroy))
				 (slot-value to-extend '_destroy)
				 (make-method ,(funcall '_destroy/js class+))))
		 
		     to-extend))))))))))

(defmethod component+.ctor2 ((component component+))
  (labels ((remove-methods (names methods)
	     (if (null names)
		 methods
		 (remove-methods (cdr names)
				 (remove (car names) methods :key #'car)))))
    (let* ((class-name (class-name component))
	   (class+-name (class-name (class-of component)))
	   (class+ (find-class+ class-name))	 	 
	   (local-methods (remove '_destroy (class+.local-methods class+)
				  :key #'car))
	   (remote-slots (mapcar (lambda (slot)
				   (with-slotdef (name reader initarg) slot
				     (list name
					   (intern (symbol-name (gensym)))
					   reader
					   (intern (symbol-name initarg)))))
				 (class+.remote-slots class+)))
	   (dom-tag (any #'xml+.tag
			 (reverse
			  (filter (lambda (class)
				    (if (typep class 'xml+) class))
				  (cdr (class+.superclasses class+)))))))
    
      ;; --------------------------------------------------------------------
      ;; Component Internal Render Method 
      ;; --------------------------------------------------------------------
      `(progn
	 (defmethod %component! ((stream core-stream) (component ,class-name))
	   (let ((component-instance-id (component.instance-id component))
		 (server-session-id (if +context+
					(session.id (context.session +context+))
					"unbound-session-id"))
		 (class-name (string (class-name (class-of component))))
		 (component-loader-uri (if +context+
					   (format nil "~Acomponent.core"
						   (web-application.base-url
						    (component.application component)
						    (context.request +context+)))
					   "TEST-COMPONENT-LOADER.core"))
		 (component-destroy-uri (if +context+
					    (format nil "~Adestroy.core"
						    (web-application.base-url
						     (component.application component)
						     (context.request +context+)))
					    "TEST-COMPONENT-DESTROY.core"))
		 ,@(mapcar (lambda (slot)
			     `(,(cadr slot)
				(component.serialize-slot component
							  ',(car slot))))
			   remote-slots))
	     (declare (ignorable component-instance-id server-session-id
				 component-destroy-uri component-loader-uri
				 ,@(mapcar #'cadr remote-slots)))
	     (with-js (component-instance-id server-session-id class-name
					     component-destroy-uri
					     component-loader-uri
					     ,@(mapcar #'cadr remote-slots))
		 stream
	       ;; -------------------------------------------------------------
	       ;; Constructor
	       ;; -------------------------------------------------------------
	       (with-call/cc
		 (lambda (to-extend)
		   (let ((args arguments)
			 (to-extend (or to-extend (new (*object))))
			 (slots
			  (jobject
			   ;; ----------------------------------------------------------------------------
			   ;; Remote Slot Initial Values
			   ;; ----------------------------------------------------------------------------
			   ,@(reduce0 (lambda (acc slot)
					(cond
					  ((eq (car slot) 'class)
					   (cons :class-name (cons (cadr slot) acc)))
					  ((member (car slot) +omitted-component-slots+)
					   acc)
					  (t
					   (cons (make-keyword (car slot)) (cons (cadr slot) acc)))))
				      remote-slots)))
			 (methods
			  (jobject
			   ;; ----------------------------------------------------------------------------
			   ;; Local Methods
			   ;; ----------------------------------------------------------------------------
			   ,@(reduce0 (lambda (acc method)
					(let* ((name (car method))
					       (proxy (component+.morphism-function-name class+ name)))
					  (cons (make-keyword name)
						(cons (funcall proxy class+) acc))))
				      (remove-methods '(_destroy) local-methods)))))

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

		     ,(if local-methods
		     	  `(setf (slot-value to-extend '_destroy)
		     		 (make-method ,(funcall '_destroy/js class+))))

		     ;; Save Constructor
		     (setf (slot-value to-extend 'ctor) (slot-value args 'callee))

		     (with-slots (component-cache) window		       
		       (aif (slot-value component-cache class-name)
			    (call/cc it to-extend)
			    (let ((ctor (funcall-cc
					 (+ component-loader-uri
					    "?s:" server-session-id
					    "$component:" (encode-u-r-i-component class-name) "$")
					 (jobject))))
			      (setf (slot-value component-cache class-name) ctor)
			      (apply ctor to-extend (list to-extend)))))))))))
	 
	 (defmethod %component! ((stream core-stream) (component ,class+-name))
	   (let ((component-instance-id "unbound-instance-id")
		 (server-session-id (if +context+
					(session.id (context.session +context+))
					"unbound-session-id")))
	     (declare (ignorable component-instance-id server-session-id))
	     (with-js (component-instance-id server-session-id)
		 stream
	       ;; -------------------------------------------------------------
	       ;; Constructor
	       ;; -------------------------------------------------------------
	       (with-call/cc
		 (lambda (to-extend)
		   (let ((to-extend (or to-extend (new (*object))))
			 (methods
			  (jobject
			   ;; ----------------------------------------------------------------------------
			   ;; Remote Methods
			   ;; ----------------------------------------------------------------------------
			   ,@(reduce0 (lambda (acc method)
					(let* ((name (car method))
					       (proxy (component+.morphism-function-name class+ name)))
					  (cons (make-keyword name)
						(cons `(make-method ,(funcall proxy class+)) acc))))
				      (if (null local-methods)
					  (remove-methods '(funkall destroy _destroy init)
							  (class+.remote-methods class+))
					  (remove-methods '(destroy _destroy init)
							  (class+.remote-methods class+)))))))
		     		     
		     ;; -------------------------------------------------------------------------
		     ;; Inject Methods to Instance
		     ;; -------------------------------------------------------------------------
		     (extend methods to-extend)
		     (setf (slot-value to-extend 'destroy)
			   (compose-prog1-cc (make-method ,(funcall 'destroy/js class+))
					     (slot-value to-extend 'destroy)))
		     

		     (let ((to-extend ,(if dom-tag
					   `(if (null (slot-value to-extend 'node-name))
						(extend to-extend
							(document.create-element
							 ,(symbol-to-js dom-tag)))
						to-extend)
					   'to-extend)))		       
		       (apply (make-method ,(funcall 'init/js class+)) to-extend null)		     		       
		       to-extend)))))))))))

;; -------------------------------------------------------------------------
;; Component Loader
;; -------------------------------------------------------------------------
(defun find-component (component)
  (find (string-upcase component)
	(class+.subclasses (find-class 'component))
	:key #'class-name :test #'string=))

(defhandler "component.core" ((self http-application) (component "component")
			      (hash "__hash"))
  (let ((class (find-component component)))
    (assert (not (null hash)))
    (javascript/suspend
     (lambda (stream)
       ;; (apply (slot-value window hash) window
       ;; 	      (list (lambda (self) component)))
       (string! stream
		"apply(window[\"")
       (string! stream (json-deserialize hash))
       (string! stream "\"], window, [")
       (if class
	   (%component! stream class)
	   (string! stream "null"))
       (string! stream "], window.k);")))))



(defhandler "destroy.core" ((self http-application) (objects "objects")
			    (hash "__hash"))
  (mapcar (lambda (object) (context.remove-action +context+ object))
	  (ensure-list (json-deserialize objects)))
  (javascript/suspend
   (lambda (stream)
     (string! stream "apply(window[\"")
     (string! stream (json-deserialize hash))
     (string! stream "\"], window, [null],window.k);"))))

;; --------------------------------------------------------------------------
;; Default Funkall Method for Components
;; --------------------------------------------------------------------------
(eval-when (:compile-toplevel :execute :load-toplevel)
  (setf (gethash 'call-next-method +javascript-cps-functions+) t)
  (setf (gethash 'method +javascript-cps-functions+) t))

(defmethod/remote funkall ((self component) action args)
  (let ((retval (funcall-cc (+ (slot-value self 'url) action "$") args)))
    (if (typep retval 'function)
    	(call/cc retval self) 
    	retval)))

(defmethod/remote init ((self component)) self)
(defmethod/remote _destroy ((self component))
  (add-to-gc (lambda ()
	       (list (+ component-destroy-uri "?s:" server-session-id)
		     component-instance-id)))
  t)

(defmethod/remote client-destroy ((self component))
  (setf (slot-value self '_destroy) (lambda () this))
  (destroy self))

(defmethod/remote destroy ((self component))
  (let ((__destroy (slot-value self '_destroy)))
    (when (not (null __destroy))
      (_destroy self)
      (delete-slot self '_destroy)))
  (delete-slot self 'destroy)
  self)

(defmethod/cc destroy ((self component))
  ;; (describe (list 'destroy self))
  (context.remove-action +context+ (component.instance-id self)))

(defmethod/cc call-component ((component component))    
  (javascript/suspend
   (lambda (stream)
     (let ((hash (http-request.query (context.request +context+) "__hash")))
       (if hash
	   (with-js (hash component) stream
	     (with-call/cc
	       (apply (slot-value window hash) window
		      (list (lambda (self) component))))) 
	   (with-js (component) stream
	     ((lambda ()
		(let ((component component))
		  (component null window.k))))))))))

(defmethod/cc answer-component ((self component) arg)
  (answer arg))

(defmethod/cc continue-component ((component component) &optional value)
  (javascript/suspend
   (lambda (stream)
     (let ((hash (json-deserialize
		  (http-request.query (context.request +context+)
				      "__hash"))))
       (if hash
	   (with-js (value hash component) stream
	     (with-call/cc
	       (apply (slot-value window hash) window
		      (list (lambda (self) value)))))	   
	   (with-js (component) stream
	     (with-call/cc
	       (lambda (self) value))))))))

(defun/cc continue/js (value)
  (javascript/suspend
   (lambda (stream)
     (let ((hash (json-deserialize
		  (http-request.query
		   (context.request +context+) "__hash"))))
       (cond
	 (hash
	  (with-js (value hash) stream
	    (with-call/cc
	      (apply (slot-value window hash) window
		     (list (lambda (self) value))))))
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

;; -------------------------------------------------------------------------
;; Callable Component
;; -------------------------------------------------------------------------
(defcomponent callable-component ()
  ())

(defmethod/remote answer-component ((self callable-component) arg)
  ;; (console.debug (list "answering" arg))
  (let ((retval (slot-value self 'k)))
    (if (typep retval 'function)
	(apply retval self (array arg))
	(throw (new (*error "No k found for component"))))))

(defmethod/cc answer-component ((self callable-component) arg)
  (answer arg))

(defmethod/remote call-component ((self callable-component))
  (let/cc k1
    (setf (slot-value self 'k) k1)
    (suspend)))

(defmethod/cc call-component ((component callable-component))    
  (javascript/suspend
   (lambda (stream)
     (let ((hash (http-request.query (context.request +context+) "__hash")))
       (if hash
	   (with-js (hash component) stream
	     (with-call/cc
	       (apply (slot-value window hash) window
		      (list (lambda (self) component))))) 
	   (with-js (component) stream
	     ((lambda ()
		(let ((component component))
		  (component null window.k))))))))))

;; -------------------------------------------------------------------------
;; Singleton Component Mixin
;; -------------------------------------------------------------------------
(defcomponent singleton-component-mixin ()
  ())

(defmethod component.instance-id ((self singleton-component-mixin))
  (or (slot-value self 'instance-id)
      (setf (slot-value self 'instance-id)
	    (format nil "~A"
		    (symbol-to-js
		     (string-replace-all
		      "/" "-"
		      (format nil "~A" (class-name (class-of self)))))))))

(defmethod/remote _destroy ((self singleton-component-mixin))
  nil)

(defmethod/remote destroy ((self singleton-component-mixin))
  (call-next-method self))

(defmethod/remote init ((self singleton-component-mixin))
  (call-next-method self))



;; (defmethod component+.ctor ((component component+))
;;   (labels ((remove-methods (names methods)
;; 	     (if (null names)
;; 		 methods
;; 		 (remove-methods (cdr names)
;; 				 (remove (car names) methods :key #'car)))))
;;     (let* ((class-name (class-name component))
;; 	   (class+ (find-class+ class-name))	 	 
;; 	   (local-methods (remove '_destroy (class+.local-methods class+)
;; 				  :key #'car))
;; 	   (remote-slots (mapcar (lambda (slot)
;; 				   (with-slotdef (name reader initarg) slot
;; 				     (list name
;; 					   (intern (symbol-name (gensym)))
;; 					   reader
;; 					   (intern (symbol-name initarg)))))
;; 				 (class+.remote-slots class+)))
;; 	   (dom-tag (any #'xml+.tag
;; 			 (reverse
;; 			  (filter (lambda (class)
;; 				    (if (typep class 'xml+) class))
;; 				  (cdr (class+.superclasses class+)))))))
    
;;       ;; --------------------------------------------------------------------
;;       ;; Component Internal Render Method 
;;       ;; --------------------------------------------------------------------
;;       `(defmethod %component! ((stream core-stream) (component ,class-name))
;; 	 (let ((component-instance-id (component.instance-id component))
;; 	       (server-session-id (if +context+
;; 				      (session.id (context.session +context+))
;; 				      "unbound-session-id"))
;; 	       ,@(mapcar (lambda (slot)
;; 			   `(,(cadr slot)
;; 			      (component.serialize-slot component
;; 							',(car slot))))
;; 			 remote-slots))
;; 	   (declare (ignorable component-instance-id server-session-id
;; 			       ,@(mapcar #'cadr remote-slots)))
;; 	   (with-js (component-instance-id server-session-id
;; 					   ,@(mapcar #'cadr remote-slots))
;; 	       stream
;; 	     ;; -------------------------------------------------------------
;; 	     ;; Constructor
;; 	     ;; -------------------------------------------------------------
;; 	     (with-call/cc
;; 	       (lambda (to-extend)
;; 		 (let ((to-extend (or to-extend (new (*object))))
;; 		       (slots
;; 			(jobject
;; 			 ;; ----------------------------------------------------------------------------
;; 			 ;; Remote Slot Initial Values
;; 			 ;; ----------------------------------------------------------------------------
;; 			 ,@(reduce0 (lambda (acc slot)
;; 				      (cond
;; 					((eq (car slot) 'class)
;; 					 (cons :class-name (cons (cadr slot) acc)))
;; 					((member (car slot) +omitted-component-slots+)
;; 					 acc)
;; 					(t
;; 					 (cons (make-keyword (car slot)) (cons (cadr slot) acc)))))
;; 				    remote-slots)))
;; 		       (methods
;; 			(jobject
;; 			 ;; ----------------------------------------------------------------------------
;; 			 ;; Remote Methods
;; 			 ;; ----------------------------------------------------------------------------
;; 			 ,@(reduce0 (lambda (acc method)
;; 				      (let* ((name (car method))
;; 					     (proxy (component+.morphism-function-name class+ name)))
;; 					(cons (make-keyword name)
;; 					      (cons `(make-method ,(funcall proxy class+)) acc))))
;; 				    (if (null local-methods)
;; 					(remove-methods '(funkall destroy init)
;; 							(class+.remote-methods class+))
;; 					(remove-methods '(destroy init)
;; 							(class+.remote-methods class+))))

		       
;; 			 ;; ----------------------------------------------------------------------------
;; 			 ;; Local Methods
;; 			 ;; ----------------------------------------------------------------------------
;; 			 ,@(reduce0 (lambda (acc method)
;; 				      (let* ((name (car method))
;; 					     (proxy (component+.morphism-function-name class+ name)))
;; 					(cons (make-keyword name)
;; 					      (cons (funcall proxy class+) acc))))
;; 				    (remove-methods '(_destroy) local-methods)))))

		     		     
;; 		   ;; -------------------------------------------------------------------------
;; 		   ;; Inject Methods to Instance
;; 		   ;; -------------------------------------------------------------------------
;; 		   (extend methods to-extend)

;; 		   ;; -------------------------------------------------------------------------
;; 		   ;; Inject Default Values Differentially
;; 		   ;; -------------------------------------------------------------------------
;; 		   (mapobject (lambda (k v)
;; 				(if (or (and (not (null v))
;; 					     (or (eq "" (slot-value to-extend k))
;; 						 (null (slot-value to-extend k))
;; 						 (eq "undefined" (slot-value to-extend k))))
;; 					(eq "undefined" (typeof (slot-value to-extend k))))
;; 				    (setf (slot-value to-extend k) v)))
;; 			      slots)

;; 		   (let ((to-extend ,(if dom-tag
;; 					 `(if (null (slot-value to-extend 'node-name))
;; 					      (extend to-extend
;; 						      (document.create-element
;; 						       ,(symbol-to-js dom-tag)))
;; 					      to-extend)
;; 					 'to-extend)))
;; 		     (setf (slot-value to-extend 'ctor)
;; 			   (slot-value arguments 'callee))
;; 		     (apply (make-method ,(funcall 'init/js class+)) to-extend null)
		     
;; 		     ,(if (null (remove-methods '(_destroy) local-methods))			  
;; 			  `(setf (slot-value to-extend 'destroy)
;; 				 (compose-prog1-cc (make-method ,(funcall 'destroy/js class+))
;; 						   (slot-value to-extend 'destroy)))
;; 			  `(setf (slot-value to-extend 'destroy)
;; 				 (compose-prog1-cc (make-method ,(funcall 'destroy/js class+))
;; 						   (slot-value to-extend 'destroy))
;; 				 (slot-value to-extend '_destroy)
;; 				 (make-method ,(funcall '_destroy/js class+))))
		 
;; 		     to-extend))))))))))