;; +-------------------------------------------------------------------------
;; | Component Framework (Lisp->Browser Functor)
;; +-------------------------------------------------------------------------
(in-package :core-server)

;; +-------------------------------------------------------------------------
;; | Component Metaclass
;; +-------------------------------------------------------------------------
(defclass+ component+ (class+)
  ((%ctor-timestamp :initform 0 :accessor component+.ctor-timestamp)
   (%cached-ctor-timestamp :initform 0 :accessor component+.cached-ctor-timestamp)
   (%stateful :initform nil :accessor component+.stateful)))

(defmethod component+.recompile-p ((self component+))
  "Predicate that tell that this component class needs recompilation"
  (> (slot-value self '%timestamp) (slot-value self '%ctor-timestamp)))

(defmethod component+.cached-recompile-p ((self component+))
  "Predicate that tell that this component class needs recompilation"
  (> (slot-value self '%timestamp) (slot-value self '%cached-ctor-timestamp)))

(defmethod xml+.tag ((self component+))
  (any #'xml+.tag
       (reverse (filter (lambda (class) (if (typep class 'xml+) class))
			(cdr (class+.superclasses self))))))
;; +-------------------------------------------------------------------------
;; | Component Class
;; +-------------------------------------------------------------------------
(defclass+ component ()
  ((session :host none :accessor component.session)
   (url :host remote :documentation "The url that this component is served.")
   (instance-id :host both :print t :documentation "Unique instance id")
   (session-id :host both :export nil :documentation "Session Id")
   (destroy-uri :host both :documentation "Destroy URL")
   (remote-slots :host remote)
   (object-cache :host none :initform nil))
  (:metaclass component+)
  (:documentation "Base component class"))

(defvar +markup-slots+ (make-html-attributes (list :core :i18n :event)))
(defmethod shared-initialize :after ((self component) slots &key &allow-other-keys)
  ;; Set Random DOM ID
  (with-slots (id remote-slots) self
    (if (and (typep self 'html-element)
	     (or (not (slot-boundp self 'id)) (null (slot-value self 'id))))
	(setf (slot-value self 'id) (random-string 5)))
    
    (if (null remote-slots)
	(let ((slots (mapcar #'slot-definition-name
			     (class+.remote-slots (class-of self)))))
	  (setf remote-slots
		(mapcar #'symbol-to-js
			(remove-if-member +markup-slots+ slots))))))
  self)

(defmethod component.object-cache ((self component))
  (with-slots (object-cache) self
    (if object-cache
	object-cache
	(setf object-cache (make-hash-table :test #'equal :synchronized t)))))

(defmethod component.instance-id ((self component))
  "Return, set the instance ID"
  (or (slot-value self 'instance-id)
      (setf (slot-value self 'instance-id)
	    (concat (symbol-to-js
		     (string-replace-all "/" "-"
					 (string (class-name (class-of self)))))
		    "-" (random-string 8)))))

(defmethod component.session-id ((self component))
  (session.id (component.session self)))

(defmethod component.application ((self component))
  "Returns application associated to this component."
  (if (component.session self)
      (session.application (component.session self))
      (session.application (context.session +context+))))

(defmethod component.action-hash ((self component) method)
  (format nil "~A-act~A" method (component.instance-id self)))

(defmethod component.find-local-method ((self component) (name symbol))
  "Returns the local method type signature"
  (find name (class+.local-methods (class-of self)) :key #'car))

(defmethod component.find-local-method ((self component) (name string))
  "Returns the local method type signature"
  (find (string-upcase name) (class+.local-methods (class-of self))
	:test #'string= :key #'car))

(defmethod/cc component.method-call ((self component) method-name args)
  "Dynamic application of method method-name w/ args to the component"  
  (let ((method (component.find-local-method self method-name)))
    (when (null method)
      (warn "Method (~A ~A) not found on instance: ~A"
	    method-name (class-name (class-of self))
	    (component.instance-id self))
      (return-from component.method-call nil))

    (flet ((find-argument (name)
	     (aif (find (symbol-to-js name) args :key #'car :test #'string=)
		  (component.deserialize self (json-deserialize (cdr it))))))
      (let* ((arguments (walk-lambda-list (cdddr method) nil nil
					  :allow-specializers t))
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
	(apply (car method) self arguments)))))

;; -------------------------------------------------------------------------
;; Serialization Interface
;; -------------------------------------------------------------------------
(defmethod component.serialize ((self component) (object t)) object)
(defmethod component.serialize ((self component) (object cons))
  (cons (component.serialize self (car object))
	(component.serialize self (cdr object))))

(defmethod component.serialize ((self component) (object list))
  (mapcar (curry #'component.serialize self) object))
(defmethod component.serialize-slot ((self component) (slot-name symbol))
  (component.serialize self (slot-value self slot-name)))
(defmethod component.deserialize ((self component) (object t)) object)

;; -------------------------------------------------------------------------
;; component/suspend
;; -------------------------------------------------------------------------
(defmacro component/suspend (lambda)
  `(javascript/suspend
    (lambda (stream)
      (let ((hash (json-deserialize
		   (http-request.query (context.request +context+) "__hash"))))
	(if hash
	    (with-js (hash ,@(find-free-variables (walk-form lambda))) stream
	      (with-call/cc
		(apply (slot-value window hash) window
		       (list ,lambda))))
	    (with-js (hash ,@(find-free-variables (walk-form lambda))) stream
	      (with-call/cc ,lambda)))))))

;; -------------------------------------------------------------------------
;; Remote & Local Morphism Interface
;; -------------------------------------------------------------------------
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
    (flet ((stub-predicate (arguments)
	     (let* ((args (filter (lambda (a)
				    (typep a 'specialized-function-argument-form))
				  arguments))
		    (specializers (mapcar #'arnesi::specializer args)))
	       (remove-if (curry #'eq t) specializers)))
	   (stub-arguments ()
	     (let ((arguments (walk-lambda-list args nil nil
						:allow-specializers t)))
	       (mapcar (lambda (arg)
			 (typecase arg
			   (specialized-function-argument-form
			    (setf (arnesi::specializer arg) t))))
		       arguments)
	       (unwalk-lambda-list arguments))))
      (let* ((class-name (class-name class))
	     (metaclass (class-name (class-of class)))
	     (morphism (component+.morphism-function-name class name))
	     (remote (component+.codomain-function-name class name))
	     (arguments (walk-lambda-list args nil nil :allow-specializers t))
	     (stub-p (stub-predicate arguments))	     
	     (arg-names (extract-argument-names args :allow-specializers t)))
	`(progn
	   (class+.add-method (find-class+ ',class-name) ',name 'local
			      '((,self ,class-name) ,@args))
	   (eval-when (:load-toplevel :compile-toplevel :execute)
	     (setf (gethash ',name +javascript-cps-functions+) t
		   (gethash ',remote +javascript-cps-functions+) t)
	     (defjsmacro ,name (&rest args) `(,',remote ,@args)))
	   (defmethod ,morphism ((self ,metaclass))
	     `(method ,',arg-names
		      (with-slots (session-id instance-id) self
			(funkall self (+ "?s:" session-id
					 "$k:" instance-id
					 "$method:" ,',(symbol-name name))
				 (create
				  ,@',(nreverse
				       (reduce0
					(lambda (acc arg)
					  (cons arg (cons (make-keyword arg) acc)))
					arg-names)))))))
	   ,(when stub-p
	     `(defmethod/cc ,name ((self ,class-name) ,@(stub-arguments))
		(error "Signature failed: (~A~{ ~A~})" ',name
		       (list
			,@(mapcar (compose #'car #'ensure-list) arg-names)))))
	   (defmethod/cc ,name ((,self ,class-name) ,@args)
	     (let ((application (component.application ,self)))
	       ,@body)))))))

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
	 (class+.add-method (find-class+ ',class-name) ',name 'remote
			    '((,self ,class-name) ,@args))
	 (eval-when (:load-toplevel :compile-toplevel :execute)
	   (setf (gethash ',name +javascript-cps-functions+) t
		 (gethash ',remote +javascript-cps-functions+) t)
	   (defjsmacro ,name (&rest args) `(,',remote ,@args)))
	 (defmethod ,functor ((self ,metaclass))
	   ,(if call-next-method-p
		`(let ((next-method (call-next-method self)))
		   `(method ,',args
			    (let ((call-next-method
				   (lambda (,',self ,@(cadr next-method))
				     ,@(cddr next-method))))
			      ,@',body)))
		``(method ,',args
			  (let ((,',self self))
			    ,@',body))))
	 (defmethod/cc ,name ((,self ,class-name) ,@args)
	   (let (,@(mapcar (lambda (arg)
			     `(,arg (component.serialize ,self ,arg)))
			   (extract-argument-names args
						   :allow-specializers t))
		 (result (action/url ((result "result"))
			   (context.remove-current-action +context+)
			   (answer (component.deserialize ,self result)))))
	     (component/suspend
	       (lambda (self)
		 (funkall self result
			  (create :result (,name self ,@args)))))))))))

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
		   (export (caddr slot))
		   (accessor (cadr slot))
		   (macro1 `(defmacro/js ,accessor (self)
			      `(slot-value ,self ',',name)))
		   (macro2 `(defsetf/js ,accessor (value self)
			      `(setf (slot-value ,self ',',name) ,value))))
	      (cond
		((not export) acc)
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
	 (metaclass (intern (format nil "~A+" name) (symbol-package name)))
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

(defmethod/cc component.register ((component component) (context http-context))
  (let ((+context+ context)
	(k (context.continuation context))
	(+action-hash-override+ (component.instance-id component)))
    (action/url ((method-name "method"))
      (let* ((args (uri.queries (http-request.uri (context.request +context+))))
	     (result (component.serialize component
		       (component.method-call component method-name args))))
	(kall k context
	      (component/suspend (lambda (self) result)))))))

(defmethod component.compile ((component component))
  (let* ((class (class-of component)))
    (format *standard-output* "Compiling constructor for ~A.~%" (class-name class))
    (eval (component.ctor component))
    (setf (slot-value class '%ctor-timestamp) (get-universal-time))))

;; (component-instance-id (component.instance-id component))
;; (server-session-id (session.id component))
;; (component-destroy-uri (if +context+
;; 			   (format nil "~Adestroy.core"
;; 				   (web-application.base-url
;; 				    (component.application component)))
;; 			   "TEST-COMPONENT-DESTROY.core"))
(defmethod/cc component.update ((self component) (context http-context))
  (with-slots (url session session-id destroy-uri) self
    (setf session (context.session context)
	  session-id (session.id session)
	  url (web-application.serve-url (context.application context)
					 (context.request context))
	  destroy-uri (format nil "~A/destroy.core"
			      (web-application.base-url
			       (context.application context))))))

(defmethod/cc component! ((stream core-stream) (component component))
  (when (and +context+ (component+.stateful-p (class-of component)))
    (component.update component +context+)
    (component.register component +context+))

  (%component! stream component))

;; --------------------------------------------------------------------------
;; This around method allows us to compile constructor evertime class
;; definition changed.
;; --------------------------------------------------------------------------
(defmethod/cc component! :around ((stream core-stream) (component component))
  (if (component+.recompile-p (class-of component))      
      (component.compile component))
  
  (call-next-method stream component))

(defvar +omitted-component-slots+
  '(style onmouseup onmousemove onclick className ondblclick
    onmousedown onkeypress onkeydown onkeyup dir lang dojoType open
    onmouseover onmouseout onfocus onchange onselect onblur))

(defmethod component+.remote-slots ((self component+))
  (mapcar (lambda (slot)
	    (with-slotdef (name) slot
	      (if (eq name 'class)
		  (list name :class-name (intern (symbol-name (gensym))))
		  (list name (make-keyword name) (intern (symbol-name (gensym)))))))
	  (remove-if-member +omitted-component-slots+ (class+.remote-slots self)
			    :key #'slot-definition-name)))

(defun %remove-methods (members methods)
  (remove-if-member members methods :key #'car))

(defmethod component+.local-methods ((self component+))
  (let ((methods (class+.local-methods self)))
    (reduce0 (lambda (acc method)
	       (let* ((name (car method))
		      (proxy (component+.morphism-function-name self name)))
		 (cons (make-keyword name)
		       (cons `(make-method ,(funcall proxy self)) acc))))
	     methods)))

(defmethod component+.stateful-p ((self component+))
  (not (null (class+.local-methods self))))

(defmethod component+.remote-methods ((self component+))
  (let* ((methods (class+.remote-methods self))
	 (to-remove (if (component+.stateful-p self)
			'(destroy init)
			'(funkall destroy init)))
	 (methods (%remove-methods to-remove methods)))
    (reduce (lambda (acc method)
	      (let* ((name (car method))
		     (proxy (component+.morphism-function-name self name)))
		(cons (make-keyword name)
		      (cons `(make-method ,(funcall proxy self)) acc))))
	    methods :initial-value nil)))

(defmethod component+.init-method ((self component+))
  (funcall (component+.morphism-function-name self 'init) self))

(defmethod component+.destroy-method ((self component+))
  (funcall (component+.morphism-function-name self 'destroy) self))

(defmethod component.ctor ((component component))
  (let* ((class+ (class-of component)) 
	 (local-methods (component+.local-methods class+))
	 (remote-methods (component+.remote-methods class+))
	 (remote-slots (component+.remote-slots class+))
	 (dom-tag (aif (xml+.tag class+) (symbol-to-js it))))
    
    ;; --------------------------------------------------------------------
    ;; Component Internal Render Method 
    ;; --------------------------------------------------------------------
    `(defmethod %component! ((stream core-stream) (component ,(class-name class+)))
       (let ,(mapcar (lambda (slot)
		       `(,(caddr slot) (component.serialize-slot component ',(car slot))))
		     remote-slots)
	 (declare (ignorable ,@(mapcar #'caddr remote-slots)))
	 (with-js ,(mapcar #'caddr remote-slots) stream
	   ;; -------------------------------------------------------------
	   ;; Constructor
	   ;; -------------------------------------------------------------
	   (with-call/cc
	     (lambda (to-extend)
	       ;; ----------------------------------------------------------------------------
	       ;; Remote Slot Initial Values & Remote Methods & Local Methods
	       ;; ----------------------------------------------------------------------------
	       (let ((slots (jobject ,@(flatten (mapcar #'cdr remote-slots))))
		     (methods (jobject ,@remote-methods ,@local-methods))
		     (to-extend (make-object-to-extend ,dom-tag to-extend)))
		 
		 ;; Inject Methods & Default Slot Values to Instance
		 (extend methods to-extend)
		 (mapobject (lambda (k v)
			      (if (or (and (not (null v)) (null-p (slot-value to-extend k)))
				      (eq "undefined" (typeof (slot-value to-extend k))))
				  (setf (slot-value to-extend k) v)))
			    slots)

		 (setf (slot-value to-extend 'ctor) (slot-value arguments 'callee))
		 ;; Call Initialization
		 (apply (make-method ,(component+.init-method class+)) to-extend null)

		 ;; Set Destruction Methods
		 (setf (slot-value to-extend 'destroy)
		       (compose-prog1-cc (make-method ,(component+.destroy-method class+))
					 (slot-value to-extend 'destroy))) 
		 
		 to-extend))))))))
			     
;; -------------------------------------------------------------------------
;; Cached Component Class
;; -------------------------------------------------------------------------
(defcomponent cached-component (component)
  ())

(defmethod component.compile ((component cached-component))
  (let* ((class (class-of component)))
    (format *standard-output* "Compiling cached constructor for ~A.~%"
	    (class-name class))
    (eval (component.ctor component))
    (setf (slot-value class '%ctor-timestamp) (get-universal-time))))

(defmethod component.ctor ((component cached-component))
  (let* ((class+ (class-of component))
	 (class-name (class-name class+))
	 (local-methods (component+.local-methods class+))
	 (remote-slots (component+.remote-slots class+))
	 (dom-tag (aif (xml+.tag class+) (symbol-to-js it))))
    
    ;; --------------------------------------------------------------------
    ;; Component Internal Render Method 
    ;; --------------------------------------------------------------------
    `(defmethod %component! ((stream core-stream) (component ,class-name))
       (let ((class-name ,(string class-name)))
	 (let ((loader-uri (if +context+
			       (format nil "~A/component.core"
				       (web-application.base-url
					(context.application +context+)))))
	       (package-name (package-name (symbol-package ',class-name))) 
	       ,@(mapcar (lambda (slot)
			   `(,(caddr slot) (component.serialize-slot component ',(car slot))))
			 remote-slots))
	   (declare (ignorable ,@(mapcar #'caddr remote-slots)))
	   (with-js (loader-uri class-name package-name ,@(mapcar #'caddr remote-slots)) stream
	     ;; -------------------------------------------------------------
	     ;; Constructor
	     ;; -------------------------------------------------------------
	     (with-call/cc
	       (lambda (to-extend)
		 ;; ----------------------------------------------------------------------------
		 ;; Remote Methods
		 ;; ----------------------------------------------------------------------------
		 (let ((slots (jobject ,@(flatten (mapcar #'cdr remote-slots))))
		       (methods (jobject ,@local-methods))
		       (to-extend (make-object-to-extend ,dom-tag to-extend)))
		 
		   ;; Inject Methods & Default Slot Values to Instance
		   (extend methods to-extend)
		   (mapobject (lambda (k v)
				(if (or (and (not (null v)) (null-p (slot-value to-extend k)))
					(eq "undefined" (typeof (slot-value to-extend k))))
				    (setf (slot-value to-extend k) v)))
			      slots)

		   (setf (slot-value to-extend 'ctor) (slot-value arguments 'callee))
		   ;; Set Destruction Methods

		   (with-slots (component-cache) window
		     (aif (slot-value component-cache class-name)
			  (call/cc it to-extend)
			  (let ((ctor (funcall-cc (+ loader-uri "?")
						  (jobject :component class-name
							   :package package-name
							   :__hash (+ "__" class-name)))))
			    (setf (slot-value component-cache class-name) ctor)
			    (apply ctor to-extend (list to-extend)))))
		 
		   to-extend)))))))))

(defmethod component+.ctor ((class+ component+))
  (let* ((class+-name (class-name (class-of class+))) 
	 (remote-methods (component+.remote-methods class+)) 
	 (dom-tag (aif (xml+.tag class+) (symbol-to-js it))))
    
    ;; --------------------------------------------------------------------
    ;; Component+ Internal Render Method 
    ;; --------------------------------------------------------------------
    `(defmethod %component+! ((stream core-stream) (component ,class+-name))
       (with-js () stream
	 ;; -------------------------------------------------------------
	 ;; Constructor
	 ;; -------------------------------------------------------------
	 (with-call/cc
	   (lambda (to-extend)
	     ;; ----------------------------------------------------------------------------
	     ;; Remote Slot Initial Values & Remote Methods & Local Methods
	     ;; ----------------------------------------------------------------------------
	     (let ((methods (jobject ,@remote-methods))
		   (to-extend (make-object-to-extend ,dom-tag to-extend)))
		 
	       ;; Inject Methods & Default Slot Values to Instance
	       (extend methods to-extend)

	       ;; Call Initialization
	       (apply (make-method ,(component+.init-method class+)) to-extend null)

	       ;; Set Destruction Methods
	       (setf (slot-value to-extend 'destroy)
		     (compose-prog1-cc (make-method ,(component+.destroy-method class+))
				       (slot-value to-extend 'destroy)))
		 
	       to-extend)))))))

(defmethod component+.compile ((component+ component+))
  (format *standard-output* "Compiling cached constructor for ~A.~%"
	  (class-name component+))
  (eval (component+.ctor component+))
  (setf (slot-value component+ '%cached-ctor-timestamp) (get-universal-time)))

(defmethod %component+! ((stream core-stream) (component+ component+))
  (error "Should not see this."))

(defmethod %component+! :around ((stream core-stream) (component+ component+))
  (cond
    ((component+.cached-recompile-p component+)
     (component+.compile component+)
     (%component+! stream component+))
    (t (call-next-method stream component+))))

;; -------------------------------------------------------------------------
;; Component Loader
;; -------------------------------------------------------------------------
(defun find-component-class (name package)
  (let ((package (or (find-package (make-keyword package))
		     (find-package :core-server))))
    (any (lambda (component)
	   (let ((class-name (class-name component)))
	     (if (and (eq (symbol-package class-name) package)
		      (string= name class-name))
		 component)))
	    (class+.subclasses (find-class 'component)))))

(defhandler "component\.core" ((self http-application) (component "component")
			       (package "package"))
  (let ((class (find-component-class (json-deserialize component)
				     (json-deserialize package))))
    (assert (not (null class)))
    (assert (not (null component)))
    (flet ((foo () (slot-value class '%cached-ctor-timestamp)))
      (with-cache (foo) (component/suspend class)))))

(defhandler "destroy.core" ((self http-application) (objects "objects")
			    (hash "__hash"))
  (mapcar (lambda (object) (context.remove-action +context+ object))
	  (ensure-list (json-deserialize objects)))
  (component/suspend nil))

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
(defmethod/remote destroy ((self component))
  (with-slots (destroy-uri session-id instance-id remote-slots) self
    (if destroy-uri
	(add-to-gc (lambda () (array (+ destroy-uri "?s:" session-id) instance-id))))
    (if remote-slots
    	(remove-slots self (reverse (cons "destroy" remote-slots)))
    	(delete-slot self 'destroy))
    self))

(defmethod/remote client-destroy ((self component))
  (setf (slot-value self 'destroy-ur) nil)
  (destroy self))

(defmethod/cc destroy ((self component))
  ;; (describe (list 'destroy self))
  (context.remove-action +context+ (component.instance-id self)))

(defmethod/cc call-component ((component component))
  (component/suspend (lambda (self) component)))

(defmethod/cc answer-component ((self component) arg)
  (answer arg))

(defmethod/cc continue-component ((component component) &optional value)
  (component/suspend (lambda (self) value)))

(defun/cc continue/js (value)
  (javascript/suspend
   (lambda (stream)
     (let ((hash (json-deserialize
		  (http-request.query
		   (context.request +context+) "__hash"))))
       (cond
	 (hash
	  (if (typep value 'arnesi::closure/cc)
	      (with-js (value hash) stream
		(with-call/cc
		  (apply (slot-value window hash) window (list value))))
	      (with-js (value hash) stream
		(with-call/cc
		  (apply (slot-value window hash) window
			 (list (lambda (self) value)))))))
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

(defmethod write-stream ((stream html-stream) (self component))
  (prog1 (call-next-method stream self)
    (when (typep self 'xml)
      (write-stream stream
		    (<:script :type "text/javascript"
			      (with-call/cc		
				(lambda (stream)
				  (let ((stream (make-indented-stream stream))
					(component self)
					(id (slot-value self 'id)))
				    (with-js (id component) stream
				      ((lambda ()
					 (let ((ctor component))
					   (ctor (document.get-element-by-id id)
						 window.k)))))))))))))

;; -------------------------------------------------------------------------
;; JSON Writer Extensions
;; -------------------------------------------------------------------------
(defmethod json! ((stream core-stream) (element component))
  (prog1 stream (with-call/cc (component! stream element))))

(defmethod json! ((stream core-stream) (element component+))
  (%component+! stream element))

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
  (component/suspend (lambda (self) component)))

;; -------------------------------------------------------------------------
;; Singleton Component Mixin
;; -------------------------------------------------------------------------
(defcomponent singleton-component-mixin ()
  ())

(defmethod component.instance-id ((self singleton-component-mixin))
  (or (slot-value self 'instance-id)
      (setf (slot-value self 'instance-id)
	    (symbol-to-js
	     (string-replace-all "/" "-"
				 (string (class-name (class-of self))))))))

;; -------------------------------------------------------------------------
;; Remote Reference Class
;; -------------------------------------------------------------------------
(defcomponent remote-reference (component)
  ((_reference-p :host remote :initform t)))

(defmethod component.serialize ((self component) (object remote-reference))
  (let ((cache (component.object-cache self)
	  ;; (session.data (component.session self))
	  )
	(instance-id (component.instance-id object)))
    (if (null (gethash instance-id cache))
	(setf (gethash instance-id cache) object)))
  object)

(defmethod component.deserialize ((self component) (object jobject))
  (aif (get-attribute object :_reference)
       (aif (gethash it (component.object-cache self))
	    it
	    (error "Cannot find reference to ~A"
		   (get-attribute object :_reference)))
       (call-next-method self object)))

;; ;; FIXME: -evrim.
;; (defmethod component.serialize-slot ((self component) slot-name)
;;   (slot-value self slot-name)
;;   ;; (let ((slot (class+.find-slot (class-of self) slot-name)))
;;   ;;   (with-slotdef (reader) slot
;;   ;;     (funcall reader self)))
;;   )




;; -------------------------------------------------------------------------
;; Garbage below
;; -------------------------------------------------------------------------
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

;; (defmethod shared-initialize :after ((self component) slots &key &allow-other-keys)
;;   (if (member 'id (mapcar #'slot-definition-name (class+.slots (class-of self))))
;;       (if (or (not (slot-boundp self 'id)) (null (slot-value self 'id)))
;; 	  (setf (slot-value self 'id) (random-string 5))))
  
;;   ;; (if (typep self 'xml)
;;   ;;     (setf (xml.children self)
;;   ;; 	    (cons 
;;   ;; 	     (<:script :type "text/javascript"
;;   ;; 		       (with-call/cc		
;;   ;; 			 (lambda (stream)
;;   ;; 			   (let ((stream (make-indented-stream stream))
;;   ;; 				 (component self)
;;   ;; 				 (id (slot-value self 'id)))
;;   ;; 			     (with-js (component id) stream
;;   ;; 			       ((lambda ()
;;   ;; 				  (let ((ctor component))
;;   ;; 				    (ctor (document.get-element-by-id id) window.k)))))))))
;;   ;; 	     (xml.children self))))
;;   self)


;; (defmethod %component! ((stream core-stream) (component+ component+))
;;   (error "Thou shall not!")
;;   ;; (if  (> (slot-value component+ '%timestamp)
;;   ;; 	  (slot-value component+ '%ctor-timestamp))      
;;   ;;      (let ((name (class-name component+)))
;;   ;; 	 (format *standard-output* "Compiling constructor for ~A.~%" name)
;;   ;; 	 (eval (component+.ctor2 component+))
;;   ;; 	 (setf (slot-value component+ 'core-server::%ctor-timestamp)
;;   ;; 	       (get-universal-time))
;;   ;; 	 (%component! stream component+))
;;   ;;      (%component! stream component+))
;;   )


;; (defmethod component+.ctor-for-cached-component ((component component+))
;;   (labels ((remove-methods (names methods)
;; 	     (if (null names)
;; 		 methods
;; 		 (remove-methods (cdr names)
;; 				 (remove (car names) methods :key #'car)))))
;;     (let* ((class-name (class-name component))
;; 	   (class+-name (class-name (class-of component)))
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
;;       `(progn
;; 	 (defmethod %component! ((stream core-stream) (component ,class-name))
;; 	   (let ((component-instance-id (component.instance-id component))
;; 		 (server-session-id (if +context+
;; 					(session.id (context.session +context+))
;; 					"unbound-session-id"))
;; 		 (class-name (string (class-name (class-of component))))
;; 		 (component-loader-uri (if +context+
;; 					   (format nil "~Acomponent/"
;; 						   (web-application.base-url
;; 						    (component.application component)))
;; 					   "TEST-COMPONENT-LOADER.core"))
;; 		 (component-destroy-uri (if +context+
;; 					    (format nil "~Adestroy.core"
;; 						    (web-application.base-url
;; 						     (component.application component)))
;; 					    "TEST-COMPONENT-DESTROY.core"))
;; 		 ,@(mapcar (lambda (slot)
;; 			     `(,(cadr slot)
;; 				(component.serialize-slot component
;; 							  ',(car slot))))
;; 			   remote-slots))
;; 	     (declare (ignorable component-instance-id server-session-id
;; 				 component-destroy-uri component-loader-uri
;; 				 ,@(mapcar #'cadr remote-slots)))
;; 	     (with-js (component-instance-id server-session-id class-name
;; 					     component-destroy-uri
;; 					     component-loader-uri
;; 					     ,@(mapcar #'cadr remote-slots))
;; 		 stream
;; 	       ;; -------------------------------------------------------------
;; 	       ;; Constructor
;; 	       ;; -------------------------------------------------------------
;; 	       (with-call/cc
;; 		 (lambda (to-extend)
;; 		   (let ((args arguments)
;; 			 (to-extend (or to-extend (new (*object))))
;; 			 (slots
;; 			  (jobject
;; 			   ;; ----------------------------------------------------------------------------
;; 			   ;; Remote Slot Initial Values
;; 			   ;; ----------------------------------------------------------------------------
;; 			   ,@(reduce0 (lambda (acc slot)
;; 					(cond
;; 					  ((eq (car slot) 'class)
;; 					   (cons :class-name (cons (cadr slot) acc)))
;; 					  ((member (car slot) +omitted-component-slots+)
;; 					   acc)
;; 					  (t
;; 					   (cons (make-keyword (car slot)) (cons (cadr slot) acc)))))
;; 				      remote-slots)))
;; 			 (methods
;; 			  (jobject
;; 			   ;; ----------------------------------------------------------------------------
;; 			   ;; Local Methods
;; 			   ;; ----------------------------------------------------------------------------
;; 			   ,@(reduce0 (lambda (acc method)
;; 					(let* ((name (car method))
;; 					       (proxy (component+.morphism-function-name class+ name)))
;; 					  (cons (make-keyword name)
;; 						(cons (funcall proxy class+) acc))))
;; 				      (remove-methods '(_destroy) local-methods)))))

;; 		     ;; -------------------------------------------------------------------------
;; 		     ;; Inject Methods to Instance
;; 		     ;; -------------------------------------------------------------------------
;; 		     (extend methods to-extend)

;; 		     ;; -------------------------------------------------------------------------
;; 		     ;; Inject Default Values Differentially
;; 		     ;; -------------------------------------------------------------------------
;; 		     (mapobject (lambda (k v)
;; 				  (if (or (and (not (null v))
;; 				      	       (null-p (slot-value to-extend k))
;; 					       ;; (or (eq "" (slot-value to-extend k))
;; 				      	       ;; 	   (null (slot-value to-extend k))
;; 				      	       ;; 	   (eq "undefined" (slot-value to-extend k)))
;; 					       )
;; 				      	  (eq "undefined" (typeof (slot-value to-extend k))))
;; 				   ;; (and (not (null v)) (not (nullp (slot-value to-extend k))))
;; 				   (setf (slot-value to-extend k) v)))
;; 				slots)

;; 		     ,(if local-methods
;; 		     	  `(setf (slot-value to-extend '_destroy)
;; 		     		 (make-method ,(funcall '_destroy/js class+))))

;; 		     ;; Save Constructor
;; 		     (setf (slot-value to-extend 'ctor) (slot-value args 'callee))

;; 		     (with-slots (component-cache) window		       
;; 		       (aif (slot-value component-cache class-name)
;; 			    (call/cc it to-extend)
;; 			    (let ((ctor (funcall-cc
;; 					 (+ component-loader-uri
;; 					    (.replace class-name "/" "XXYZ")
;; 					    ".core?"
;; 					    ;; "?s=" server-session-id "$"
;; 					    ;; "?"
;; 					    "component=" (encode-u-r-i-component class-name)
;; 					    "&")
;; 					 (jobject :__hash (+ "__" class-name)))))
;; 			      (setf (slot-value component-cache class-name) ctor)
;; 			      (apply ctor to-extend (list to-extend)))))))))))
	 
;; 	 (defmethod %component! ((stream core-stream) (component ,class+-name))
;; 	   (let ((component-instance-id "unbound-instance-id")
;; 		 (server-session-id (if +context+
;; 					(session.id (context.session +context+))
;; 					"unbound-session-id")))
;; 	     (declare (ignorable component-instance-id server-session-id))
;; 	     (with-js (component-instance-id server-session-id)
;; 		 stream
;; 	       ;; -------------------------------------------------------------
;; 	       ;; Constructor
;; 	       ;; -------------------------------------------------------------
;; 	       (with-call/cc
;; 		 (lambda (to-extend)
;; 		   (let ((to-extend (or to-extend (new (*object))))
;; 			 (methods
;; 			  (jobject
;; 			   ;; ----------------------------------------------------------------------------
;; 			   ;; Remote Methods
;; 			   ;; ----------------------------------------------------------------------------
;; 			   ,@(reduce0 (lambda (acc method)
;; 					(let* ((name (car method))
;; 					       (proxy (component+.morphism-function-name class+ name)))
;; 					  (cons (make-keyword name)
;; 						(cons `(make-method ,(funcall proxy class+)) acc))))
;; 				      (if (null local-methods)
;; 					  (remove-methods '(funkall destroy _destroy init)
;; 							  (class+.remote-methods class+))
;; 					  (remove-methods '(destroy _destroy init)
;; 							  (class+.remote-methods class+)))))))
		     		     
;; 		     ;; -------------------------------------------------------------------------
;; 		     ;; Inject Methods to Instance
;; 		     ;; -------------------------------------------------------------------------
;; 		     (extend methods to-extend)
;; 		     (setf (slot-value to-extend 'destroy)
;; 			   (compose-prog1-cc (make-method ,(funcall 'destroy/js class+))
;; 					     (slot-value to-extend 'destroy)))
		     

;; 		     (let ((to-extend ,(if dom-tag
;; 					   `(if (null (slot-value to-extend 'node-name))
;; 						(extend to-extend
;; 							(document.create-element
;; 							 ,(symbol-to-js dom-tag)))
;; 						to-extend)
;; 					   'to-extend)))		       
;; 		       (apply (make-method ,(funcall 'init/js class+)) to-extend null)		     		       
;; 		       to-extend)))))))))))


;; old local morphism
;; (with-query ((hash "__hash")) (context.request +context+)
;;   (let ((hash (json-deserialize hash)))
;;     (javascript/suspend
;;      (lambda (stream)
;;        (let ((result (action/url ((result "result"))
;; 		       (context.remove-current-action +context+)
;; 		       (answer (component.deserialize ,self result)))))
;; 	 (if hash
;; 	     (with-js (result hash ,@args) stream
;; 	       (with-call/cc
;; 		 (apply (slot-value window hash) window
;; 			(list (lambda (self)
;; 				(funkall self result
;; 					 (create :result (,name self ,@args))))))))
;; 	     (with-js (result ,@args) stream
;; 	       (with-call/cc
;; 		 (lambda (self)
;; 		   (funkall self result
;; 			    (create :result (,name self ,@args))))))))))))
