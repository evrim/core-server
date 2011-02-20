(in-package :core-server)

;; -------------------------------------------------------------------------
;; Table Component (Single Select)
;; -------------------------------------------------------------------------
(defcomponent <core:table (<:table)
  ((instances :host remote :initarg :instances :initform nil)
   (primary-field :host remote :initform "name")
   (template-class :initform nil :host remote)
   (hilight-class :initform "hilighted" :host remote)
   (selected-class :initform "selected" :host remote)
   (selected :initform nil :host remote)
   (_sorted-slot :host remote)))

(defmethod/remote get-template-class ((self <core:table))
  (let ((_instances (instances self)))
    (or (slot-value self 'template-class)
	(and _instances (slot-value (car _instances) 'core-class)))))

(defmethod/remote column-span ((self <core:table))
  (call/cc
   (event (c)
     (apply self.get-template-class self
	    (list
	     (lambda (_class)		      
	       (let ((a 1))
		 (mapobject (lambda (k v) (setq a (+ 1 a))) _class)
		 (c a))))))))

(defmethod/remote sort-table ((self <core:table) slot)
  (with-slots (name) slot
    (cond
      ((and (_sorted-slot self) (eq slot (_sorted-slot self)))
       (setf (instances self)
	     (sort (lambda (a b) (> (slot-value a name) (slot-value b name)))
		   (instances self)))
       (setf (_sorted-slot self) nil))
      (t
       (setf (instances self)
	     (sort (lambda (a b) (< (slot-value a name) (slot-value b name)))
		   (instances self)))
       (setf (_sorted-slot self) slot)))

    (replace-node (aref (self.get-elements-by-tag-name "TBODY") 0)
		  (tbody self))
    
    ;; Give emphasis on selected again
    (awhen (selected self)
      (awhen (find (lambda (a) (eq a it)) (instances self))
	(_on-select self it)))))

(defmethod/remote _on-select ((self <core:table) object)
  (flet ((parent (node) (slot-value node 'parent-node)))
    (mapcar-cc (lambda (object)
		 (let* ((_radio (slot-value object 'radio))
			(_parent (call/cc parent (call/cc parent _radio))))
		   (remove-class _parent (selected-class self))))
	       (instances self))
    (add-class (call/cc parent (call/cc parent (slot-value object 'radio)))
	       (selected-class self))
    (setf (slot-value (slot-value object 'radio) 'checked) t
	  (selected self) object)))

(defmethod/remote on-select ((self <core:table) object)
  (_on-select self object)
  (answer-component self object))

(defmethod/remote add-instance ((self <core:table) instance)
  (let ((tbody (aref (.get-elements-by-tag-name self "TBODY") 0))
	(radio (<:input :type "radio"
			:name "table-object"
			:onclick (event (e)
				   (with-call/cc
				     (on-select self instance))
				   true)))
	(_instances (instances self)))
    
    (setf (slot-value instance 'radio) radio)

    (if (null _instances)
	(let ((tbody (aref (.get-elements-by-tag-name self "TBODY") 0)))
	  (.remove-child tbody (slot-value tbody 'first-child))))
    
    (setf (instances self) (cons instance (instances self)))
    (replace-node (aref (.get-elements-by-tag-name self "TFOOT") 0)
		  (tfoot self))

    (append tbody
	    (<:tr :class (if (eq 1 (mod (slot-value (instances self) 'length)
					2))
			     (hilight-class self)
			     "")
	     (cons
	      (<:td :class "radio" radio)	       
	      (reverse
	       (mapcar
		(lambda (slot)
		  (with-slots (name label initform reader) slot
		    (let ((value (if reader
				     (reader instance)
				     (slot-value instance name))))
		      (<:td :class name
			    (or value (slot-value slot 'initform))))))
		(template-class self))))))))

(defmethod/remote remove-instance ((self <core:table) instance)
  (let ((_instances (instances self)))    
    (setf (instances self)
	  (filter-cc (lambda (a) (not (eq instance a))) _instances)))

  (awhen (slot-value instance 'radio)
    (labels ((find-parent-tr (node)
	       (cond
		 ((null node)
		  nil)
		 ((eq "tr" (.to-lower-case (slot-value node 'tag-name)))
		  node)
		 (t
		  (call/cc find-parent-tr (slot-value node 'parent-node))))))
      (awhen (find-parent-tr it)
	(.remove-child (slot-value it 'parent-node) it))))

  (delete-slot instance 'radio)
  (if (eq (selected self) instance) (setf (selected self) nil))
  (replace-node (aref (.get-elements-by-tag-name self "TFOOT") 0)
		(tfoot self))

  (if (null (instances self))
      (replace-node (aref (.get-elements-by-tag-name self "TBODY") 0)
		    (tbody self)))
  instance)

(defmethod/remote thead ((self <core:table))
  (<:thead
   (<:tr
    (<:th :class "radio" " ")
    (reverse
     (mapcar (lambda (slot)
	       (with-slots (name label) slot
		 (<:th :class name
		       (<:a :onclick (lifte (sort-table self slot))
			    (or label name)))))
	     (template-class self))))))

(defmethod/remote tbody ((self <core:table))
  (let ((_instances (instances self)))
    (if (null _instances)
	(<:tbody (<:tr (<:td :colspan (column-span self)
			     "Table has no elements.")))
	(<:tbody
	 (mapcar2-cc
	  (lambda (object index)
	    (let ((radio
		   (<:input :type "radio"
			    :name "table-object"
			    :onclick (event (e)
				      (with-call/cc (on-select self object))
				      true))))
	      (setf (slot-value object 'radio) radio)
	      (<:tr :class (if (eq 0 (mod index 2)) (hilight-class self))
		    (cons
		     (<:td :class "radio" radio)	       
		     (reverse
		      (mapcar
		       (lambda (slot)
			 (with-slots (name label initform reader) slot
			   (let ((value (if reader
					    (reader object)
					    (slot-value object name))))
			     (<:td :class name
				   (or value (slot-value slot 'initform))))))
		       (template-class self)))))))
	  _instances (seq (slot-value _instances 'length)))))))

(defmethod/remote tfoot ((self <core:table))
  (if (instances self)
      (<:tfoot
       (<:tr
	(<:td :class "text-right"
	      :colspan (column-span self)
	      (+ (slot-value (instances self) 'length) " item(s)."))))
      (<:tfoot)))

(defmethod/remote init ((self <core:table))
  (add-class self "table")
  (load-css "http://www.coretal.net/style/table.css")
  (mapcar-cc (lambda (a) (.remove-child self a))
	     (slot-value self 'child-nodes))
  (mapcar-cc (lambda (i) (append self i))
	     (list (thead self) (tbody self) (tfoot self))))

(defmacro deftable (name supers slots &rest rest)
  `(progn
     (defcomponent ,name (,@supers <core:table)
       ()
       (:default-initargs
	 :template-class
	   (jobject
	    ,@(reduce
	       (lambda (acc slot)			
		 (append acc
			 `(,(make-keyword (car slot))
			    (core-server::jobject
			     :name ',(symbol-to-js (car slot)) ,@(cdr slot)))))
	       (reverse slots) :initial-value nil))
	 ,@(cdr (flatten1 rest))))))

;; +----------------------------------------------------------------------------
;; | Table Component
;; +----------------------------------------------------------------------------
;; (defcomponent <core:table (<:table)
;;   ((instances :host remote :initarg :instances :initform nil)
;;    (hilight-class :initform "hilighted" :host remote)
;;    (selected-class :initform "selected" :host remote)
;;    (selected :initform nil :host remote)
;;    (primary-field :initform "name" :host remote)
;;    (template-class :initform nil :host none)
;;    (local-cache :initform (make-hash-table :weakness :value :test #'equal)
;; 		:host none)))

;; (defmethod/local get-instances ((self <core:table))
;;   (mapcar (lambda (jobject instance)
;; 	    (describe jobject)
;; 	    (setf (slot-value jobject 'attributes)
;; 		  (cons :ref-id
;; 			(cons (let ((str (random-string 5)))
;; 				(setf (gethash str (local-cache self)) instance)
;; 				str)
;; 			      (slot-value jobject 'attributes))))
;; 	    jobject)
;; 	  (mapcar (rcurry #'object->jobject
;; 			  (or (template-class self)
;; 			      (class-of (car (instances self)))))
;; 		  (instances self))
;; 	  (instances self)))

;; (defmethod/remote add-selected ((component <core:table) selection)
;;   (let ((node (slot-value (slot-value (slot-value selection 'checkbox) 'parent-node) 'parent-node)))
;;     (add-class node (selected-class self))
;;     (remove-class node (hilight-class self))
;;     (set-selected component (cons selection
;; 				  (remove-selected component selection)))))


;; (defmethod/remote remove-selected ((component <core:table) selection)
;;   (let ((node (slot-value (slot-value (slot-value selection 'checkbox) 'parent-node) 'parent-node)))
;;     (remove-class node (selected-class component))
;;     (set-selected component (filter (lambda (a) (not (eq a selection)))
;; 				    (get-selected component)))))

;; (defmethod/remote thead ((component <core:table))
;;   (<:thead
;;    (<:tr
;;     (<:th :class "checkbox"
;; 	  (<:input :type "checkbox"
;; 		   :onchange (event (e)
;; 			       (let ((checked this.checked))
;; 				 (with-call/cc
;; 				   (mapcar (lambda (object)
;; 					     (setf (slot-value (slot-value object 'checkbox) 'checked)
;; 						   checked))
;; 					   (get-instances component))
;; 				   (if checked
;; 				       (set-selected component (get-instances component))
;; 				       (set-selected component nil))))
;; 			       false)))
;;     (mapcar (lambda (slot)
;; 	      (with-slots (name label) slot
;; 		(<:th :class name (or label name))))
;; 	    (slot-value (car (instances self)) 'class)))))

;; (defmethod/local _set-selected ((self <core:table) ref-id)
;;   (setf (slot-value self 'selected) (gethash ref-id (local-cache self)))
;;   t)

;; (defmethod/remote on-select ((self <core:table) object)
;;   (_set-selected self (slot-value object 'ref-id))
;;   (answer-component self object))

;; (defmethod/remote tbody ((component <core:table))
;;   (let ((instances (get-instances component)))
;;     (if (null instances)
;; 	(<:tbody (<:tr (<:th "Table has no elements.")))
;; 	(<:tbody
;; 	 (mapcar2
;; 	  (lambda (object index)
;; 	    (let ((checkbox
;; 		   (<:input :type "checkbox"
;; 			    :onchange (event (e)
;; 				       (let ((checked this.checked))
;; 					       (with-call/cc
;; 						 (if checked
;; 						     (add-selected component object)
;; 						     (remove-selected component object))))
;; 					     false))))
;; 	      (setf (slot-value object "checkbox") checkbox)
;; 	      (<:tr :class (if (eq 0 (mod index 2)) (get-hilight-class component))
;; 		    (<:td :class "checkbox" checkbox)	       
;; 		    (mapcar (lambda (slot)			  
;; 			      (let ((name (slot-value slot 'name))
;; 				    (value (slot-value object (slot-value slot 'name))))
;; 				(<:td :class name
;; 				      (if (equal name (primary-field self))
;; 					  (<:a :onclick (event (e)
;; 							       (with-call/cc (on-select component object))
;; 							       false)
;; 					       (or value (slot-value slot 'initform)))
;; 					  (or value (slot-value slot 'initform))))))
;; 			    (slot-value object 'class)))))
;; 	  (get-instances component) (seq (slot-value instances 'length)))))))

;; (defmethod/remote tfoot ((component <core:table))
;;   (<:tfoot))

;; (defmethod/remote init ((component <core:table))
;;   (add-class component "grid")
;;   (setf (slot-value component 'inner-h-t-m-l) nil)
;;   (mapcar (lambda (i) (.append-child component i))
;; 	  (list (thead component) (tbody component) (tfoot component))))

;; (defmacro deftable (name supers slots &rest rest)
;;   `(progn
;;      (defcomponent ,name (,@supers <core:table)
;;        ()
;;        (:default-initargs
;; 	 :slots (jobject
;; 		 :class
;; 		 ,@(mapcar
;; 		    (lambda (slot)
;; 		      `(core-server::jobject
;; 			:name ',(symbol-to-js (car slot)) ,@(cdr slot)))
;; 		    slots))
;; 	 ,@(flatten1 rest)))))


;; (defmacro deftable (name class slots)
;;   `(defun/cc ,name (instances
;; 		    &key (table-class "table-class"))
;;      (<:table :class table-class
;; 	      (<:thead (<:tr ,@(mapcar (lambda (slot)
;; 					 `(<:th ,(getf (cdr slot) :label)))
;; 				       slots)))
;; 	      (<:tbody
;; 	       (mapcar (lambda (instance)
;; 			 (<:tr
;; 			  ,@(mapcar (lambda (slot)
;; 				      `(<:td (funcall ,(getf (cdr slot) :reader) instance)))
;; 				    slots)))
;; 		       instances)))))

;; (defmacro deftable-template (name)
;;   (let ((class+ (find-class+ name)))    
;;     `(defmethod/local core-server::abuzer ((self ,name))
;;        (flet ((get-key (name key)
;; 		(getf (cdr (assoc name (slot-value self 'slots))) key)))
;; 	 (list
;; 	  (<:thead
;; 	   (<:tr
;; 	    ,@(mapcar (lambda (slot)
;; 			(with-slotdef (name) slot
;; 			  `(<:th (get-key ',name :label))))
;; 		      (class+.local-slots class+))))
;; 	  (<:tbody
;; 	   (mapcar (lambda (instance)
;; 		     (<:tr
;; 		      (mapcar (lambda (slot)
;; 				)
;; 			      )))
;; 		   (slot-value self 'instances))))))))

;; (defmacro deftable (name supers slots &rest rest)  
;;   `(progn
;;      (defhtml-component ,name (<:table ,@supers)
;;        ((slots :initarg :slots :initform ',slots)
;; 	(instances :initarg :instances :initform nil))
;;        ,@rest)
;;      (deftable-template ,name)
;;      ))

;; (defmethod shared-initialize :after ((component <core:table) slot-names
;; 				     &key &allow-other-keys)
;;   ;; (setf (table.objects component)
;; ;; 	(mapcar (lambda (instance)
;; ;; 		  (apply #'jobject
;; ;; 			 (nreverse
;; ;; 			  (reduce (lambda (acc slot)
;; ;; 				    (cons (make-keyword (car slot))
;; ;; 					  (cons (funcall (let ((reader (getf (cdr slot) :reader)))
;; ;; 							   (if (and (listp reader) (eq 'lambda (car reader)))
;; ;; 							       (setf reader (eval reader)
;; ;; 								     (getf (cdr slot) :reader) reader))
;; ;; 							   (or reader (slot-value instance (car slot))))
;; ;; 							 instance)
;; ;; 						acc)))
;; ;; 				  (table.slots component)
;; ;; 				  :initial-value `(:id ,(get-id instance))))))
;; ;; 		(table.instances component))
;; ;; 	(table.table-slots component)
;; ;; 	(mapcar (lambda (slot)
;; ;; 		  (jobject :name (symbol-to-js (car slot))
;; ;; 			   :type (symbol-to-js (getf (cdr slot) :type))
;; ;; 			   :label (getf (cdr slot) :label)))
;; ;; 		(table.slots component)))
;;   )
