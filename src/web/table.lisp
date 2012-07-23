(in-package :core-server)

;; -------------------------------------------------------------------------
;; Table Component (Single Select)
;; -------------------------------------------------------------------------
(defcomponent <core:table (<:table cached-component slot-representations
				   callable-component)
  ((instances :host remote :initarg :instances :initform nil)
   (primary-field :host remote :initform "name")
   (template-class :initform nil :host remote)
   (hilight-class :initform "hilighted" :host remote)
   (hover-class :initform "hover" :host remote)
   (selected-class :initform "selected" :host remote)
   (table-css :host remote :initform "style/table.css")
   (selected :initform nil :host remote)
   (_sorted-slot :host remote)
   (_head :host remote)
   (_foot :host remote)
   (_resize-thread :host remote)))

(defmethod/remote get-template-class ((self <core:table))
  (with-slots (instances) self
    (if (and instances (null (template-class self)))
	(setf (template-class self)
	      (slot-value (car instances) 'core-class))))
  (slot-value self 'template-class))

(defmethod/remote column-span ((self <core:table))
  (aif (object-to-list (template-class self))
       (slot-value it 'length)
       1))

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

(defmethod/remote _on-select ((self <core:table) instance)
  (mapcar-cc (lambda (instance)
	       (awhen (slot-value instance '_table-row)
		 (remove-class it (selected-class self))))
	     (instances self))

  (flet ((first-child (node) (slot-value node 'first-child)))
    (awhen (slot-value instance '_table-row)
      (add-class it (selected-class self))
      (setf (slot-value (call/cc first-child (call/cc first-child it)) 'checked) t
	    (selected self) instance))))

(defmethod/remote on-select ((self <core:table) instance)
  (_on-select self instance)
  (answer-component self instance))

(defmethod/remote thead ((self <core:table))
  (<:table :class (slot-value self 'class-name)
	   (<:thead
	    (<:tr
	     (<:th :class "first-column" " ")
	     (mapcar (lambda (slot)
		       (with-slots (name label) slot
			 (<:th :class name
			       (<:a :onclick (lifte (sort-table self slot))
				    (or label name)))))
		     (template-class self))))))

(defmethod/remote make-row ((self <core:table) instance)
  (let* ((radio (<:input :type "radio" :name "table-object"
			 :onclick (lifte2 (on-select self instance))))
	 (row (<:tr :onmouseover (event (e) (add-class this (hover-class self)))
		    :onmouseout (event (e) (remove-class this (hover-class self)))
		    :onclick (lifte (on-select self (slot-value this 'instance)))
		    (cons (<:td :class "first-column" radio)
			  (mapcar
			   (lambda (slot)
			     (<:td :class (slot-value slot 'name)
				   (get-slot-view self slot instance)))
			   (template-class self))))))
    (setf (slot-value instance '_table-row) row
	  (slot-value row 'instance) instance)
    row))

(defmethod/remote tbody ((self <core:table))
  (aif (instances self)
       (<:tbody
	(mapcar2-cc
	 (lambda (instance index)
	   (let ((row (make-row self instance)))
	     (if (eq 0 (mod index 2))
		 (add-class row (hilight-class self)))
	     row))
	 it (seq (slot-value it 'length))))
       (<:tbody
	(<:tr (<:td :colspan (column-span self) "Table has no elements.")))))

(defmethod/remote tfoot ((self <core:table))
  (<:table :class (slot-value self 'class-name)
	   (<:tfoot
	    (<:tr
	     (<:td :class "text-right"
		   :colspan (column-span self)
		   (aif (instances self)
			(+ (slot-value it 'length) " item(s).")
			"No items."))))))

(defmethod/remote update-tfoot ((self <core:table))
  (setf (_foot self) (replace-node (_foot self) (tfoot self))))

;; function ResizeWidths(div) {
;;     var headerCells =  $(div).find('.headerTable thead').find('th');
;;     var contentCells = $(div).find('.contentTable thead').find('th');
;;     for(var i =0, ii = headerCells.length;i<ii;i++)
;;     {
;;         if($(headerCells[i]).width()!=$(contentCells[i]).width())
;;             $(headerCells[i]).width($(contentCells[i]).width());
;;     }
;; }
(defmethod/remote resize-thead ((self <core:table))
  (with-slots (instances) self
    (when (and instances (> (slot-value instances 'length) 0))
      (flet ((get-width (element)
	       (slot-value element 'offset-width))
	     (by-tag-name (root tag-name)
	       (node-search (lambda (a)
			      (and (slot-value a 'tag-name)
				   (eq (slot-value a 'tag-name)
				       tag-name)))
			    root)))
	(setf (slot-value (slot-value (_head self) 'style) 'width)
	      (+ (get-width self) "px"))
	(mapcar2 (lambda (head-cell body-cell)
		   (setf (slot-value (slot-value head-cell 'style) 'width)
			 (+ (- (get-width body-cell) 10) "px")))
		 (by-tag-name (car (by-tag-name (_head self) "TR")) "TH")
		 (by-tag-name self "TD"))))))

(defmethod/remote remove-child-nodes ((self <core:table))
  (mapcar-cc (lambda (a) (.remove-child self a))
	     (slot-value self 'child-nodes)))

(defmethod/remote destroy ((self <core:table))
  (clear-interval (_resize-thread self))
  (with-slots (parent-node) self    
    (with-slots (parent-node) parent-node
      (.remove-child parent-node (_head self))
      (.remove-child parent-node (_foot self)))
    (replace-node parent-node self))
  (call-next-method self))

(defmethod/remote init ((self <core:table))
  (add-class self "core-table")
  (load-css (table-css self))
  (remove-child-nodes self)
  (let ((head (setf (_head self) (thead self)))
	(foot (setf (_foot self) (tfoot self))))
    (append self (tbody self))
    (labels ((do-when-parent (f)
	       (if (slot-value self 'parent-node)
		   (call/cc f)
		   (make-web-thread (lambda () (do-when-parent f)))))
	     (wrap-me (head foot)
	       (let ((parent-node (slot-value self 'parent-node)))
		 (let ((div (<:div :class "core-table-wrapper")))
		   (if (has-class parent-node "core-table-overflow")
		       (replace-node (slot-value parent-node 'parent-node)
				     div)
		       (replace-node self div))
		   (append div head)
		   (append div (<:div :class "core-table-overflow" self))
		   (append div foot)
		   (setf (_resize-thread self)
			 (set-interval (event () (resize-thead self window.k))
				       300))))))
      (do-when-parent (lambda () (wrap-me head foot))))))

(defmethod/remote add-instance ((self <core:table) instance)
  (with-slots (instances) self
    (let ((tbody (aref (.get-elements-by-tag-name self "TBODY") 0))
	  (row (make-row self instance)))
    
      (if (null instances)
	  (.remove-child tbody (slot-value tbody 'first-child)))

      (setf (instances self) (cons instance instances))
      (update-tfoot self)

      (if (eq 1 (mod (slot-value (instances self) 'length) 2))
	  (add-class row (hilight-class self)))
    
      (prepend tbody row)
      (setf (slot-value (slot-value self 'parent-node) 'scroll-top) "0")
      (resize-thead self)
      (on-select self instance))))

(defmethod/remote remove-instance ((self <core:table) instance)
  (with-slots (instances) self
    (setf (instances self)
	  (filter-cc (lambda (a) (not (eq instance a))) instances))

    (with-slots (_table-row) instance
      (if _table-row
	  (.remove-child (slot-value _table-row 'parent-node) _table-row))

      (delete-slot instance '_table-row)
      (if (eq (selected self) instance) (setf (selected self) nil))
      (update-tfoot self)

      (if (null (instances self))
	  (replace-node (aref (.get-elements-by-tag-name self "TBODY") 0)
			(tbody self)))

      (resize-thead self))

    ;; (setf (slot-value (slot-value self 'parent-node) 'scroll-top) "0")
    instance))

(defmacro deftable (name supers slots &rest rest)
  `(defcomponent ,name (,@supers <core:table)
     ()
     (:default-initargs :template-class ,(%generate-template-class slots)
       ,@(cdr (flatten1 rest)))))

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


;; (defmethod/remote column-span ((self <core:table))
;;   (call/cc
;;    (event (c)
;;      (apply self.get-template-class self
;; 	    (list
;; 	     (lambda (_class)		      
;; 	       (let ((a 1))
;; 		 (mapobject (lambda (k v) (setq a (+ 1 a))) _class)
;; 		 (c a))))))))
