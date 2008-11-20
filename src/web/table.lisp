(in-package :core-server)

;; +----------------------------------------------------------------------------
;; | Table Component
;; +----------------------------------------------------------------------------
(defcomponent <core:table (<:table)
  ((instances :host remote :initarg :instances :initform nil)
   (hilight-class :initform "hilighted" :host remote)
   (selected-class :initform "selected" :host remote)
   (selected :initform nil :host remote)
   (slots :initform nil :host remote)))

(defmethod/remote add-selected ((component <core:table) selection)
  (add-class (slot-value
	      (slot-value (slot-value selection 'checkbox) 'parent-node)
	      'parent-node)
	     (get-selected-class component))
  (set-selected component (cons selection
				(remove-selected component selection))))

(defmethod/remote remove-selected ((component <core:table) selection)
  (remove-class (slot-value
		 (slot-value (slot-value selection 'checkbox) 'parent-node)
		 'parent-node)
		(get-selected-class component))
  (set-selected component (filter (lambda (a) (not (eq a selection)))
				  (get-selected component))))

(defmethod/remote thead ((component <core:table))
  (<:thead
   (<:tr
    (<:th :class "checkbox"
	  (<:input :type "checkbox"
		   :onchange (lambda (e)
			       (let ((checked this.checked))
				 (mapcar (lambda (object) (setf object.checkbox.checked checked))
					 (get-instances component))
				 (if checked
				     (set-selected component (get-instances component))
				     (set-selected component nil)))
			       false)))
    (mapcar (lambda (slot)
	      (with-slots (name label) slot
		(<:th :class name (or label name))))
	    (get-slots component)))))

(defmethod/local handle-click ((component <core:table) object)
  (let ((object (find object (table.instances component) :key #'get-id)))
    (when object
      (answer (cons :select object)))))

(defmethod/remote tbody ((component <core:table))
  (if (null (get-instances component))
      (return (<:tbody (<:tr (<:th "Table has no elements.")))))
  
  (let ((i 0))
    (<:tbody
     (mapcar
      (lambda (object)
	(let ((checkbox (<:input :type "checkbox"
				 :onchange (lambda (e)
					     (if this.checked
						 (add-selected component object)
						 (remove-selected component object))))))
	  (setf (slot-value object "checkbox") checkbox)
	  (setf i (+ 1 i))
	  (<:tr :class (if (eq 0 (mod i 2)) (get-hilight-class component))
		(<:td :class "checkbox" checkbox)	       
		(mapcar (lambda (slot)			  
			  (let ((name (slot-value slot "name"))
				(value (slot-value (slot-value object name) 'value)))
			    (<:td :class name
				  (if (eq name "name")
				      (<:a :onclick (lambda (e)
						      (handle-click component object)
						      false)
					   value)
				      value))))
			(get-slots component)))))
      (get-instances component)))))

(defmethod/remote tfoot ((component <core:table))
  (<:tfoot))

(defmethod/remote init ((component <core:table))
  (add-class component "grid")
  (mapcar (lambda (i) (.append-child component i))
	  (list (thead component) (tbody component) (tfoot component))))

(defmacro deftable (name supers slots &rest rest)
  `(progn
     (defcomponent ,name (,@supers <core:table)
       ()
       (:default-initargs
	 :slots (list
		 ,@(mapcar
		    (lambda (slot)
		      `(core-server::jobject :name ',(symbol-to-js (car slot)) ,@(cdr slot)))
		    slots))
	 ,@(flatten1 rest)))))


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
