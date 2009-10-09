(in-package :core-server)

;; +----------------------------------------------------------------------------
;; | Table Component
;; +----------------------------------------------------------------------------
(defcomponent <core:table (<:table)
  ((instances :host remote :initarg :instances :initform nil)
   (hilight-class :initform "hilighted" :host remote)
   (selected-class :initform "selected" :host remote)
   (selected :initform nil :host remote)
   (primary-field :initform "name" :host remote)
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
	    (or (slot-value (car (instances self)) 'class)
		(get-slots component))))))

(defmethod/remote on-select ((component <core:table) object)
  (answer-component component object))

(defmethod/remote tbody ((component <core:table))
  (let ((instances (get-instances component)))
    (if (null instances)
	(<:tbody (<:tr (<:th "Table has no elements.")))
	(<:tbody
	 (mapcar2
	  (lambda (object index)
	    (let ((checkbox (<:input :type "checkbox"
				     :onchange (lambda (e)
						 (if this.checked
						     (add-selected component object)
						     (remove-selected component object))))))
	      (setf (slot-value object "checkbox") checkbox)
	      (<:tr :class (if (eq 0 (mod index 2)) (get-hilight-class component))
		    (<:td :class "checkbox" checkbox)	       
		    (mapcar (lambda (slot)			  
			      (let ((name (slot-value slot 'name))
				    (value (slot-value object (slot-value slot 'name))))
				(<:td :class name
				      (if (equal name (primary-field self))
					  (<:a :onclick (event (e)
							  (with-call/cc (on-select component object))
							  false)
					       value)
					  value))))
			    (or (get-slots component)
				(slot-value object 'class))))))
	  (get-instances component) (seq (slot-value instances 'length)))))))

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
