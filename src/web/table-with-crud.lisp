(in-package :core-server)

;; -------------------------------------------------------------------------
;; Table w/ Crud Component
;; -------------------------------------------------------------------------
(defcomponent <core:table-with-crud (<:div)
  ((table :host remote :initform (error "Provide :table-ctor"))
   (crud :host remote :initform (error "Provide :crud-ctor"))
   (input-element :host remote
		  :initform (<core:default-value-input :default-value
				"Enter key value (ie a key-value)"))
   (table-title :host remote :initform "Items")
   (_crud :host remote)
   (_table :host remote)))

(defmethod/local get-instances ((self <core:table-with-crud))
  (error "Please implement get-instances method."))

(defmethod/local add-instance ((self <core:table-with-crud) key)
  (error "Please implement add-instance method."))

(defmethod/local delete-instance ((self <core:table-with-crud) key)
  (error "Please implement delete-instance method."))

(defmethod/remote make-table ((self <core:table-with-crud))
  (let ((_instances (mapcar-cc (lambda (a) (if (typep a 'function)
					       (make-component a)
					       a))
			       (get-instances self))))
    (make-component (table self) :instances _instances)))

(defmethod/remote do-add-instance ((self <core:table-with-crud) key)
  (let ((_instance (add-instance self key)))
    (if _instance
	(add-instance (_table self) (make-component _instance)))))

(defmethod/remote make-form ((self <core:table-with-crud))
  (let* ((_id (random-string))
	 (_span (<:span :id _id :class "validation"))
	 (val (make-component (input-element self)
			      :validation-span-id _id)))
    (add-class val "width-250px")
    (add-class val "pad5")
    (<:form :onsubmit (lifte
		       (let ((_value (get-input-value val)))
			 (+ _value "")
			 (reset-input-value val)
			 (do-add-instance self _value)))
	    _span
	    val
	    (<:input :type "submit" :disabled t :value "Add"))))

(defmethod/remote destroy ((self <core:table-with-crud))
  (remove-class self "core")
  (call-next-method self))

(defmethod/remote init ((self <core:table-with-crud))
  (add-class self "core")
  (call-next-method self)
  (let ((form (make-form self))
	(table (setf (_table self) (make-table self)))
	(crud (setf (_crud self) (<:div))))
    (append self (<:div :class "heading"
			(<:h2 :class "left" (table-title self))
			(<:div :class "right" form)))
    (append self table)
    (append self crud)
    (make-web-thread
     (lambda ()
       (let* ((result (call-component table))
	      (new-crud (make-component (crud self) :instance result)))
	 (setf (_crud self) (replace-node (_crud self) new-crud))
	 (make-web-thread
	  (lambda ()
	    (destructuring-bind (action &rest args) (call-component new-crud)
	      (cond
		((eq "delete" action)
		 (delete-instance self result)
		 (setf (_crud self) (replace-node (_crud self) (<:div)))
		 (remove-instance (_table self) result))
		(t
		 (_debug (list "crud" "action" action "args" args))
		 nil))))))))))
