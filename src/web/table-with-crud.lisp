(in-package :core-server)

;; -------------------------------------------------------------------------
;; Table w/ Crud Component
;; -------------------------------------------------------------------------
(defcomponent <core:table-with-crud (<:div)
  ((table :host remote :initform (error "Provide :table"))
   (crud :host remote :initform (error "Provide :crud"))
   (input-element :host remote
		  :initform (<core:default-value-input :default-value
				"Enter key value (ie a key-value)"))
   (table-title :host remote :initform "Items")
   (_crud :host remote)
   (_table :host remote)))

(defmethod/local get-instances ((self <core:table-with-crud))
  (error "Please implement get-instances method."))

(defmethod/cc get-instances :around ((self <core:table-with-crud))
  (authorize (component.application self) (query-session :user)
	     (call-next-method self)))

(defmethod/local add-instance ((self <core:table-with-crud) key)
  (error "Please implement add-instance method."))

(defmethod/cc add-instance :around ((self <core:table-with-crud) key)
  (authorize (component.application self) (query-session :user)
	     (call-next-method self key)))

(defmethod/local delete-instance ((self <core:table-with-crud)
				  (instance remote-reference))
  (error "Please implement delete-instance method."))

(defmethod/cc delete-instance :around ((self <core:table-with-crud)
				       (instance remote-reference))
  (authorize (component.application self) (query-session :user)
	     (call-next-method self instance)))

(defmethod/local update-instance ((self <core:table-with-crud)
				  (instance remote-reference) args)
  (error "Please implement update-instance method."))

(defmethod/cc update-instance :around ((self <core:table-with-crud)
				       (instance remote-reference) args)
  (authorize (component.application self) (query-session :user)
	     (call-next-method self instance args)))

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
	 (val (make-component (input-element self) :validation-span-id _id)))
    (add-class val "width-250px")
    (add-class val "pad5")
    (<:form :onsubmit (lifte
		       (progn (do-add-instance self (get-input-value val))
			      (reset-input-value val)))
	    _span val
	    (<:input :type "submit" :disabled t :value "Add"))))

(defmethod/remote destroy ((self <core:table-with-crud))
  (remove-class self "core")
  (if (and (_crud self) (slot-value (_crud self) 'destroy))
      (destroy (_crud self)))
  (if (and (_table self) (slot-value (_table self) 'destroy))
      (destroy (_table self)))
  
  (call-next-method self))

(defmethod/remote handle-crud ((self <core:table-with-crud) instance
			       action args)
  (cond
    ((eq "delete" action)
     (when (delete-instance self instance)
       (with-slots (_crud) self
	 (destroy _crud)
	 (setf (_crud self) (replace-node _crud (<:div)))
	 (remove-instance (_table self) instance))))
    ((eq "update" action)
     (let ((updates (update-instance self instance args)))
       (cond
	 ((typep updates 'function)
	  (remove-instance (_table self) instance)
	  (add-instance (_table self) (make-component updates)))
	 (t
	  (remove-instance (_table self) instance)
	  (extend updates instance)
	  (add-instance (_table self) instance)))))
    (t (_debug (list "crud" "action" action "args" args)))))

(defmethod/remote _make-crud-component ((self <core:table-with-crud)
					instance)
  (make-component (crud self) :instance instance))

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
       (let* ((instance (call-component table))
	      (new-crud (_make-crud-component self instance)))
	 (if (and (_crud self) (slot-value (_crud self) 'destroy))
	     (destroy (_crud self)))
	 
	 (setf (_crud self) (replace-node (_crud self) new-crud))
	 (make-web-thread
	  (lambda ()
	    (destructuring-bind (action &rest args) (call-component new-crud)
	      (handle-crud self instance action args)))))))))
