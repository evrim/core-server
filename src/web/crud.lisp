(in-package :core-server)

;; Fixme: Date lazim, Integer->Date, Text Editor

;; -------------------------------------------------------------------------
;; Crud Component
;; -------------------------------------------------------------------------
(defcomponent <core:crud (<:div callable-component cached-component
				slot-representations)
  ((instance :host remote)
   (template-class :host remote)
   (title :host remote :initform "Please set crud title")
   (deletable-p :host both :initform t)
   (editable-p :host both :initform t)
   (crud-css :host remote :initform "style/crud.css")
   (slot-editors :host remote :initform nil)
   (default-slot-editor :host remote :initform (<core:required-value-input))
   (yes-no-dialog-ctor :host remote :initform (yes-no-dialog))
   (_template :host remote)
   (_result :host remote :initform (jobject))))

(defmethod/remote get-title ((self <core:crud))
  (slot-value self 'title))

(defmethod/remote get-template-class ((self <core:crud))
  (or (slot-value self 'template-class)
      (and (instance self) (slot-value (instance self) 'core-class))))

(defmethod/remote make-yes-no-dialog ((self <core:crud))
  (yes-no-dialog-ctor self))

(defmethod shared-initialize :after ((self <core:crud) slots &rest initargs)
  (declare (ignore initargs))
  (awhen (template-class self)
    (flet ((beautify (lst)
	     (uniq (sort lst #'string< :key #'car) :test #'string= :key #'car)))
      (let ((slots (beautify
		    (mapcar
		     (lambda (attribute)
		       (if (typep attribute 'jobject)
			   (let ((type (get-attribute attribute :remote-type)))
			     (if (and (null (get-attribute attribute :read-only))
				      type)
				 (cons type it)))))
		     (jobject.attributes it)))))
	(setf (slot-editors self)
	      (beautify
	       (remove-if (lambda (a) (or (null (car a)) (null (cdr a))))
			  (mapcar
			   (lambda (remote-type)
			     (cons (car remote-type)
				   (%get-slot-editor self (car remote-type))))
			   slots))))))))

(defmethod/remote _get-slot-editor ((self slot-representations) slot)
  (with-slots (slot-editors) self
    (with-slots (remote-type) slot
      (let ((editor (find (lambda (a) (equal (car a) remote-type))
			  (slot-editors self))))
	(if editor
	    (car (cdr editor))
	    (default-slot-editor self))))))

(defmethod/remote edit-me ((self <core:crud) slot)
  (with-slots (reader name remote-type options size min-length) slot
    (flet ((set-ret (val)
	     (setf (slot-value (_result self) name) val)
	     val))
      (let ((slot-editor (_get-slot-editor self slot))
	    (_value (if reader
			(reader (instance self))
			(slot-value (instance self) name))))
	(_debug (list "crud" "name" name "value" _value "remote-type"
		      remote-type))
	(let* ((_id (random-string))
	       (_span (<:span :class "validation " :id _id)))
	  (list
	   (cond
	     ((eq "password" remote-type) ;; Needs two fields to compare
	      (set-ret
	       (make-component slot-editor :value _value
			       :min-length min-length
			       :validation-span-id _id)))
	     ((member remote-type '("number" "email")) ;; OK
	      (set-ret
	       (make-component slot-editor :value _value
			       :validation-span-id _id)))
	     ((eq remote-type "timestamp")
	      (set-ret
	       (make-component slot-editor :value _value
			       :validation-span-id _id)))
	     ((eq remote-type "date")
	      (set-ret
	       (make-component slot-editor :value _value :show-time nil
			       :validation-span-id _id)))
	     ((eq "html" remote-type) ;; OK
	      (let* ((value (cond
			      ((null _value) "")
			      ((slot-value _value 'tag-name)
			       (let ((div (<:div _value)))
				 (slot-value div 'inner-h-t-m-l)))
			      (t _value)))
		     (textarea (<:textarea :name name :id (random-string) value))
		     (editor (call/cc slot-editor textarea)))
		(set-ret editor)))
	     ((eq "select" remote-type) ;; OK
	      (let ((_editor (make-component slot-editor :current-value _value
					     :option-values options)))
		(set-ret _editor)))
	     ((or (eq "multipleSelect" remote-type)
		  (eq "multiple-select" remote-type))
	      (let ((_editor (make-component slot-editor :current-value _value
					     :option-values options
					     :size (or size 5))))
		(set-ret _editor)))
	     ((or (eq "multiple-checkbox" remote-type)
		  (eq "multipleCheckbox" remote-type))
	      (let ((_editor (make-component slot-editor :current-value _value
					     :option-values options)))
		(set-ret _editor)))
	     (t
	      (set-ret (make-component (default-slot-editor self)
				       :value _value 
				       :validation-span-id _id))))
	   _span))))))

(defmethod/remote view-me ((self <core:crud) slot)
  (let ((value (get-slot-view self slot (instance self))))
    (<:span (or value "Not set"))))

(defmethod/remote do-edit ((self <core:crud))
  (let ((_template (_template self)))
    (setf (_template self) (replace-node _template (edit-template self)))))

(defmethod/remote view-template ((self <core:crud))
  (<:div :class "crud"
	 (<:form
	  (mapcar-cc
	   (lambda (slot)
	     (with-slots (name label read-only) slot	       
	       (with-field (+ label ":") (view-me self slot))))
	   (reverse (reverse (get-template-class self))))
	  (<:p :class "buttons"
	       (if (editable-p self)
		   (<:input :type "button" :value "Edit"
			    :onclick (lifte (do-edit self))))
	       (if (deletable-p self)
		   (<:input :type "button" :value "Delete"
			    :onclick (lifte (do-delete1 self))))))))

(defmethod/remote do-cancel ((self <core:crud))
  (let ((_template (_template self)))
    (setf (_template self) (replace-node _template (view-template self)))))

(defmethod/remote do-save1 ((self <core:crud))
  ;; (_debug (list "result" (_result self)))
  (answer-component self (list "update"
			       (mapobject (lambda (k v) (get-input-value v))
					  (_result self)))))

(defmethod/remote do-delete1 ((self <core:crud))
  (if (call-component (make-component (make-yes-no-dialog self)
				      :title "delete"
				      :message (+ "Do you really want to"
						  " delete this item?")))
      (answer-component self (list "delete" (instance self)))))

(defmethod/remote edit-template ((self <core:crud))
  (<:div :class "crud"
	 (<:form :onsubmit (lifte (do-save1 self))
		 (mapcar-cc
		  (lambda (slot)
		    (with-slots (name label read-only) slot
		      (cond
			(read-only
			 (with-field (+ label ": ")
			   (<:div :class "value-view" (view-me self slot))))
			(t
			 (destructuring-bind (editor span) (edit-me self slot)
			   (with-field (<:div :class "label-edit" label ": ")
			     (list editor span)))))))
		  (reverse (reverse (get-template-class self))))
		 (<:p :class "buttons"
		      (<:input :type "submit" :value "Save"
			       ;; :disabled t
			       ;; :onclick (lifte (do-save1 self))
			       )
		      (<:input :type "button" :value "Cancel"
			       :onclick (lifte (do-cancel self)))))))

(defmethod/remote remove-child-nodes ((self <core:crud))
  (mapcar-cc (lambda (a) (.remove-child self a))
	     (reverse (slot-value self 'child-nodes))))

(defmethod/remote destroy ((self <core:crud))
  (remove-css (crud-css self))
  (remove-class self "crud")
  (remove-child-nodes self)
  (delete-slots self 'instance 'template-class 'crud-css 'title
  		'deletable-p 'editable-p 'yes-no-dialog-ctor '_ckeditor
  		'_template '_result)
  (call-next-method self))

(defmethod/remote init ((self <core:crud))
  (load-css (crud-css self))
  (add-class self "crud")
  (append self (<:h2 (title self)))
  (append self (setf (_template self) (view-template self))))

(defmacro defwebcrud (name supers slots &rest rest)
  `(defcomponent ,name (,@supers <core:crud)
     ()
     (:default-initargs :template-class ,(%generate-template-class slots)
       ,@(cdr (flatten1 rest)))))


;; old edit-me
;; (flet ((default-onchange (name)
;; 	   (event (e)
;; 	     (with-slots (_result) self
;; 	       (setf (slot-value _result name) this.value))))
;; 	 (multi-checkbox-onchange (name)
;; 	   (event (e)		  
;; 	     (let ((value this.value))
;; 	       (with-slots (_result) self		 
;; 		 (if this.checked
;; 		     (setf (slot-value _result name)
;; 			   (cons value
;; 				 (filter (lambda (a) (not (eq a value)))
;; 					 (slot-value _result name))))
;; 		     (setf (slot-value _result name)
;; 			   (filter (lambda (a) (not (eq a value)))
;; 				   (slot-value _result name))))))	     
;; 	     t)))
;;     (with-slots (reader name type options size) slot
;;       (let ((value (if reader
;; 		       (reader (instance self))
;; 		       (slot-value (instance self) name))))
;; 	(cond
;; 	  ((eq type "select")
;; 	   (<:select :onchange (default-onchange name)
;; 	    (mapcar-cc (lambda (option)
;; 			 (if (eq option value)
;; 			     (<:option :selected t option)
;; 			     (<:option option)))
;; 		       options)))
;; 	  ((eq type "multiple-select")
;; 	   (setf (slot-value (_result self) name) value)
;; 	   (<:div
;; 	    (mapcar
;; 	     (lambda (option)
;; 	       (<:div
;; 		(<:div :class "left pad5"
;; 		       (if (member option value)
;; 			   (<:input :type "checkbox" :value option
;; 				    :checked t :name option
;; 				    :onchange (multi-checkbox-onchange name))
;; 			   (<:input :type "checkbox" :value option
;; 				    :name option
;; 				    :onchange (multi-checkbox-onchange name))))
;; 		(<:div :class "pad5" option)))
;; 	     options)))
;; 	  ((eq type "password")
;; 	   (<:input :type "password" :name name :value ""
;; 		    :onchange (default-onchange name)))
;; 	  ((eq type "html")
;; 	   (let* ((value (cond
;; 			   ((null value) "")
;; 			   ((not (null (slot-value value 'tag-name)))
;; 			    (let ((div (<:div value)))
;; 			      (slot-value div 'inner-h-t-m-l)))
;; 			   (t value)))
;; 		  (textarea (<:textarea :name name :id (random-string) value))
;; 		  (editor (call/cc (_ckeditor self) textarea)))
;; 	     (setf (slot-value textarea 'onchange)
;; 		   (compose-prog1 (slot-value textarea 'onchange)
;; 				  (default-onchange name)))
;; 	     editor))
;; 	  (t
;; 	   (<:input :type "text" :name name :value value
;; 		    :onchange (default-onchange name)))))))