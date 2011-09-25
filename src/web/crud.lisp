(in-package :core-server)

;; Fixme: Date lazim, Integer->Date, Text Editor

;; -------------------------------------------------------------------------
;; Crud Component
;; -------------------------------------------------------------------------
(defcomponent <core:crud (<:div callable-component cached-component)
  ((instance :host remote)
   (template-class :host remote)
   (title :host remote :initform "Please set crud title")
   (deletable-p :host both :initform t)
   (editable-p :host both :initform t)
   (crud-css :host remote :initform "http://www.coretal.net/style/crud.css")
   (yes-no-dialog-ctor :host remote :initform (yes-no-dialog))
   (_template :host remote)
   (_result :host remote :initform (jobject))))

(defmethod/remote make-yes-no-dialog ((self <core:crud))
  (yes-no-dialog-ctor self))

(defmethod/remote get-title ((self <core:crud))
  (slot-value self 'title))

(defmethod/remote get-template-class ((self <core:crud))
  (or (slot-value self 'template-class)
      (and (instance self) (slot-value (instance self) 'core-class))))

(defmethod/remote get-value ((self <core:crud) slot)
  (with-slots (name type options size reader) slot
    (let* ((_instance (instance self))
	   (_value (if reader
		       (reader _instance)
		       (slot-value _instance name))))
      (cond
	((eq "password" type)
	 "*****************")
	((eq "multiple-select" type)
	 (reduce-cc (lambda (acc atom)
		      (+ acc ", " atom))
		    (cdr _value) (car _value)))
	(t
	 _value)))))

(defmethod/remote edit-me ((self <core:crud) slot)
  (flet ((default-onchange (name)
	   (event (e)
	     (with-slots (_result) self
	       (setf (slot-value _result name) this.value))))
	 (multi-checkbox-onchange (name)
	   (event (e)		  
	     (let ((value this.value))
	       (with-slots (_result) self		 
		 (if this.checked
		     (setf (slot-value _result name)
			   (cons value
				 (filter (lambda (a) (not (eq a value)))
					 (slot-value _result name))))
		     (setf (slot-value _result name)
			   (filter (lambda (a) (not (eq a value)))
				   (slot-value _result name))))))	     
	     t)))
    (with-slots (reader name type options size) slot
      (let ((value (if reader
		       (reader (instance self))
		       (slot-value (instance self) name))))
	(cond
	  ((eq type "select")
	   (<:select :onchange (default-onchange name)
	    (mapcar-cc (lambda (option)
			 (if (eq option value)
			     (<:option :selected t option)
			     (<:option option)))
		       options)))
	  ((eq type "multiple-select")
	   (setf (slot-value (_result self) name) value)
	   (<:div
	    (mapcar
	     (lambda (option)
	       (<:div
		(<:div :class "left pad5"
		       (if (member option value)
			   (<:input :type "checkbox" :value option
				    :checked t :name option
				    :onchange (multi-checkbox-onchange name))
			   (<:input :type "checkbox" :value option
				    :name option
				    :onchange (multi-checkbox-onchange name))))
		(<:div :class "pad5" option)))
	     options)))
	  ((eq type "password")
	   (<:input :type "password" :name name :value value
		    :onchange (default-onchange name)))
	  (t
	   (<:input :type "text" :name name :value value
		    :onchange (default-onchange name))))))))

(defmethod/remote view-me ((self <core:crud) slot)
  (let ((value (get-value self slot)))
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
    (setf (_template self)
	  (replace-node _template (view-template self)))))

(defmethod/remote do-save1 ((self <core:crud))
;;   (_debug (list "result" (_result self)))
  (answer-component self (list "update" (_result self))))

(defmethod/remote do-delete1 ((self <core:crud))
  (if (call-component (make-component (make-yes-no-dialog self)
				      :title "delete"
				      :message (+ "Do you really want to"
						  " delete this item?")))
      (answer-component self (list "delete"))))

(defmethod/remote edit-template ((self <core:crud))
  (<:div :class "crud"
	 (<:form
	  (mapcar-cc
	   (lambda (slot)
	     (with-slots (name label read-only) slot	       
	       (with-field (+ label ":")
		 (cond
		   (read-only
		    (view-me self slot))
		   (t
		    (let ((edit (edit-me self slot)))
		      edit))))))
	   (reverse (reverse (get-template-class self))))
	  (<:p (<:input :type "submit" :value "Save"
;; 			:disabled t
			:onclick (lifte (do-save1 self)))
	       (<:input :type "button" :value "Cancel"
			:onclick (lifte (do-cancel self)))))))

(defmethod/remote remove-child-nodes ((self <core:crud))
  (mapcar-cc (lambda (a) (.remove-child self a))
	     (reverse (slot-value self 'child-nodes))))

(defmethod/remote destroy ((self <core:crud))
  (remove-css (crud-css self))
  (remove-class self "crud")
  (remove-child-nodes self)
  (delete-slots self 'instance 'template-class 'crud-css 'title)
  (call-next-method self))

(defmethod/remote init ((self <core:crud))
  (load-css (crud-css self))
  (add-class self "crud")
  (append self (<:h2 (title self)))
  (append self (setf (_template self) (view-template self))))

(defmacro defwebcrud (name supers slots &rest rest)
  `(progn
     (defcomponent ,name (,@supers <core:crud)
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
	       slots :initial-value nil))
	 ,@(cdr (flatten1 rest))))))
