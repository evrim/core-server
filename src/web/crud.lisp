(in-package :core-server)

;; Date lazim, Integer->Date
;; Text Editor
(defcomponent <core:crud (<:div)
  ((instance :host none :initform nil)
   (template-class :host none :initform nil)))

(defmethod/local _get-instance ((component <core:crud))
  (if (slot-value component 'instance)
      (with-slots (instance template-class) component
	(let ((template-class (if (symbolp template-class) (class+.find template-class) template-class)))
	  (object->jobject (slot-value component 'instance)
			   template-class)))
      nil))

(defmethod/remote get-instance ((component <core:crud))
  (if (slot-value component 'instance)
      (slot-value component 'instance)
      (setf (slot-value component 'instance) (_get-instance component))))

(defmethod/remote get-class ((component <core:crud))
  (slot-value (slot-value component 'instance) 'class))

(defmethod/remote get-type ((component <core:crud) field)
  (slot-value (slot-value (get-class component) field) 'type))

(defmacro/js instance (crud)
  `(slot-value ,crud 'instance))

(defmethod/remote get-value ((component <core:crud) field)
  (if (eq "password" (get-type component field))
      "*****************"
      (slot-value (instance component) field)))

(defmethod/local update-slot ((component <core:crud) slot-name value)
  (if (instance component)
      (answer
       (cons
	(slot-definition-name
	 (class+.find-slot (template-class component) (string-upcase slot-name)))
	value))
      nil))

(defmethod/remote set-value ((component <core:crud) field value)
  (setf (slot-value (instance component) field) value)
  (answer-component component (update-slot component field value)))

(defmethod/remote edit-me ((component <core:crud) slot)
  (let ((value (get-value component (slot-value slot 'name))))
    (with-slots (name type) slot
      (cond
	((eq type "password")
	 (<:input :type "password" :name name :value value))
	(t
	 (<:input :type "text" :name name :value value))))))

(defmethod/remote view-me ((component <core:crud) slot)
  (let ((value (get-value component (slot-value slot 'name))))
    (<:a :href "#"
	 (if (null value)
	     (+ "Click to set " (slot-value slot 'name))
	     value))))

(defmethod/remote template ((component <core:crud))  
  (<:div :class "crud"
   (<:form
    (mapcar (lambda (slot)
	      (with-slots (name label) slot
		(let ((view (view-me component slot))
		      (edit (edit-me component slot)))
		  (labels ((edit-it ()
			     (debug "edit")
			     (show edit)
			     (hide view)
			     (let ((focus (slot-value edit 'focus)))
			       (focus)))
			   (save-it ()
			     (debug "save")
			     (setf (slot-value view 'inner-h-t-m-l) (slot-value edit 'value))
			     (show view)
			     (hide edit)
			     (set-value component name (slot-value edit 'value)))
			   (cancel-it ()
			     (debug "cancel")
			     (setf (slot-value edit 'value) (get-value component name))
			     (show view)
			     (hide edit)))
		    (hide edit)
		    (setf (slot-value view 'onclick)
			  (lambda (e)
			    (make-web-thread edit-it)
			    ;; (setf edit.onblur (lambda (e) (save-it)))
			    false)
			  (slot-value edit 'onkeydown)
			  (lambda (e)
			    (let ((keycode (slot-value e 'key-code)))
			      (cond
				((eq 27 keycode) ;; escape
				 (make-web-thread cancel-it)
				 false)
				((eq 13 keycode) ;; enter
				 (make-web-thread save-it)
				 false)
				(t
				 (debug (+ "key:" keycode))
				 true)))
			    ;; 					  (setf edit.onblur (lambda (e) false))
			    ))
		    (with-field label (list view edit))))))
	    (get-class component)))
   (<:p (<:b "Note: ") "Click on the field to modify, press enter to save or escape to cancel.")))

(defmethod/remote init ((component <core:crud))
  (get-instance component)
  (let ((template (template component)))
    (setf (slot-value component 'inner-h-t-m-l) nil)
    (.append-child component template))
  component)