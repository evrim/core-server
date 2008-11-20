(in-package :core-server)

(defcomponent <core:crud (<:div)
  ((instance :host remote :initform nil)))

(defmethod/remote get-type ((component <core:crud) field)
  (slot-value (slot-value (get-instance component) field) "type"))

(defmethod/remote get-value ((component <core:crud) field)
  (if (eq "password" (get-type component field))
      "*****************"
      (slot-value (slot-value (get-instance component) field) "value")))

(defmethod/remote set-value ((component <core:crud) field value)
  (setf (slot-value (slot-value (get-instance component) field) "value")
	value)
  (update-slot component field value)
  value)

(defmethod/local update-slot ((component <core:crud) slot-name value)
  (answer (cons slot-name value)))

(defmethod/remote edit-me ((component <core:crud) slot)
  (with-slots (name value type) slot
    (cond
      ((eq type "password")
       (<:input :type "password" :name name :value value))
      (t
       (<:input :type "text" :name name :value value)))))

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
	      (with-slots (name value label) slot
		(let ((view (view-me component slot))
		      (edit (edit-me component slot)))
		  (labels ((edit-it ()
			     (console.debug "edit")
			     (show edit)
			     (hide view)
			     (edit.focus))
			   (save-it ()
			     (console.debug "save")
			     (set-value component name edit.value)
			     (setf view.inner-h-t-m-l (get-value component name))
			     (show view)
			     (hide edit))
			   (cancel-it ()
			     (console.debug "cancel")
			     (setf edit.value value)
			     (show view)
			     (hide edit)))
		    (hide edit)
		    (setf view.onclick (lambda (e)
					 (edit-it)
					 ;; (setf edit.onblur (lambda (e) (save-it)))
					 false)
			  edit.onkeydown (lambda (e)
					   (let ((keycode))
					     (setf keycode (if window.even
							       event.key-code
							       e.key-code))
					     (cond
					       ((eq 27 keycode) ;; escape
						(cancel-it))
					       ((eq 13 keycode) ;; enter
						(save-it))
					       (t
						(console.debug (+ "key:" keycode)))))
					   ;; 					  (setf edit.onblur (lambda (e) false))
					   true))
		    (with-field (+ label ":") (list view edit))))))
	    (get-instance component)))
   (<:p (<:b "Note: ") "Click on the field to modify, press enter to save or escape to cancel.")))

(defmethod/remote init ((component <core:crud))
  (.append-child component (template component)))