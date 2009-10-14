(in-package :core-server)

(defcomponent <core:crud (<:div)
  ((instance :host remote :initform nil)))

(defmethod/remote get-class ((component <core:crud))
  (slot-value (instance self) 'class))

(defmethod/remote get-type ((component <core:crud) field)
  (slot-value (slot-value (get-class self) field) 'type))

(defmethod/remote get-value ((component <core:crud) field)
  (if (eq "password" (get-type component field))
      "*****************"
      (slot-value (instance self) field)))

(defmethod/remote set-value ((component <core:crud) field value)
  (setf (slot-value (instance component) field)value)
  ;; (update-slot component field value)
  value)

(defmethod/local update-slot ((component <core:crud) slot-name value)
  (answer (cons slot-name value)))

(defmethod/remote edit-me ((component <core:crud) slot)
  (let ((value (get-value self (slot-value slot 'name))))
    (with-slots (name type) slot
      (cond
	((eq type "password")
	 (<:input :type "password" :name name :value value))
	(t
	 (<:input :type "text" :name name :value value))))))

(defmethod/remote view-me ((component <core:crud) slot)
  (let ((value (get-value self (slot-value slot 'name))))
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
			     (console.debug "edit")
			     (show edit)
			     (hide view)
			     (console.debug edit)
			     (make-web-thread
			      (lambda ()
				(try
				 (let ((focus (slot-value edit 'focus)))
				   (console.debug focus)
				   (focus))
				 (:catch (e) (console.debug e))))))
			   (save-it ()
			     (console.debug "save")
			     (set-value component name (slot-value edit 'value))
			     (setf (slot-value view 'inner-h-t-m-l) (get-value component name))
			     (show view)
			     (hide edit))
			   (cancel-it ()
			     (console.debug "cancel")
			     (setf (slot-value edit 'value) (get-value self name))
			     (show view)
			     (hide edit)))
		    (hide edit)
		    (setf (slot-value view 'onclick)
			  (event (e)
			    (with-call/cc (call/cc edit-it))
			    ;; (setf edit.onblur (lambda (e) (save-it)))
			    false)
			  (slot-value edit 'onkeydown)
			  (event (e)
			    (with-call/cc
			      (let ((keycode e.key-code;; (if window.event
					     ;; 	 window.event.key-code
					     ;; 	 e.key-code)
				      ))
				(cond
				  ((eq 27 keycode) ;; escape
				   (call/cc cancel-it))
				  ((eq 13 keycode) ;; enter
				   (call/cc save-it))
				  (t
				   (console.debug (+ "key:" keycode))))))
			    ;; 					  (setf edit.onblur (lambda (e) false))
			    true))
		    (with-field label (list view edit))))))
	    (get-class component)))
   (<:p (<:b "Note: ") "Click on the field to modify, press enter to save or escape to cancel.")))

(defmethod/remote init ((component <core:crud))
  (.append-child component (template component)))