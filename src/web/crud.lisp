(in-package :core-server)

;; Fixme: Date lazim, Integer->Date, Text Editor

;; -------------------------------------------------------------------------
;; Crud Component
;; -------------------------------------------------------------------------
(defcomponent <core:crud (<:div)
  ((instance :host remote)
   (template-class :host remote)
   (title :host remote :initform "Please set crud title")
   (crud-css :host remote :initform "http://www.coretal.net/style/crud.css")))

(defmethod/remote get-template-class ((self <core:crud))
  (or (slot-value self 'template-class)
      (and (instance self) (slot-value (instance self) 'core-class))))

(defmethod/remote get-type ((self <core:crud) slot)  
  (slot-value slot 'type))

(defmethod/remote get-value ((self <core:crud) slot)
  (let ((_instance (instance self))
	(_reader (slot-value slot 'reader)))
    (if (eq "password" (get-type self slot))
	"*****************"
	(if _reader
	    (_reader _instance)
	    (slot-value _instance (slot-value slot 'name))))))

(defmethod/remote set-value ((self <core:crud) field value)
  (answer-component self (list field value)))

(defmethod/remote edit-me ((self <core:crud) slot)  
  (with-slots (name type options) slot
    (let ((value (slot-value (instance self) name)))
      (cond
	((eq type "select")
	 (<:select
	  (mapcar-cc (lambda (option)
		       (if (eq option value)
			   (<:option :selected t option)
			   (<:option option)))
		     options)))
	((eq type "password")
	 (<:input :type "password" :name name :value value))
	(t
	 (<:input :type "text" :name name :value value))))))

(defmethod/remote view-me ((self <core:crud) slot)
  (let ((value (get-value self slot)))
    (if (slot-value slot 'read-only)
	(<:span value)
	(<:a (if (null value)
		 (+ "Click to set " (slot-value slot 'name))
		 value)))))

(defmethod/remote template ((self <core:crud))
  (<:div :class "crud"
	 (<:form
	  (mapcar-cc
	   (lambda (slot)
	     (with-slots (name label) slot
	       (let ((view (view-me self slot))
		     (edit (edit-me self slot)))
		 (labels ((edit-it ()
			    (show edit)
			    (hide view)
			    (let ((focus (slot-value edit 'focus)))
			      (focus)))
			  (save-it ()
			    ;;                    (setf (slot-value view 'inner-h-t-m-l) (slot-value edit 'value))
			    ;; 		      (show view)
			    ;; 		      (hide edit)
			    ;; 		      (set-value self name (slot-value edit 'value))
			    (let ((obj (jobject)))
			      (setf (slot-value obj name) (slot-value edit 'value))
			      (answer-component self obj)))
			  (cancel-it ()
			    (setf (slot-value edit 'value) (get-value self name))
			    (show view)
			    (hide edit)))
		   (hide edit)
		   (if (not (slot-value slot 'read-only))
		       (setf (slot-value view 'onclick)
			     (lifte (call/cc edit-it))
			     (slot-value edit 'onkeydown)
			     (event (e)
				    (let ((keycode (slot-value e 'key-code)))
				      (cond
					((eq 27 keycode) ;; escape
					 (make-web-thread cancel-it)
					 false)
					((eq 13 keycode) ;; enter
					 (make-web-thread save-it)
					 false)
					(t t)))
				    ;; (setf edit.onblur (lambda (e) false))
				    )))
		   (with-field (+ label ":") (list view edit))))))
	   (reverse (get-template-class self))))
	 (<:p (<:b "Note: ") "Click on the field to modify, press enter to save or escape to cancel.")))

(defmethod/remote destroy ((self <core:crud))
  (remove-css (crud-css self))
  (remove-class self "crud")
  (mapcar-cc (lambda (a) (.remove-child self a))
	     (reverse (slot-value self 'child-nodes)))
  (delete-slots self 'instance 'template-class 'crud-css 'title)
  (call-next-method self))

(defmethod/remote init ((self <core:crud))
  (load-css (crud-css self))
  (add-class self "crud")
  (append self (<:h2 (title self)))
  (append self (template self)))

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
