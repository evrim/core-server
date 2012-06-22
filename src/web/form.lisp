(in-package :core-server)

;; +-------------------------------------------------------------------------
;; | Form/Input Components
;; +-------------------------------------------------------------------------

;; +-------------------------------------------------------------------------
;; | Validting HTML Input
;; +-------------------------------------------------------------------------
(defcomponent <core:validating-input (<:input cached-component)
  ((validation-span-id :host remote :initform nil)
   (valid-class :host remote :initform "valid")
   (invalid-class :host remote :initform "invalid")
   (valid :host remote :initform nil))
  (:default-initargs :value "" :type "text"))

(defmethod/remote set-validation-message ((self <core:validating-input)
					  result msg)
  (awhen (validation-span-id self)
    (awhen (document.get-element-by-id it)
      (cond
	(result
	 (add-class it (valid-class self))
	 (remove-class it (invalid-class self)))
	(t
	 (add-class it (invalid-class self))
	 (remove-class it (valid-class self))))
      (setf (slot-value it 'inner-h-t-m-l) msg))))

(defmethod/remote enable-or-disable-form ((self <core:validating-input))
  (awhen (slot-value self 'form) ;; not avail at first run-validate
    (let* ((form it)
	   (valid
	    (reduce-cc
	     (lambda (acc input)
	       (cond
		 ((or (eq (typeof (slot-value input 'valid)) "undefined")
		      (slot-value input 'disabled))
		  acc)
		 (t (and acc (valid input)))))
	     (append (reverse (.get-elements-by-tag-name form "INPUT"))
		     (append
		      (reverse (.get-elements-by-tag-name form "SELECT"))
		      (reverse (.get-elements-by-tag-name form "TEXTAREA"))))
	     t)))
      (mapcar (lambda (input)
		(when (and (slot-value input 'type)
			   (eq "SUBMIT"
			       (.to-upper-case (slot-value input 'type))))
		  (cond
		    (valid
		     (setf (slot-value input 'disabled) false)
		     (remove-class input "disabled"))
		    (t
		     (setf (slot-value input 'disabled) true)
		     (add-class input "disabled"))))
		nil)
	      (self.form.get-elements-by-tag-name "INPUT")))))

(defmethod/remote validate ((self <core:validating-input))
  t)

(defmethod/remote _validate ((self <core:validating-input))
  (validate self))

(defmethod/remote run-validator ((self <core:validating-input))
  (let ((result (_validate self)))
    (cond
      ((typep result 'string)
       (setf (valid self) nil)
       (set-validation-message self nil result)
       (add-class self (invalid-class self))
       (remove-class self (valid-class self))
       (enable-or-disable-form self))
      (t
       (setf (valid self) t)
       (set-validation-message self t "OK")
       (add-class self (valid-class self))
       (remove-class self (invalid-class self))
       (enable-or-disable-form self)))))

(defmethod/remote onchange ((self <core:validating-input) e)
  (run-validator self) t)

(defmethod/remote onkeydown ((self <core:validating-input) e)
  (run-validator self) t)

(defmethod/remote onkeyup ((self <core:validating-input) e)
  (run-validator self) t)

(defmethod/remote get-input-value ((self <core:validating-input))
  (cond
    ((eq "string" (typeof (validate self)))
     (throw (new (*error (+ "get-input-value called although"
			    " input is invalid. Value:"
			    (slot-value self 'value))))))
    (t
     (slot-value self 'value))))

(defmethod/remote init ((self <core:validating-input))
  (flet ((do-validate (f)
	   (if (slot-value self 'form)
	       (run-validator self)
	       (make-web-thread (lambda () (f f))))))
    (do-validate do-validate)))

;; +-------------------------------------------------------------------------
;; | Default Value HTML Input
;; +-------------------------------------------------------------------------
(defcomponent <core:default-value-input (<core:validating-input)
  ((default-value :host remote :initform nil)))

(defmethod/remote adjust-default-value ((self <core:default-value-input))
  (cond
    ((equal self.default-value self.value)
     (setf self.value ""))
    ((equal "" self.value)
     (setf self.value self.default-value))))

(defmethod/remote onfocus ((self <core:default-value-input) e)
  (adjust-default-value self))

(defmethod/remote onblur ((self <core:default-value-input) e)
  (adjust-default-value self))

(defmethod/remote validate ((self <core:default-value-input))
  (if (and (or (and (eq "INPUT" (slot-value self 'tag-name))
		    (or (eq "" (slot-value self 'type))
			(eq "TEXT" (.to-upper-case (slot-value self 'type)))
			(eq "PASSWORD" (.to-upper-case (slot-value self 'type)))))
	       (eq "TEXTAREA" (slot-value self 'tag-name)))
	   (eq (slot-value self 'default-value) (slot-value self 'value)))
      (_"This field is required.")
      (call-next-method self)))

(defmethod/remote reset-input-value ((self <core:default-value-input))
  (setf (slot-value self 'value) "")
  (adjust-default-value self)
  (run-validator self)
  (set-validation-message self nil ""))

(defmethod/remote init ((self <core:default-value-input))
  (if (null (slot-value self 'default-value))
      (setf (slot-value self 'default-value) (slot-value self 'value)))

  (setf (slot-value self 'default-value)
	(_ (slot-value self 'default-value)))
  
  (if (or (null (slot-value self 'value)) (eq "" (slot-value self 'value)))
      (setf (slot-value self 'value) (slot-value self 'default-value)))

  (call-next-method self))

;; +-------------------------------------------------------------------------
;; | Email HTML Component
;; +-------------------------------------------------------------------------
(defcomponent <core:email-input (<core:default-value-input)
  ()
  (:default-initargs :default-value "Enter email"))

(defmethod/remote validate-email ((self <core:email-input))
  (let ((expression (regex "/^[a-zA-Z0-9._-]+@([a-zA-Z0-9.-]+\.)+[a-zA-Z0-9.-]{2,4}$/")))
    (if (.test expression self.value)
	t
	(_"Your email is invalid."))))

(defmethod/remote validate ((self <core:email-input))
  (let ((result (call-next-method self)))
    (if (typep result 'string)
	result
	(validate-email self))))

;; +-------------------------------------------------------------------------
;; | Password HTML Component
;; +-------------------------------------------------------------------------
(defcomponent <core:password-input (<core:default-value-input)
  ((min-length :initform 6 :host remote))
  (:default-initargs :type "password" :default-value "Enter password"))

(defmethod/remote validate-password ((self <core:password-input))
  (cond
    ((or (null self.value) (< self.value.length self.min-length))
     (_"Your password is too short."))
    (t
     t)))

(defmethod/remote validate ((self <core:password-input))
  (let ((result (call-next-method self)))
    (if (typep result 'string)
	result
	(validate-password self))))

;; +-------------------------------------------------------------------------
;; | Required Input
;; +-------------------------------------------------------------------------
(defcomponent <core:required-value-input (<core:default-value-input)
  ())

(defmethod/remote validate-required-value ((self <core:required-value-input))
  (cond
    ((or (equal (slot-value self 'type) "checkbox")
	 (equal (slot-value self 'type) "radio"))
     (if (slot-value self 'checked)
	 t
	 (_"This box must be checked.")))
    (t
     (let ((_val (slot-value self 'value)))
       (if (or (null _val) (eq _val ""))
	   (_"This field is required.")
	   t)))))

(defmethod/remote validate ((self <core:required-value-input))
  (let ((result (call-next-method self)))
    (if (typep result 'string)
	result
	(validate-required-value self))))

;; +-------------------------------------------------------------------------
;; | Number Input
;; +-------------------------------------------------------------------------
(defcomponent <core:number-value-input (<core:default-value-input)
  ()
  (:default-initargs :default-value "Enter a number"))

;; FIXME: validate loses cc.
(defmethod/remote get-input-value ((self <core:number-value-input))
  ;; (if (not (eq "string" (typeof (validate self))))
  ;;     (parse-float (slot-value self 'value)))
  (parse-float (slot-value self 'value)))

(defmethod/remote validate-number ((self <core:number-value-input))
  (let ((_val (slot-value self 'value)))
    (try
     (if (eq (typeof (eval _val)) "number")
	 t
	 (_ "%1 is not a number." _val))
     (:catch (e)
       (_"%1 is not a number." _val)))))

(defmethod/remote validate ((self <core:number-value-input))
  (let ((result (call-next-method self)))
    (if (typep result 'string)
	result
	(validate-number self))))

;; -------------------------------------------------------------------------
;; Date Input
;; -------------------------------------------------------------------------
(defcomponent <core:date-time-input (<core:validating-input supply-jquery-ui)
  ((jquery-date-time-picker-uri :host remote
				:initform +jquery-date-time-picker-uri+)
   (jquery-date-time-picker-css :host remote
				:initform +jquery-date-time-picker-css+)
   (default-value :host remote)
   (show-time :host remote :initform t))
  (:default-initargs :default-value "Enter a date"))

(defmethod/remote get-input-value ((self <core:date-time-input))
  (.datetimepicker (j-query self) "getDate"))

(defmethod/remote init ((self <core:date-time-input))
  (call-next-method self)
  (load-jquery-ui self)
  (load-css (jquery-date-time-picker-css self))
  (load-javascript (jquery-date-time-picker-uri self)
		   (lambda () (not (null j-query.fn.datetimepicker))))  
  (.datetimepicker (j-query self)
		   (jobject :time-format "h:m" :separator " @ "
			    :show-timepicker (if (show-time self)
						 t
						 false)))
  (.datetimepicker (j-query self)
		   "setDate" (or (if (typep (default-value self) 'string)
				     (setf (default-value self)
					   (new (*date (default-value self))))
				     (default-value self))
				 (new (*date)))))

;; -------------------------------------------------------------------------
;; Select Input
;; -------------------------------------------------------------------------
(defcomponent <core:select-input (<:select)
  ((current-value :host remote)
   (option-values :host remote)
   (item-equal-p :host remote :initform nil)
   (_value-cache :host remote :initform nil)))

(defmethod/remote get-input-value ((self <core:select-input))
  (slot-value (_value-cache self) (slot-value self 'value)))

(defmethod/remote init ((self <core:select-input))
  (setf (_value-cache self) (jobject))
  (let ((equal-fun (or (item-equal-p self) (lambda (a b) (eq a b))))
	(hash-list (mapcar (lambda (a) (random-string))
			   (seq (slot-value (option-values self) 'length)))))
    (mapcar (lambda (a) (append self a))
	    (mapcar
	     (lambda (a)
	       (destructuring-bind (hash data) a
		 ;; (_debug (list "a" a "hash" hash "data" data))
		 (cond
		   ((atom data)
		    (setf (slot-value (_value-cache self) hash) data)
		    (<:option :selected (call/cc equal-fun
						 (current-value self)
						 data)
			      :value hash (_ data)))
		   (t
		    (destructuring-bind (name value) data
		      ;; (_debug (list 2 "name" name "value" value))
		      (setf (slot-value (_value-cache self) hash) value)
		      (<:option :selected (call/cc equal-fun
						   (current-value self)
						   value)
				:value hash (_ name)))))))
	     (mapcar2 (lambda (a b) (list b a))
		      (option-values self)
		      hash-list)))))

;; -------------------------------------------------------------------------
;; Multiple Select Input
;; -------------------------------------------------------------------------
(defcomponent <core:multiple-select-input (<core:select-input)
  ()
  (:default-initargs :size 5 :multiple "multiple"))

(defmethod/remote get-input-value ((self <core:multiple-select-input))
  (reverse-cc
   (reduce-cc (lambda (acc option)
		(if (slot-value option 'selected)
		    (cons (slot-value (_value-cache self)
				      (slot-value option 'value))
			  acc)
		    acc))
	      (slot-value self 'options) nil)))

(defmethod/remote init ((self <core:multiple-select-input))
  (setf (slot-value self 'multiple) "multiple")
  (call-next-method self))

;; -------------------------------------------------------------------------
;; Multiple Checkbox
;; -------------------------------------------------------------------------
(defcomponent <core:multiple-checkbox (<:div)
  ((current-value :host remote)
   (option-values :host remote)
   (item-equal-p :host remote :initform nil)
   (_value-cache :host remote :initform nil)))

(defmethod/remote get-input-value ((self <core:multiple-checkbox))
  (reverse-cc
   (reduce-cc (lambda (acc checkbox)
		(if (slot-value checkbox 'checked)
		    (cons (slot-value (_value-cache self)
				      (slot-value checkbox 'value))
			  acc)
		    acc))
	      (node-search (lambda (a)
			     (with-slots (type) a
			       (and type
				    (eq "CHECKBOX" (.to-upper-case type)))))
			   self) nil)))

(defmethod/remote init ((self <core:multiple-checkbox))
  (setf (_value-cache self) (jobject))
  (let ((equal-fun (or (item-equal-p self) (lambda (a b) (eq a b))))
	(hash-list (mapcar (lambda (a) (random-string))
			   (seq (slot-value (option-values self) 'length)))))
    (mapcar (lambda (a) (append self a))
	    (mapcar
	     (lambda (a)
	       (destructuring-bind (hash data) a
		 ;; (_debug (list "a" a "hash" hash "data" data))
		 (cond
		   ((atom data)
		    (setf (slot-value (_value-cache self) hash) data)
		    (<:label :class "block" :for hash
			     (<:input :type "checkbox"
				      :checked (call/cc equal-fun
							(current-value self)
							data)
				      :value hash
				      :id hash)
			     (_ data)))
		   (t
		    (destructuring-bind (name value) data
		      ;; (_debug (list 2 "name" name "value" value))
		      (setf (slot-value (_value-cache self) hash) value)
		      (<:label :for hash :class "block"
			       (<:input :type "checkbox"
					:id hash
					:checked (call/cc equal-fun
							  (current-value self)
							  value)
					:value hash)
			       (_ name)))))))
	     (mapcar2 (lambda (a b) (list b a))
		      (option-values self)
		      hash-list)))))

;; -------------------------------------------------------------------------
;; Radio Group
;; -------------------------------------------------------------------------
;; NOTE: Used in Coretal Sidebar
(defcomponent <core:radio-group (<:div)
  ((items :host remote)
   (_result :host remote)))

(defmethod/remote get-input-value ((self <core:radio-group))
  (if (_result self)
      (.index-of (items self) (_result self))
      (throw (new (*error (+ "get-input-value called although"
			    " input is invalid. (radiogroup)"))))))

(defmethod/remote init ((self <core:radio-group))
  (let ((rnd (random-string)))
    (+ rnd "")
    (labels ((match (a)
	       (with-slots (type tag-name) a
		 (let ((type (and type (.to-upper-case type)))
		       (tag-name (and tag-name (.to-upper-case tag-name))))
		   (and tag-name type (eq tag-name "INPUT")
			(or (eq type "TEXT") (eq type "SELECT"))
			(not (eq type "RADIO"))))))
	     (input-nodes (a)
	       (cond
		 ((null a) nil)
		 ((call/cc match a) (list a))
		 (t (node-search match a))))
	     (disable-inputs (a)
	       (let ((inputs (call/cc input-nodes a)))
		 (_debug (list "inputs" inputs a))
		 (mapcar (lambda (a) (setf (slot-value a 'disabled) t))
			 inputs)
		 inputs))
	     (enable-input (a) (setf (slot-value a 'disabled) false))
	     (handle-event (item)
	       (let ((payload (nth 1 item)))
		 (_debug (list "payload"payload))
		 (setf (_result self) item)
		 (mapcar-cc
		  (lambda (a)
		    (_debug (list "a1" a))
		    (mapcar-cc (lambda (a)
				 (_debug (list "a2" a))
				 (setf (slot-value a 'disabled) t)
				 (_debug (list "disabling" a)))
			       (call/cc input-nodes a)))
		  (mapcar-cc (lambda (a) (car (cdr a))) (items self)))
		 (mapcar-cc (lambda (a)
			      (_debug (list "a3" a))
			      (setf (slot-value a 'disabled) false)
			      (_debug (list "enabling" a)))
			    (call/cc input-nodes payload)))))
      (mapcar-cc (lambda (a) (append self a))
		 (mapcar-cc
		  (lambda (item)
		    (destructuring-bind (title payload checked) item
		      (with-field
			  (list (<:input :checked (and checked t)
					 :type "radio" :name rnd
					 :onclick
					 (event (e)
					   (let ((self this))
					     (with-call/cc
					       (call/cc handle-event item)))
					   true))
				" " (_ title))
			(progn (if (not checked)
				   (disable-inputs payload))
			       payload))))
		  (items self))))))