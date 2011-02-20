(in-package :core-server)

;; +-------------------------------------------------------------------------
;; | Form/Input Components
;; +-------------------------------------------------------------------------

;; +-------------------------------------------------------------------------
;; | Validting HTML Input
;; +-------------------------------------------------------------------------
(defcomponent <core:validating-input (<:input)
  ((validation-span-id :host remote :initform nil)
   (valid-class :host remote :initform "valid")
   (invalid-class :host remote :initform "invalid")
   (valid :host remote :initform nil))
  (:default-initargs :value ""))

(defmethod/remote set-validation-message ((self <core:validating-input) msg)
  (when (not (null (validation-span-id self)))
    (let ((element (document.get-element-by-id (validation-span-id self))))
      (when element
	(setf (slot-value element 'inner-h-t-m-l) msg)))))

(defmethod/remote enable-or-disable-form ((self <core:validating-input))
  (when (slot-value self 'form) ;; not avail at first run-validate
    (let ((valid (reduce-cc (lambda (acc input)
			      (if (eq "undefined" (typeof (slot-value input 'valid)))
				  acc
				  (and acc input.valid)))
			    (self.form.get-elements-by-tag-name "INPUT")
			    t)))    
      (mapcar (lambda (input)
		(when (and input.type (eq "SUBMIT" (input.type.to-upper-case)))
		  (if valid
		      (setf input.disabled false)
		      (setf input.disabled true)))
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
       (set-validation-message self result)
       (add-class self (invalid-class self))
       (remove-class self (valid-class self))
       (enable-or-disable-form self))
      (t
       (setf (valid self) t)
       (set-validation-message self "OK")
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
     (throw (new (*error (+ "get-input-value called although input is invalid. Value:"
			    (slot-value self 'value))))))
    (t
     (slot-value self 'value))))

(defmethod/remote init ((self <core:validating-input))
  (run-validator self))

;; +----------------------------------------------------------------------------
;; | Default Value HTML Input
;; +----------------------------------------------------------------------------
(defcomponent <core:default-value-input (<core:validating-input)
  ((default-value :host remote :initform nil))
  (:default-initargs :value ""))

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

(defmethod/remote _validate ((self <core:default-value-input))
  (if (equal (slot-value self 'default-value) (slot-value self 'value))
      ""
      (call-next-method self)))

(defmethod/remote init ((self <core:default-value-input))
  (if (null (slot-value self 'default-value))
      (setf (slot-value self 'default-value) (slot-value self 'value)))

  (if (or (null (slot-value self 'value))
	  (eq "" (slot-value self 'value)))
      (setf (slot-value self 'value) (slot-value self 'default-value)))

  (call-next-method self))

;; +----------------------------------------------------------------------------
;; | Email HTML Component
;; +----------------------------------------------------------------------------
(defcomponent <core:email-input (<core:default-value-input)
  ())

(defmethod/remote validate-email ((self <core:email-input))
  (let ((expression (regex "/^[a-zA-Z0-9._-]+@([a-zA-Z0-9.-]+\.)+[a-zA-Z0-9.-]{2,4}$/")))
    (if (.test expression self.value)
	t
	"Your email is invalid")))

(defmethod/remote validate ((self <core:email-input))
  (validate-email self))

;; +----------------------------------------------------------------------------
;; | Password HTML Component
;; +----------------------------------------------------------------------------
(defcomponent <core:password-input (<core:default-value-input)
  ((min-length :initform 6 :host remote))
  (:default-initargs :type "password"))

(defmethod/remote validate-password ((self <core:password-input))
  (cond
    ((or (null self.value) (< self.value.length self.min-length))
     "Your password is too short.")
    (t
     t)))

(defmethod/remote validate ((self <core:password-input))
  (validate-password self))

;; +-------------------------------------------------------------------------
;; | Required Input
;; +-------------------------------------------------------------------------
(defcomponent <core:required-value-input (<core:default-value-input)
  ())

(defmethod/remote validate ((self <core:required-value-input))
  (let ((_val (slot-value self 'value)))
    (if (or (null _val) (eq _val ""))
	"This field is required."
	t)))

;; +-------------------------------------------------------------------------
;; | Number Input
;; +-------------------------------------------------------------------------
(defcomponent <core:number-value-input (<core:default-value-input)
  ()
  (:default-initargs :default-value "Enter a number"))

(defmethod/remote get-input-value ((self <core:number-value-input))
  (cond
    ((eq "string" (typeof (validate self)))
     nil)
    (t
     (eval (+ (slot-value self 'value) " ")))))

(defmethod/remote validate ((self <core:number-value-input))
  (let ((_val (slot-value self 'value)))
    (try
     (if (eq (typeof (eval _val)) "number")
	 t
	 (+ _val " is not a number."))
     (:catch (e)
       (+ _val " is not a number.")))))