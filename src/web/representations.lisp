(in-package :core-server)

;; -------------------------------------------------------------------------
;; Slot Representations
;; -------------------------------------------------------------------------
(defcomponent slot-representations ()
  ())

(defmethod/remote get-slot-view ((self slot-representations) _slot _instance)
  (with-slots (name remote-type options size reader) _slot
    (let ((_value (if reader
		      (reader _instance)
		      (slot-value _instance name))))
      (cond
	((eq "password" remote-type) "********")
	((or (eq "number" remote-type) (eq "email" remote-type)) _value)
	((and _value (eq "date" remote-type))
	 (take 10 (date-to-string (lisp-date-to-javascript _value))))
	((and _value (eq "timestamp" remote-type))
	 (date-to-string (lisp-date-to-javascript _value)))
	((eq remote-type "checkbox")
	 (<:input :type "checkbox" :checked (if _value t nil) :disabled t))
	((eq "html" remote-type)
	 (cond
	   ((null _value) nil)
	   ((not (null (slot-value _value 'tag-name))) _value)
	   (t (let ((div (<:div)))
		(setf (slot-value div 'inner-h-t-m-l) _value)
		div))))
	((eq "select" remote-type)
	 _value)
	((or (eq "multiple-select" remote-type)
	     (eq "multiple-checkbox" remote-type))
	 (reduce-cc (lambda (acc atom) (+ acc ", " atom))
		    (cdr _value) (car _value)))	
	((null _value) "Not set")
	(t _value)))))

(defmethod %get-slot-editor ((self slot-representations) remote-type)
  (cond
    ((equal "password" remote-type) (<core:password-input))
    ((equal "number" remote-type) (<core:number-value-input))
    ((equal "email" remote-type) (<core:email-input))
    ((equal "checkbox" remote-type) (<core:checkbox))
    ((or (equal "date" remote-type) (equal "timestamp" remote-type))
     (<core:date-time-input))
    ((equal "html" remote-type)
     (<core:ckeditor :config core-server::+ckeditor-simple-config+))
    ((equal "select" remote-type) (<core:select-input))
    ((or (equal "multiple-select" remote-type)
	 (equal "multipleSelect" remote-type))
     (<core:multiple-select-input))
    ((or (equal "multiple-checkbox" remote-type)
	 (equal "multipleCheckbox" remote-type))
     (<core:multiple-checkbox))
    (t nil)))

;; -------------------------------------------------------------------------
;; Helper For Defwebcrud & Deftable Macros
;; -------------------------------------------------------------------------
(defparameter +remote-types+
  '(nil password number email date timestamp html select
    multiple-select multiple-checkbox checkbox))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun %generate-template-class (slots)
    (flet ((process-slot (&key name remote-type reader label read-only
			       options size min-length)
	     (assert (member remote-type +remote-types+ :test #'string=)
		     nil "Remote-type ~A should be one of ~A" remote-type
		     +remote-types+)
	     (list :name (symbol-to-js name)
		   :label label		   
		   :remote-type (if (and remote-type (symbolp remote-type))
				    (symbol-to-js remote-type)
				    remote-type)
		   :reader reader
		   :read-only read-only
		   :options options
		   :size (or size 5)
		   :min-length (or min-length 6))))
      `(jobject
	,@(reduce
	   (lambda (acc slot)
	     (append acc
		     `(,(make-keyword (car slot))
			(jobject ,@(apply #'process-slot (cons :name slot))))))
	   slots :initial-value nil)))))
