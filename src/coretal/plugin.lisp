(in-package :Core-server)

(defclass+ plugin+ (component+)
  ())

(defclass+ plugin ()
  ()
  (:metaclass plugin+))

(defmacro defplugin (name supers slots &rest rest)
  `(defcomponent ,name (plugin ,@supers)
     ((,(plugin-instance-slot-name name) :host remote)
      ,@slots)
     ,@rest
     (:metaclass plugin+)))

(defmethod component.instance-id ((self plugin))
  (setf (slot-value self (plugin-instance-slot-name (class-name (class-of self))))
	(call-next-method self)))

(eval-when (:compile-toplevel :load-toplevel)
  (defun plugin-instance-slot-name (name)
    (intern (format nil "~A-INSTANCE-ID" name) (symbol-package name)))

  (defmethod component+.local-funkall-morphism ((class plugin+) name self args)
    (let* ((metaclass (class-name (class-of class)))
	   (morphism (component+.morphism-function-name class name))
	   (arg-names (extract-argument-names args :allow-specializers t))
	   (slot-name (plugin-instance-slot-name (class-name class))))
      `(defmethod ,morphism ((self ,metaclass))
	 `(method ,',arg-names
	    (with-slots (session-id ,',slot-name) self
	      (funkall self (+ "?s:" session-id
			       "$k:" ,',slot-name
			       "$method:" ,',(symbol-name name))
		       (create
			,@',(nreverse
			     (reduce0
			      (lambda (acc arg)
				(cons arg (cons (make-keyword arg) acc)))
			      arg-names))))))))))
