(in-package :core-server)

;; Component Procotol
(defclass component ()
  ((local-args :accessor command.local-args :initarg :local-args :initform '())
   (remote-args :accessor command.remote-args :initarg :remote-args :initform '())))

(defgeneric/cc run (component)
  (:documentation "Run this component with an instance of the component class."))

(eval-when (:execute)
  (defvar +component-registry+ (make-hash-table :test #'equal)))

(defun local-methods-of-class (name)
  (getf (gethash name +component-registry+) :local-methods))

(defun remote-methods-of-class (name)
  (getf (gethash name +component-registry+) :remote-methods))

(defun add-local-method-for-class (name method-name)
  (setf (getf (gethash name +component-registry+) :local-methods)
	(cons method-name
	      (remove method-name
		      (getf (gethash name +component-registry+) :local-methods)))))

(defun add-remote-method-for-class (name method-name)
  (setf (getf (gethash name +component-registry+) :remote-methods)
	(cons method-name
	      (remove method-name
		      (getf (gethash name +component-registry+) :remote-methods)))))

;; (defun remote-class-definition-of-class (name)
;;   `(setf ,name
;; 	 (create
;; 	  :eben 1
;; 	  ,@(reduce (lambda (acc method)
;; 		      (cons (make-keyword method)
;; 			    (cons `(lambda (gee) gee)
;; 				  acc)))
;; 		    (local-methods-of-class name) :initial-value nil)
;; 	  ,@(reduce (lambda (acc method)
;; 		      (cons (make-keyword method)
;; 			    (cons `(,method self)
;; 				  acc)))
;; 		    (remote-methods-of-class name) :initial-value nil))))

(defmacro define-run-method-for-class (class-name)
  (let* ((local-methods (reduce (lambda (acc method)
				  (cons (make-keyword method)
					(cons `(lambda (gee) gee)
					      acc)))
				(local-methods-of-class class-name) :initial-value nil))
	 (remote-methods (reduce (lambda (acc method)
				   (cons (make-keyword method)
					 (cons `(with-call/cc (,method self))
					       acc)))
				 (remote-methods-of-class class-name) :initial-value nil))
	 (body `(setf ,class-name (create :eben 2 ,@local-methods ,@remote-methods))))
    (describe local-methods)
    (describe remote-methods)
    (describe body)
    `(defmethod/cc shit ((self ,class-name))       
       (<:js
	`(setf ,,class-name (create :eben 2 ,,@local-methods ,,@remote-methods))))))

;; (defmethod van1 ((self component))
;;   (let ((class-name (class-name (class-of self))))
;;     (<:js
;;      `(setf ,class-name
;; 	    (create
;; 	     ,@(reduce (lambda (acc method)
;; 			 (cons (make-keyword method)
;; 			       (cons `(lambda (gee) gee)
;; 				     acc)))
;; 		       (local-methods-of-class class-name) :initial-value nil)
;; 	     ,@(reduce (lambda (acc method)
;; 			 (cons (make-keyword method)
;; 			       (cons (let ((method (function method)))
;; 				       (with-call/cc (funcall method self)))
;; 				     acc)))
;; 		       (remote-methods-of-class class-name) :initial-value nil))))))

(defmacro defmethod/local (name ((self class-name) &rest args) &body body)  
  (add-local-method-for-class class-name name)
  `(prog1 (defmethod/cc ,name ((,self ,class-name) ,@args) ,@body)
;;     (define-run-method-for-class ,class-name)
     ))

(defmacro defmethod/remote (name ((self class-name) &rest args) &body body)
  (let ((arg-names (arnesi:extract-argument-names args :allow-specializers t)))
    (add-remote-method-for-class class-name name)    
    `(prog1 (defmethod/cc ,name ((,self ,class-name))	      
	      `(lambda ,',arg-names
		 ,',@body))
;;       (define-run-method-for-class ,class-name)
       )))


;; Standard Component
(defclass standard-component (component)
  ())

(defmethod/cc run ((self standard-component))
  )


(defmacro defcomponent (name supers slots &rest default-initargs)
  (labels ((clazz-name (name)
	     (intern (string-upcase (format nil "~A" name))))
	   (gen-class (name &optional direction)
	     (case direction
	       ((to view send)
		(clazz-name (format nil "~A-~A" name 'send)))
	       ((from form receive)
		(clazz-name (format nil "~A-~A" name 'receive)))
	       (t
		(clazz-name (format nil "~A" name)))))
	   (filter-slot (slot-def)
	     (when (or (eq 'local (getf (cdr slot-def) :host))
		       (eq 'both (getf (cdr slot-def) :host)))	       
	       (unless (getf (cdr slot-def) :initarg)
		 (setf (getf (cdr slot-def) :initarg)
		       (make-keyword (car slot-def)))))
	     (unless (getf (cdr slot-def) :accessor)
	       (setf (getf (cdr slot-def) :accessor)
		     (car slot-def)))
	     (remf (cdr slot-def) :host)
	     slot-def)
	   (local-slot (acc slot-def)
	     (if (or (eq (getf (cdr slot-def) :host) 'local)
		     (eq (getf (cdr slot-def) :host) 'both))
		 (cons (list (car slot-def) (getf (cdr slot-def) :initform)) acc)
		 acc))
	   (local-args (slotz)
	     (let ((args (append
			  (nreverse (reduce #'local-slot slotz :initial-value nil))
			  (reduce #'(lambda (acc super)
				      (append acc (getf (gethash super +component-registry+)
							:local-args)))
				  supers :initial-value nil)))
		   (super-args
		    (reduce #'append (mapcar #'class-default-initargs supers))))
 	       (setf args		     
		     (reduce
		      #'(lambda (acc arg)
			  (let ((value (cadr (assoc (car arg) super-args
						    :test #'string=))))
			    (if value
				(cons (list (car arg) value) acc)
				(cons arg acc))))
			     args :initial-value nil))
	       (reduce #'(lambda (acc arg)
			   (let ((value (getf (cdar default-initargs)
					      (make-keyword (car arg)))))
			     (if value
				 (cons (list (car arg) value) acc)
				 (cons arg acc))))
		       args :initial-value nil)))
	   (function-key-args (slotz)
	     (reduce #'(lambda (acc slot-def)			 
			 (cons (make-keyword (car slot-def))
			       (cons (car slot-def) acc)))
		     (local-args slotz) :initial-value nil))
	   (filter-default-initargs (lst)
	     (nreverse (reduce #'(lambda (acc item)
				   (if (or (eq item :default-initargs)
					   (eq item :local-args)
					   (eq item :remote-args))
				       acc
				       (cons item acc)))
			       lst :initial-value nil))))
    (setf (getf (gethash name +component-registry+) :local-args) (local-args slots))
    `(progn       
       (defclass ,(gen-class name t) (,@supers component)
	 ,(mapcar #'filter-slot (copy-tree slots))
	 (:default-initargs ,@(filter-default-initargs (car default-initargs)))
	 ,@(cdr default-initargs))
       (defun/cc ,(intern (string-upcase name)) (&key ,@(local-args slots))
	 (run (apply #'make-instance ',(gen-class name t)
		     (list ,@(function-key-args slots))))))))

(defcomponent window ()
  ((title :host local)
   (matil :host remote)))