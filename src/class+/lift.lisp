(in-package :core-server)

;; +-------------------------------------------------------------------------
;; | Class+ Lifting
;; +-------------------------------------------------------------------------
;; Author: Evrim Ulu <evrim@core.gen.tr>
;; Date: July 2011

;; -------------------------------------------------------------------------
;; Permute
;; -------------------------------------------------------------------------
;; 
;; SERVER> (permute '((a b) (c d) (e f)))
;; ((A C E) (A C F) (A D E) (A D F) (B C E) (B C F) (B D E) (B D F))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun permute (list)
    (flatten1
     (mapcar (lambda (atom)
	       (mapcar (curry #'cons atom)
		       (let ((a (permute (cdr list))))
			 (if (atom a)
			     (list a)
			     a))))
	     (car list))))

  ;; SERVER> (car (%find-method 'foo (list (find-class 'a) (find-class 'a))))
  ;; #<STANDARD-METHOD FOO (A A) {100521CF71}>
  (defun %find-method (method-name specializers)
    (aif (sb-pcl::find-generic-function method-name nil) 
	 (sb-pcl::compute-applicable-methods-using-classes it
							   specializers))))

;; +-------------------------------------------------------------------------
;; | Method Lifting
;; +-------------------------------------------------------------------------
(defmacro %defmethod/lift (method-name (&rest args)
			   &optional output-method-name)
  (let* ((walk-args (mapcar (lambda (arg)
			      (typecase arg
				(keyword-function-argument-form
				 (if (null (supplied-p-parameter arg))
				     (setf (supplied-p-parameter arg)
					   (gensym)))))
			      arg)
			    (walk-lambda-list args nil '()
					      :allow-specializers t)))
	 (specializers (mapcar (lambda (arg)
				 (typecase arg
				   (specialized-function-argument-form
				    (find-class (arnesi::specializer arg)))
				   (t
				    (find-class 't))))
			       walk-args))
	 (lifts (mapcar (lambda (class)
			  (aif (mapcar
				(compose #'class+.find
					 #'sb-pcl::slot-definition-type)
				(filter
				 (lambda (slot)
				   (eq (slot-definition-host slot)
				       'lift))
				 (class+.slots class)))
			       it
			       (list class)))
			specializers))
	 (method (car (any (lambda (specializers)
			     (%find-method method-name specializers))
			   (permute lifts)))))
    (when method
      (let* ((method-lambda-list (walk-lambda-list
				  (sb-pcl::method-lambda-list method)
				  nil nil :allow-specializers t))
	     (method-lambda-list (append method-lambda-list
					 (drop (length method-lambda-list)
					       walk-args)))
	     (method-specializers (sb-pcl::method-specializers method))
	     (method-specializers (append
				   method-specializers
				   (mapcar
				    (lambda (a)
				      (declare (ignore a))
				      (find-class 't))
				    (seq (- (length specializers)
					    (length method-specializers))))))
	     (arguments (mapcar #'list method-specializers specializers
				walk-args method-lambda-list)))
	(labels ((find-slot (class class-to-find)
		   (let ((slot (any
				(lambda (slot)
				  (with-slotdef (type) slot
				    (if (member class-to-find
					 (class+.superclasses
					  (find-class type)))
					slot)))
				(filter (lambda (slot)
					  (eq 'lift
					      (slot-definition-host slot)))
					(class+.slots class)))))
		     (slot-definition-name slot)))
		 (process-argument (atom)
		   (destructuring-bind (new old arg arg-old) atom
		     (let ((name (name arg))
			   (name-old (name arg-old)))
		       (typecase arg
			 (specialized-function-argument-form
			  (if (eq old new)
			      `(list ,name)
			      `(list (slot-value ,name
						 ',(find-slot old new)))))
			 (keyword-function-argument-form
			  `(if ,(arnesi::supplied-p-parameter arg)
			       (list ,(make-keyword name-old) ,name)
			       nil))
			 (t
			  `(list ,name)))))))
	  `(defmethod ,(or output-method-name method-name)
	       ,(unwalk-lambda-list walk-args)
	     ,(cond
	       ((atom method-name)
		`(apply ',method-name
			(append ,@(mapcar #'process-argument arguments))))
	       (t
		`(,(car method-name)
		   (,(cadr method-name)
		     ,@(flatten1
			(mapcar #'cdr (mapcar #'process-argument (cdr arguments)))))
		   ,(name (caddr (car arguments))))))))))))

(defmacro defmethod/lift (method-name (&rest args)
			  &optional output-method-name)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel)
       (%defmethod/lift ,method-name ,args ,output-method-name))
     (eval-when (:execute)
       (%defmethod/lift ,method-name ,args ,output-method-name))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun class+-find-slot-in-classes (slot supers)    
    (any (lambda (class)
	   (aif (class+.find-slot class slot)
		class))
	 supers))
  
  (defmethod class+-lifted-slots (name supers slots)
    (mapcar (lambda (class)
	      (let ((keyword (make-keyword (class-name class))))
		`(,(class-name class) :host lift
		   :initarg ,keyword
		   :initform (error ,(format nil "Provide :~A" keyword))
		   :type ,(class-name class))))
	    (uniq
	     (mapcar (lambda (slot)
		       (aif (class+-find-slot-in-classes
			     (car slot) (mapcar #'class+.find supers))
			    it
			    (error "Can't find slot ~A to lift in superclasses"
				   (car slot))))
		     (filter (lambda (slot) (getf (cdr slot) :lift))
			     slots))))))


(defmethod shared-initialize :after ((self class+-instance) slots
				     &key &allow-other-keys)
  (let ((supers (reverse
		 (remove (class-of self)
			 (class-superclasses (class-of self))))))
    (mapcar (lambda (slot)
	      (let* ((name (slot-definition-name slot))
		     (target (class+-find-slot-in-classes name supers)))
		(copy-slots (slot-value self (class-name target))
			    self (list name))))
	    (filter (lambda (slot)
		      (and (not (null (slot-definition-lift slot)))
			   (eq (slot-definition-host slot) 'remote)))
		    (class+.slots (class-of self)))))
  self)

(defmacro defclass+-slot-lifts (class-name)
  (flet ((find-lifted-slot (slot new-class)
	   (any (lambda (class)
		  (aif (class+.find-slot class slot)
		       it))
		(class+.direct-superclasses new-class))))
    (let* ((class (class+.find class-name)))
      `(progn
	 ,@(mapcar
	    (lambda (slot)
	      (with-slotdef (name reader writer) slot
		(let ((lifted-slot (find-lifted-slot name class)))
		  (let ((reader1 reader)
			(writer1 writer))
		    (with-slotdef (reader writer) lifted-slot
		      `(progn
			 (defmethod/lift ,reader1 ((self ,class-name))
			   ,reader)
			 (defmethod/lift ,writer1 (value (self ,class-name))
			   ,writer)))))))
	    (filter (lambda (slot) (slot-definition-lift slot))
		    (class+.slots class)))))))

;; -------------------------------------------------------------------------
;; Lift Usage
;; -------------------------------------------------------------------------
;; (defclass+ a ()
;;   ((slot1 :host local :initform "slot1-of-a")
;;    (slot2 :host remote :initform "slot2-of-a")))

;; (defclass+ c ()
;;   ((slot3 :host local :initform "slot3-of-c")))

;; (defclass+ b (a c)
;;   ((slot1 :lift t)
;;    (slot2 :host remote :lift t)
;;    (slot3 :host remote :lift t)))

;; -------------------------------------------------------------------------
;; Method Lifting
;; -------------------------------------------------------------------------
;; (defmethod lifted-method ((a1 a) (a2 a) x &key (y 'default-y y-supplied-p))
;;   (describe (list a1 a2 y y-supplied-p))
;;   (list 'lifted-slot2-first-a (a.slot2 a1)
;; 	'lifted-slot2-second-a (a.slot2 a2)
;; 	'x x 'y y))

;; (defmethod (setf lifted-setf-method) (value (a1 a) (a2 a))
;;   (list value a1 a2))

;; (defmethod lifted-method ((b1 b) (b2 b) x &key y)
;;   (call-next-method (slot-value b1 'a)
;; 		    (slot-value b2 'a)))

;; (defmethod/lift lifted-method ((first-b b) (second-b b) x1 &key (y1 nil supplied-p)))
;; ;; (PROGN 
;; ;;   (EVAL-WHEN (:EXECUTE)
;; ;;     (DEFMETHOD LIFTED-METHOD ((FIRST-B B) (SECOND-B B) X1 &KEY Y1)
;; ;;       (LIFTED-METHOD (SLOT-VALUE FIRST-B 'A) (SLOT-VALUE SECOND-B 'A)
;; ;; 		     X1 :Y Y1))))

;; (defmethod/lift (setf lifted-setf-method) (value (a1 b) (a2 b)))
;; ;; (PROGN 
;; ;;   (EVAL-WHEN (:EXECUTE)
;; ;;     (DEFMETHOD (SETF LIFTED-SETF-METHOD) (VALUE (A1 B) (A2 B))
;; ;;       (SETF (LIFTED-SETF-METHOD (SLOT-VALUE A1 'A) (SLOT-VALUE A2 'A))
;; ;; 	       VALUE))))

;; -------------------------------------------------------------------------
;; Slot Lifting
;; -------------------------------------------------------------------------
;; SERVER> (let* ((a (a))
;; 	       (b (b :a a :c (c))))
;; 	  (setf (b.slot1 b) "foo-bar")
;; 	  (describe b)
;; 	  (describe a))
;; #<B  {1002A09C91}>
;;   [standard-object]

;; Slots with :INSTANCE allocation:
;;   SLOT3  = "slot3-of-c"
;;   SLOT1  = NIL
;;   SLOT2  = "slot2-of-a"
;;   A      = #<A  {1002A09771}>
;;   C      = #<C  {1002A09A31}>
;; #<A  {1002A09771}>
;;   [standard-object]

;; Slots with :INSTANCE allocation:
;;   SLOT1  = "foo-bar"
;;   SLOT2  = "slot2-of-a"
;; ; No value
;; SERVER>
