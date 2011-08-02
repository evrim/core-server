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
  (let* ((walk-args (walk-lambda-list args nil '() :allow-specializers t))
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
	(labels ((find-lifted-slot (old-class new-class)
		   ;; new: page/anonymous, old: template, in between: page
		   (any (lambda (class)
			  (aif (member old-class (class+.superclasses class))
			       class))
			(class+.direct-superclasses new-class)))
		 (process-argument (acc atom)
		   (destructuring-bind (new old arg arg-old) atom
		     (let ((name (name arg))
			   (name-old (name arg-old)))
		       (typecase arg
			 (specialized-function-argument-form
			  (if (eq old new)
			      (cons name acc)
			      (cons
			       `(slot-value ,name
					    ',(class-name
					       (find-lifted-slot new old)))
			       acc)))
			 (keyword-function-argument-form
			  (cons name
				(cons (make-keyword name-old) acc)))
			 (t
			  (cons name acc)))))))
	  `(defmethod ,(or output-method-name method-name) ,args
	     ,(cond
	       ((atom method-name)
		`(,method-name
		  ,@(nreverse (reduce0 #'process-argument arguments))))
	       (t
		(describe (list (length method-specializers)
				(length specializers)
				(length walk-args)
				(length method-lambda-list)
				method-lambda-list
				walk-args))
		(describe arguments)
		`(,(car method-name)
		   (,(cadr method-name)
		     ,@(nreverse
			(reduce0 #'process-argument (cdr arguments))))
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
  (let ((supers (remove (class-of self)
			(class-superclasses (class-of self)))))
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
  (let* ((class (class+.find class-name))
	 (supers (remove class (class-superclasses class))))
    `(progn
       ,@(mapcar (lambda (slot)
		   (with-slotdef (name reader writer) slot
		     (let ((reader1 reader) (writer1 writer))
		       (let* ((lift-class (class+-find-slot-in-classes
					   name supers))
			      (lift-class-name (class-name lift-class))
			      (lift-slot (class+.find-slot lift-class name)))
			 (with-slotdef (reader writer) lift-slot
			   `(progn
			      (defmethod ,reader1 ((self ,class-name))
				(,reader
				 (slot-value self ',lift-class-name)))
			      (defmethod ,writer1 (value (self ,class-name))
				(setf
				 (,(cadr writer)
				   (slot-value self ',lift-class-name))
				 value))))))))
		 (filter (lambda (slot) (slot-definition-lift slot))
			 (class+.slots class))))))

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
;; (defmethod lifted-method ((a1 a) (a2 a) x &key y)
;;   (list 'lifted-slot2-first-a (a.slot2 a1)
;; 	'lifted-slot2-second-a (a.slot2 a2)
;; 	'x x 'y y))

;; (defmethod (setf lifted-setf-method) (value (a1 a) (a2 a))
;;   (list value a1 a2))

;; (defmethod/lift lifted-method ((first-b b) (second-b b) x1 &key y1))
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
