(in-package :core-server)

;; +-------------------------------------------------------------------------
;; | Class+ Lifting
;; +-------------------------------------------------------------------------
;; Author: Evrim Ulu <evrim@core.gen.tr>
;; Date: July 2011

;; +-------------------------------------------------------------------------
;; | Method Lifting
;; +-------------------------------------------------------------------------
(defmacro %defmethod/lift (method-name (&rest args))  
  (let* ((class-name (if (atom method-name) (cadar args) (cadadr args)))
	 (class (class+.find class-name))
	 (arg-classes (mapcar (lambda (arg)
				(or (and (atom arg) (find-class 't))
				    (find-class (cadr arg))))
			      args)))
    (flet ((%find-method (class)
	     (sb-pcl::compute-applicable-methods-using-classes
	      (sb-pcl::find-generic-function method-name)
	      (if (atom method-name)
		  (cons class (cdr arg-classes))
		  (cons (car arg-classes)
			(cons class (cddr arg-classes)))))))
      (let* ((method (car
		      (reverse
		       (any #'%find-method
			    (remove class (class-superclasses class))))))
	     (specializers (sb-pcl::method-specializers method))
	     (lambda-list (sb-pcl::method-lambda-list method)))
	(cond
	  ((atom method-name)
	   `(defmethod ,method-name ,(mapcar
				      (lambda (l s)
					(list l (class-name s)))
				      (cons 'self (cdr lambda-list))
				      (cons class (cdr specializers)))
	      (,method-name
	       (slot-value self ',(class-name (car specializers)))
	       ,@(cdr lambda-list))))
	  (t
	   `(defmethod ,method-name ,(mapcar
				      (lambda (l s)
					(list l (class-name s)))
				      (cons (car lambda-list)
					    (cons 'self
						  (cddr lambda-list)))
				      (cons class specializers))
	      (,(car method-name)
		(,(cadr method-name)
		  (slot-value self
			      ',(class-name (cadr specializers))))
		,(car lambda-list)))))))))

(defmacro defmethod/lift (method-name (&rest args))
  `(eval-when (:load-toplevel :execute)
     (eval `(%defmethod/lift ,',method-name ,',args))))


;; +-------------------------------------------------------------------------
;; | Class+ Lift Macro
;; +-------------------------------------------------------------------------
(defmacro deflift+ (class-name supers slots &rest args)
  (let* ((lifted-slots (remove-if #'null
			(mapcar (lambda (slot)
				  (any
				   (lambda (class)
				     (aif (class+.find-slot class (car slot))
					  (cons class
						(cons it
						      (%fix-slot-definition
						       class-name slot)))))
				   (mapcar #'class+.find supers)))
				(filter (lambda (slot)
					  (eq (getf (cdr slot) :host)
					      'lifted))
					slots))))
	 (local-lifted-slots
	  (filter (lambda (slot)
		    (eq (slot-definition-host (cadr slot)) 'local))
		  lifted-slots))
	 (remote-lifted-slots
	  (filter (lambda (slot)
		    (eq (slot-definition-host (cadr slot)) 'remote))
		  lifted-slots))
	 (lifted-supers (uniq
			 (sort
			  (mapcar #'car lifted-slots) #'>
			  :key (lambda (a) (slot-value a '%timestamp))))))
    `(progn
       (defclass+ ,class-name ,supers
	 (,@(filter (lambda (slot)
		      (not (eq (getf (cdr slot) :host) 'lifted)))
		    slots)
	    ,@(mapcar (lambda (class)
			`(,(class-name class) :host none
			   :initarg ,(make-keyword (class-name class))))
		      lifted-supers)
	    ,@(mapcar (lambda (slot)
			(let ((slotdef (cddr slot)))
			  (if (getf (cdr slotdef) :host)
			      (prog1 slotdef
			  	(setf (getf (cdr slotdef) :host) 'remote))
			      (list* (car slotdef)
			  	     :host 'remote
			  	     (cdr slotdef)))))
		      remote-lifted-slots))
	 ,@args)
       ,@(mapcar
	  (lambda (slot)
	    (destructuring-bind (class slotdef &rest newslot) slot
	      (with-slotdef (reader writer) slotdef
		(let* ((reader1 (or (getf (cdr newslot) :accessor)
				    (getf (cdr newslot) :reader)))
		       (writer1 (or (getf (cdr newslot) :accessor)
				    (getf (cdr newslot) :writer))))
		  `(progn
		     (defmethod ,reader1 ((self ,class-name))
		       (,reader (slot-value self ',(class-name class))))
		     (defmethod (setf ,writer1) (value (self ,class-name))
		       (setf (,(cadr writer)
			       (slot-value self ',(class-name class)))
			     value)))))))
	  local-lifted-slots)
       ,(when (not (null remote-lifted-slots))
	  ;; (warn "deflift+ overrides shared-initialize :after of class ~A"
	  ;; 	class-name)
	  `(defmethod shared-initialize :after ((self ,class-name) slots
						&rest initargs)
	     (declare (ignore initargs))
	     ,@(mapcar (lambda (slot)
			 (let ((class (car (find (cadr slot) lifted-slots
						 :key #'cadr))))
			   `(copy-slots
			     (slot-value self ',(class-name class))
			     self '(,(slot-definition-name (cadr slot))))))
		       remote-lifted-slots)))
       (class+.find ',class-name))))

;; (defclass+ a ()
;;   ((slot1 :host local)
;;    (slot2 :host remote)))

;; (defmethod hede ((self a))
;;   (list 'lifted-slot1 (a.slot1 self) 1 2 3))

;; (defmethod foo ((self a) c d e)
;;   (list 'lifted-slot1 (a.slot1 self) c d e))

;; (deflift+ b (a)
;;   ((slot1 :host lifted)
;;    (slot2 :host lifted)))


;; (defmethod/lift hede ((self b)))
;; (defmethod/lift foo ((self b) c d e))

;; (defclass+ c ()
;;   ())

;; (defmethod hede ((self c))
;;   (list 'ccc))

;; SERVER> (let* ((a (a :slot1 "slot1-of-a" :slot2 "slot2-of-a"))
;; 	       (b (b :a a)))
;; 	  (setf (b.slot1 b) "slot1-of-b")
;; 	  (describe a)
;; 	  (describe b)
;; 	  (hede b))

;; #<A  {1004CB7511}>
;;   [standard-object]

;; Slots with :INSTANCE allocation:
;;   SLOT1  = "slot1-of-b"
;;   SLOT2  = "slot2-of-a"
;; #<B  {1004CB7551}>
;;   [standard-object]

;; Slots with :INSTANCE allocation:
;;   SLOT1  = NIL
;;   SLOT2  = "slot2-of-a"
;;   A      = #<A  {1004CB7511}>
;; (LIFTED-SLOT1 "slot1-of-b" 1 2 3)


;; SERVER> (let* ((a (a :slot1 "slot1-of-a" :slot2 "slot2-of-a"))
;; 	       (b (b :a a)))
;; 	  (setf (b.slot1 b) "slot1-of-b")
;; 	  (describe a)
;; 	  (describe b)
;; 	  (foo b 'c 'd 'e))

;; #<A  {1003B56381}>
;;   [standard-object]

;; Slots with :INSTANCE allocation:
;;   SLOT1  = "slot1-of-b"
;;   SLOT2  = "slot2-of-a"
;; #<B  {1003B563C1}>
;;   [standard-object]

;; Slots with :INSTANCE allocation:
;;   SLOT1  = NIL
;;   SLOT2  = "slot2-of-a"
;;   A      = #<A  {1003B56381}>
;; (LIFTED-SLOT1 "slot1-of-b" C D E)
