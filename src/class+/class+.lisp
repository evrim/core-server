(in-package :core-server)

;; +-------------------------------------------------------------------------
;; | Class+ Implementation
;; +-------------------------------------------------------------------------
(defclass class+ (standard-class)
  ((rest :initarg :rest :initform nil :accessor class+.rest)
   (ctor :initarg :ctor :initform nil :accessor class+.%ctor)
   (methods :initarg :methods :initform nil :accessor class+.%methods)
   (%timestamp :accessor class+.timestamp))
  (:documentation "Metaclass of Class+ classes"))

(defprint-object (self class+ :identity t :type t)
  (format t "~A" (class-name self)))

(defmethod shared-initialize :after ((class class+) slot-names
				     &key &allow-other-keys)
  (prog1 class (setf (class+.timestamp class) (get-universal-time))))

;; --------------------------------------------------------------------------
;; Class+ Protocol Implementation
;; --------------------------------------------------------------------------
(defmethod class+.name ((self class+))
  (class-name self))

(defmethod class+.direct-superclasses ((class class+))
  (filter (lambda (c) (typep c 'class+)) (class-direct-superclasses class)))

(defmethod class+.superclasses ((class class+))
  (filter (lambda (c) (typep c 'class+)) (class-superclasses class)))

(defmethod class+.direct-subclasses ((class class+))
  (filter (lambda (a) (typep a 'class+)) (class-direct-subclasses class)))

(defmethod class+.subclasses ((class class+))
  (filter (lambda (a) (typep a 'class+)) (class-subclasses class)))

(defmethod (setf class+.timestamp) (timestamp (class class+))  
  (mapcar (lambda (class)
	    (setf (slot-value class '%timestamp) timestamp))
	  (cons class (class+.subclasses class))))

(defvar +standard-class-slots+ (class-slots (find-class 'standard-class)))

(defmethod class+.slots ((class class+))
  (when (not (class-finalized-p class))
    (finalize-inheritance class))

  (uniq (reverse
	 (filter
	  (lambda (e)
	    (not (member (slot-definition-name e) +standard-class-slots+
			 :key #'slot-definition-name)))
	  (class-slots class)))
	:key #'slot-definition-name :test #'string=))

;; (defmethod class+.self-slots ((class class+))
;;   (set-difference 
;;    (class+.slots class)
;;    (reduce #'append 
;;   	   (mapcar (lambda (a) (class+.slots a))
;;   		   (remove class (class+.superclasses class))))
;;    :key #'core-server::slot-definition-name))

(defmethod class+.direct-slots ((class class+))
  (sb-mop:class-direct-slots class))

(defmethod class+.add-method ((class class+) name type args)
  (setf (class+.%methods class)
	(cons (cons name (cons type args)) (class+.remove-method class name))))

(defmethod class+.remove-method ((class class+) name)
  (setf (class+.timestamp class) (get-universal-time)
	(class+.%methods class) (filter (lambda (a) (not (eq (car a) name)))
					(class+.%methods class))))

(defmethod class+.methods ((class class+))
  (let ((methods))
    (mapcar (lambda (m) (pushnew m methods :key #'car))
	    (reduce0 #'append
		     (mapcar #'class+.%methods
			     (cons class (class+.superclasses class)))))
    methods))

;; --------------------------------------------------------------------------
;; Extra Definitions
;; --------------------------------------------------------------------------
(defmethod class+.slot-search ((class class+) match-p)
  (filter match-p (reverse (class+.slots class))))

(defmethod class+.find-slot ((class t) slot-name) nil)
(defmethod class+.find-slot ((class class+) slot-name)
  (find slot-name (class+.slots class) :key #'slot-definition-name :test #'string=))

(defmethod class+.local-slots ((class class+))
  (class+.slot-search class (lambda (s)
			      (or (eq 'local (slot-value s 'host))
				  (eq 'both (slot-value s 'host))))))

(defmethod class+.remote-slots ((self class+))
  (class+.slot-search self (lambda (s)
			     (or (eq 'remote (slot-value s 'host))
				 (eq 'both (slot-value s 'host))))))

(defmethod class+.local-methods ((class class+))
  (filter (lambda (m) (eq 'local (cadr m)))
	  (class+.methods class)))

(defmethod class+.find-local-method ((class class+) (name symbol))
  (find name (class+.local-methods class) :key #'car))

(defmethod class+.find-local-method ((class class+) (name string))
  (find name (class+.local-methods class) :key #'car :test #'string=))

(defmethod class+.remote-methods ((class class+))
  (filter (lambda (m) (eq 'remote (cadr m)))
	  (class+.methods class)))

(defmethod class+.find-remote-method ((class class+) (name symbol))
  (find name (class+.remote-methods class) :key #'car))

(defmethod class+.find-remote-method ((class class+) (name string))
  (find name (class+.remote-methods class) :key #'car :test #'string=))

(defun class+.slot-boundp (object slot)
  (and (slot-exists-p object slot) (slot-boundp object slot)))

;; -----------------------------------------------------------------------------
;; MOP Implementation
;; -----------------------------------------------------------------------------
(defmethod validate-superclass ((class class+) (super standard-class)) t)
(defmethod validate-superclass ((class standard-class) (super class+)) nil)

(defclass class+-slot-definition (standard-slot-definition)
  ((host :initarg :host :initform 'none :accessor slot-definition-host)
   (remote-type :initarg :remote-type :initform 'primitive
		:accessor slot-definition-remote-type)
   (relation :initarg :relation :initform nil :accessor slot-definition-relation)
   (index :initarg :index :initform nil :accessor slot-definition-index)
   (print :initarg :print :initform nil :accessor slot-definition-print)
   (label :initarg :label :initform nil :accessor slot-definition-label)
   (lift  :initarg :lift :initform nil :accessor slot-definition-lift)
   (authorize :initarg :authorize :initform nil :accessor slot-definition-authorize)
   (export  :initarg :export :initform t :accessor slot-definition-export)
   (leaf :initarg :leaf :initform nil :accessor slot-definition-leaf)))

(defclass class+-direct-slot-definition (class+-slot-definition
					 standard-direct-slot-definition)
  ())

(defclass class+-effective-slot-definition (class+-slot-definition
					    standard-effective-slot-definition)
  ())

(defmethod direct-slot-definition-class ((class class+) &rest initargs)
  (declare (ignore initargs))
  (find-class 'class+-direct-slot-definition))

(defmethod %class+-inherited-slots ((class class+))
  '(host remote-type ;; sb-pcl::readers sb-pcl::writers
	 relation index print label lift export leaf authorize))

(defmethod compute-effective-slot-definition-initargs :around ((class class+) direct-slotds)
  (let ((slotd (car direct-slotds)))
    (typecase slotd
      (class+-slot-definition
       (append (call-next-method)
	       (reduce (lambda (acc slot)
			 (cons (make-keyword slot)
			       (cons (slot-value slotd slot) acc)))
		       (%class+-inherited-slots class) :initial-value nil)))
      (t (call-next-method)))))

;; ;; -------------------------------------------------------------------------
;; ;; Lifted Slots
;; ;; -------------------------------------------------------------------------
;; (defclass class+-effective-lifted-slot-definition (class+-lifted-slot-mixin
;; 						   class+-effective-slot-definition)
;;   ())

;; ;; -------------------------------------------------------------------------
;; ;; Authorized Slots
;; ;; -------------------------------------------------------------------------
;; (defclass class+-effective-authorized-and-lifted-slot-definition
;;     (class+-lifted-slot-mixin class+-authorized-slot-mixin
;; 			      class+-effective-slot-definition)
;;   ())

(defmethod effective-slot-definition-class ((class class+) &rest initargs)
  ;; (cond
  ;;   ((and (getf initargs :authorize) (getf initargs :lift))
  ;;    (find-class 'class+-effective-authorized-and-lifted-slot-definition))
  ;;   ((getf initargs :lift) (find-class 'class+-effective-slot-definition))
  ;;   (t (find-class 'class+-effective-slot-definition)))
  (find-class 'class+-effective-slot-definition))

;; --------------------------------------------------------------------------
;; Slot Definition Sugars
;; --------------------------------------------------------------------------
(defmethod slot-definition-supplied-p ((slot class+-slot-definition))
  (intern (format nil "~A-SUPPLIED-P" (slot-definition-name slot))))

(defmethod slot-definition-to-plist ((slot class+-slot-definition))
  (let ((initarg (car (reverse (slot-definition-initargs slot)))))
    (flet ((find-direct-slot ()
	     (let* ((class (slot-value slot 'sb-pcl::%class))
		    (slots (flatten1 (mapcar #'(lambda (a)
						 (if (typep a 'standard-class)
						     (slot-value a 'sb-pcl::direct-slots)))
					     (class-superclasses class)))))
	       (find (slot-definition-name slot) slots :key #'slot-definition-name))))
      (let ((direct-slot (find-direct-slot)))
	(list :name (slot-definition-name slot)
	      :initarg initarg
	      :initform ;; (slot-definition-initform slot)
	      (let ((initform (assoc initarg
				     (class-default-initargs
				      (class-name
				       (sb-pcl::slot-definition-class slot))))))
		(if initform
		    (cadr initform)		      
		    (slot-definition-initform slot)))
	      :supplied-p (slot-definition-supplied-p slot)
	      :host (slot-definition-host slot)
	      :remote-type (slot-definition-remote-type slot)
	      :type (sb-pcl::slot-definition-type slot)
	      :relation (slot-definition-relation slot)
	      :writer (if direct-slot
			  (car (reverse (sb-mop::slot-definition-writers direct-slot))))
	      :reader (if direct-slot
			  (car (reverse (sb-mop::slot-definition-readers direct-slot))))
	      :index (slot-definition-index slot)
	      :label (slot-definition-label slot)
	      :lift (slot-definition-lift slot)
	      :export (slot-definition-export slot)
	      :leaf (slot-definition-leaf slot)
	      :authorize (slot-definition-authorize slot))))))

(defmacro with-slotdef (arglist slot &body body)
  `(destructuring-bind (&key ,@arglist &allow-other-keys)
       (slot-definition-to-plist ,slot)
     ,@body))

(defmethod slot-definition-singular-type ((slot class+-slot-definition))
  (with-slotdef (type) slot
    (intern (subseq (symbol-name type) 0 (1- (length (symbol-name type))))
	    (symbol-package (slot-definition-name slot)))))

(defmethod slot-definition-singular-typep ((slot class+-slot-definition))
  (let ((type (sb-pcl::slot-definition-type slot)))
    (and (symbolp type)
	 (not (null (slot-definition-relation slot)))
	 (not (ends-with (symbol-name type) "*"))
	 (not (null (find-class+ type))))))

(defmethod slot-definition-plural-typep ((slot class+-slot-definition))
  (let* ((type (sb-pcl::slot-definition-type slot))
	 (name (symbol-name type)))
    (and (symbolp type)
	 (not (null (slot-definition-relation slot)))
	 (ends-with name "*")
	 (not (null (find-class+ (intern (subseq name 0 (1- (length name)))
					 (symbol-package
					  (slot-definition-name slot)))))))))

(defmethod slot-definition-relational-slot ((slot class+-slot-definition))
  (let* ((type (if (slot-definition-singular-typep slot)
		   (sb-pcl::slot-definition-type slot)
		   (slot-definition-singular-type slot))))
    (class+.find-slot (find-class+ type) (slot-definition-relation slot))))

;; --------------------------------------------------------------------------
;; Relations
;; --------------------------------------------------------------------------
(defmethod class+.relations ((class class+))
  (filter (lambda (slot)
	    (not (null (slot-definition-relation slot))))
	  (class+.slots class)))

(defmethod class+.1-to-n-relations ((class class+))  
  (filter (lambda (slot)
	    (and (slot-definition-plural-typep slot)
		 (slot-definition-singular-typep
		  (slot-definition-relational-slot slot))))
	  (class+.relations class)))

(defmethod class+.n-to-1-relations ((class class+))
  (filter (lambda (slot)
	    (and (slot-definition-singular-typep slot)
		 (slot-definition-plural-typep
		  (slot-definition-relational-slot slot))))
	  (class+.relations class)))

(defmethod class+.n-to-n-relations ((class class+))
  (filter (lambda (slot)
	    (and (slot-definition-plural-typep slot)
		 (slot-definition-plural-typep
		  (slot-definition-relational-slot slot))))
	  (class+.relations class)))

(defmethod slot-definition-relation-type ((slot class+-slot-definition))
  (let ((relational-slot (slot-definition-relational-slot slot)))
    (values
     (cond
       ((null relational-slot)
	(error "Cannot find relational-slot of slot ~A" slot))
       ((and (slot-definition-plural-typep slot)
	     (slot-definition-singular-typep relational-slot))
	;; 1->n Relation
	:one-to-n)
       ((and (slot-definition-singular-typep slot)
	     (slot-definition-plural-typep relational-slot))
	;; n->1 Relation
	:n-to-one)			
       ((and (slot-definition-plural-typep slot)
	     (slot-definition-plural-typep relational-slot))
	;; n->n Relation
	:n-to-n)
       ((and (slot-definition-singular-typep slot)
	     (slot-definition-singular-typep relational-slot))
	:one-to-one)
       (t (error "Unknown type of relation slot: ~A" slot)))
     relational-slot)))

;; -------------------------------------------------------------------------
;; Operations
;; -------------------------------------------------------------------------
(defmethod class+.list-function ((self class+) &optional prefix)
  (intern (format nil "~A.LIST" (or prefix (class-name self)))))

(defmethod class+.find-function ((self class+) &optional prefix)
  (intern (format nil "~A.FIND" (or prefix (class-name self)))))

(defmethod class+.query-function ((self class+) &optional prefix)
  (intern (format nil "~A.QUERY" (or prefix (class-name self)))))

(defmethod class+.add-function ((self class+) &optional prefix)
  (intern (format nil "~A.ADD" (or prefix (class-name self)))))

(defmethod class+.delete-function ((self class+) &optional prefix)
  (intern (format nil "~A.DELETE" (or prefix (class-name self)))))

(defmethod class+.update-function ((self class+) &optional prefix)
  (intern (format nil "~A.UPDATE" (or prefix (class-name self)))))

(defmethod class+.crud-functions ((self class+) &optional prefix)
  (list (class+.list-function self prefix)
	(class+.find-function self prefix)
	(class+.query-function self prefix)
	(class+.add-function self prefix)
	(class+.update-function self prefix)
	(class+.delete-function self prefix)))

;; -------------------------------------------------------------------------
;; Traces
;; -------------------------------------------------------------------------
(deftrace class+-relations
    '(slot-definition-singular-type slot-definition-singular-typep
      slot-definition-typep slot-definition-plural-typep
      slot-definition-relational-slot slot-definition-relation-type
      class+.n-to-n-relations class+.n-to-1-relations class+.relations
      class+.1-to-n-relations))

;; --------------------------------------------------------------------------
;; Indexes
;; --------------------------------------------------------------------------
(defmethod class+.indexes ((class class+))
  (filter (lambda (slot)
	    (with-slotdef (index) slot
	      index))
	  (class+.slots class)))

;; --------------------------------------------------------------------------
;; Constructor Generation Methods
;; --------------------------------------------------------------------------
(defmethod class+.ctor-lambda-list ((self class+) &optional
				    (include-supplied-p nil))
  (reduce0 (lambda (acc slot)
	     (with-slotdef (initarg initform supplied-p) slot
	       (if initarg
		   (let ((symbol (intern (symbol-name initarg))))
		     (if include-supplied-p
			 (cons (list symbol initform supplied-p) acc)
			 (cons (list symbol initform) acc)))
		   acc)))
	   (filter (lambda (slot)
		     (not (string= 'none (slot-definition-host slot))))
	    (class+.slots self))))

(defmethod class+.local-ctor-lambda-list ((self class+) &optional
					  (include-supplied-p nil))
  (let ((local-slots (mapcar #'slot-definition-name
			     (class+.local-slots self))))
    (filter (lambda (slot)
	      (member (car slot) local-slots :test #'string=))
	    (class+.ctor-lambda-list self include-supplied-p))))

(defmethod class+.ctor-query-lambda-list ((self class+) &optional
					  (include-supplied-p nil))
  (mapcar (lambda (arg)
	    (if include-supplied-p
		`(,(car arg) nil ,(caddr arg))
		`(,(car arg) nil)))
	  (class+.ctor-lambda-list self include-supplied-p)))

(defmethod class+.ctor-arguments ((self class+) &optional lambda-list)      
  (reduce0 (lambda (acc slot)
	     (with-slotdef (initarg) slot
	       (if initarg
		   (cons initarg (cons (intern (symbol-name initarg)) acc))
		   acc)))
	   (aif (extract-argument-names lambda-list)
		(filter (lambda (slot)
			  (with-slotdef (name) slot
			    (member name it)))
			(class+.slots self))
		(filter (lambda (slot)
			  (not (string= 'none (slot-definition-host slot))))
			(class+.slots self)))))

(defmethod class+.ctor-keyword-arguments ((self class+) &optional lambda-list)
  (let ((arguments (or lambda-list (class+.ctor-lambda-list self t))))
    `(nreverse
      (reduce0 (lambda (acc atom)
		 (destructuring-bind (keyword value supplied-p) atom
		   (if supplied-p
		       (cons value (cons keyword acc))
		       acc)))
	       (list ,@(mapcar (lambda (arg)
				 `(list ,(make-keyword (car arg))
					,(car arg) ,@(cddr arg)))
			       arguments))))))

(defmethod class+.ctor-name ((self class+))
  (with-slots (ctor) self
    (or (and (atom ctor) ctor)
	(and (atom (car ctor)) (car ctor))
	(and (atom (caar ctor)) (caar ctor))
	(class+.name self)
	;; (intern (format nil "MAKE-~A" (class+.name self))
	;; 	      (symbol-package (class+.name self)))
	)))

(defmethod class+.%ctor ((self class+))
  (with-slots (ctor) self
    (if (atom (caar ctor))
	(cdar ctor)
	(cdr ctor))))

(defmethod class+.ctor ((self class+))
  (let ((name (class+.name self))
	(ctor-name (class+.ctor-name self)))
    (aif (class+.%ctor self)
	 `(progn
	    (fmakunbound ',ctor-name)
	    (defun ,ctor-name ,it
	      (make-instance ',name ,@(class+.ctor-arguments self it))))
	 (let ((keywords (class+.ctor-lambda-list self t)))
	   `(progn
	      (fmakunbound ',ctor-name)
	      (defun ,ctor-name (&key ,@keywords)
		(declare (ignorable ,@(mapcar #'caddr keywords)))
		(make-instance ',name ,@(class+.ctor-arguments self))))))))

(defmacro defclass+-ctor (name)
  (class+.ctor (find-class+ name)))

(defmacro defclass+-print-object (name)
  (let* ((class+ (find-class+ name))
	 (slots (filter #'slot-definition-print (class+.slots class+))))
    `(defprint-object (self ,name)
       (format t "~{~A~^ ~}"
	       (list
		,@(nreverse
		   (reduce0
		    (lambda (acc slot)
		      (with-slotdef (name) slot
			(cons `(list ',name (slot-value self ',name)) acc)))
		    slots)))))))

;; --------------------------------------------------------------------------
;; defclass+ Macro
;; --------------------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %fix-slot-definition (class-name slot-definition)
    (if (and (eq 'lift (getf (cdr slot-definition) :host))
	     (null (getf (cdr slot-definition) :type)))
	(error "Please provide :type to slot ~A" (car slot-definition)))
    
    (if (not (member :initform slot-definition))
	(setf (getf (cdr slot-definition) :initform)
	      (if (eq 'lift (getf (cdr slot-definition) :host))
		  `(error ,(format nil "Provide :~A"
				   (or (getf (cdr slot-definition) :initarg)
				       (car slot-definition))))
		  nil)))

    (if (not (member :initarg slot-definition))
	(setf (getf (cdr slot-definition) :initarg)
	      (make-keyword (car slot-definition))))

    (if (not (or (member :accessor slot-definition)
		 (member :reader slot-definition)
		 (member :writer slot-definition)))
	(setf (getf (cdr slot-definition) :accessor)
	      (intern (format nil "~A.~A" class-name
			      (car slot-definition)))))

    slot-definition)
  
  (defun %filter-rest (rest)
    (reverse (append (aif (cdr (assoc :ctor rest)) (list `(:ctor ,it)))
		     (list `(:metaclass ,(or (cadr (assoc :metaclass rest))
					     'class+)))
		     (list `(:rest ',rest))
		     (filter (lambda (a)
			       (and (not (eq :ctor (car a)))
				    (not (eq :metaclass (car a)))
				    (not (eq :rest (car a)))))
			     rest)))))

(defclass class+-instance ()
  ()
  (:metaclass class+))

(defclass object-with-id ()
  ((database-id :host both :index t :reader get-database-id :initform -1
		:initarg :database-id :print t :export nil))
  (:metaclass class+))

(defmacro defclass+ (name supers slots &rest rest)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (deftype ,(intern (format nil "~A*" name) (symbol-package name)) ()
	 '(or null cons))
       (defclass ,name (,@(remove 'class+-object supers) class+-instance)
	 (,@(mapcar (lambda (slot) (%fix-slot-definition name slot)) slots)
	  ,@(class+-lifted-slots name supers slots))
	 ,@(%filter-rest rest))
       (defclass+-ctor ,name))
     (defclass+-print-object ,name)
     (defclass+-slot-lifts ,name)
     (find-class ',name)))

;; (defclass+ user1 ()
;;   ((username :host local)
;;    (password :host remote)
;;    (email)
;;    (address :host none))
;;   (:documentation "eben"))

;; Core Server: Web Application Server
;;
;; Copyright (C) 2006-2008  Metin Evrim Ulu, Aycan iRiCAN
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>


