(in-package :core-server)

;; +----------------------------------------------------------------------------
;; | Class+ Implementation
;; +----------------------------------------------------------------------------
(defclass class+ (standard-class)
  ((rest :initarg :rest :initform nil :accessor class+.rest)
   (ctor :initarg :ctor :initform nil :accessor class+.%ctor)
   (methods :initarg :methods :initform nil :accessor class+.%methods)
   (%timestamp :accessor class+.timestamp))
  (:documentation "Metaclass of Class+ classes"))

(defprint-object (self class+ :identity t :type t)
  (format t "~A" (class-name self)))

(defmethod shared-initialize :after ((class class+) slot-names &key &allow-other-keys)
  (prog1 class (setf (class+.timestamp class) (get-universal-time))))

;; -----------------------------------------------------------------------------
;; Class+ Protocol Implementation
;; -----------------------------------------------------------------------------
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

(defmethod class+.slots ((class class+))
  (when (not (class-finalized-p class))
    (finalize-inheritance class))

  (filter (lambda (s)
	    (and (typep s 'class+-slot-definition)
		 (not (eq 'none (slot-definition-host s)))))
	  (class-slots class)))

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

;; ----------------------------------------------------------------------------
;; Extra Definitions
;; ----------------------------------------------------------------------------
(defmethod class+.slot-search ((class class+) match-p)
  (filter match-p (reverse (class+.slots class))))

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

(defmethod class+.remote-methods ((class class+))
  (filter (lambda (m) (eq 'remote (cadr m)))
	  (class+.methods class)))

;; -----------------------------------------------------------------------------
;; MOP Implementation
;; -----------------------------------------------------------------------------
(defmethod validate-superclass ((class class+) (super standard-class)) t)
(defmethod validate-superclass ((class standard-class) (super class+)) nil)

(defclass class+-slot-definition (standard-slot-definition)
  ((host :initarg :host :initform 'local :accessor slot-definition-host)
   (client-type :initarg :client-type :initform 'primitive :accessor slot-definition-client-type)
   (relation :initarg :relation :initform nil :accessor slot-definition-relation)
   (index :initarg :index :initform nil :accessor slot-definition-index)
   (print :initarg :print :initform nil :accessor slot-definition-print)
   (label :initarg :label :initform nil :accessor slot-definition-label)))

(defclass class+-direct-slot-definition (class+-slot-definition standard-direct-slot-definition)
  ())

(defclass class+-effective-slot-definition (class+-slot-definition standard-effective-slot-definition)
  ())

(defmethod direct-slot-definition-class ((class class+) &rest initargs)
  (declare (ignore initargs))
  (find-class 'class+-direct-slot-definition))

(defmethod effective-slot-definition-class ((class class+) &rest initargs)
  (declare (ignore initargs))
  (find-class 'class+-effective-slot-definition))

(defmethod %class+-inherited-slots ((class class+))
  '(host client-type sb-pcl::readers sb-pcl::writers relation index print label))

(defmethod compute-effective-slot-definition ((class class+) slot-name slot-defs)
  (flet ((copy-slots (slots source target)
	   (mapc (lambda (slot)
		   (when (typep source 'class+-direct-slot-definition)
		     (setf (slot-value target slot) (slot-value source slot))))
		 slots)
	   target))
    (copy-slots (%class+-inherited-slots class) (car slot-defs) (call-next-method))))

;; ----------------------------------------------------------------------------
;; Slot Definition Sugars
;; ----------------------------------------------------------------------------
(defmethod slot-definition-supplied-p ((slot class+-slot-definition))
  (intern (format nil "~A-SUPPLIED-P" (slot-definition-name slot))))

(defmethod slot-definition-to-plist ((slot class+-slot-definition))
  (let ((initarg (car (reverse (slot-definition-initargs slot)))))
    (list :name (slot-definition-name slot)
	  :initarg initarg
	  :initform (let ((initform (assoc initarg
					   (reverse
					    (class-default-initargs
					     (class-name (sb-pcl::slot-definition-class slot)))))))
		      (if initform
			  (cadr initform)		      
			  (slot-definition-initform slot)))
	  :supplied-p (slot-definition-supplied-p slot)
	  :host (slot-definition-host slot)
	  :client-type (slot-definition-client-type slot)
	  :type (sb-pcl::slot-definition-type slot)
	  :relation (slot-definition-relation slot)
	  :writer (car (reverse (sb-pcl::slot-definition-writers slot)))
	  :reader (car (reverse (sb-pcl::slot-definition-readers slot)))
	  :index (slot-definition-index slot)
	  :label (slot-definition-label slot))))

(defmacro with-slotdef (arglist slot &body body)
  `(destructuring-bind (&key ,@arglist &allow-other-keys)
       (slot-definition-to-plist ,slot)
     ,@body))

(defmethod slot-definition-singular-type ((slot class+-slot-definition))
  (with-slotdef (type) slot
    (intern (subseq (symbol-name type) 0 (1- (length (symbol-name type))))
	    (symbol-package type))))

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
	 (not (null (find-class+ (intern (subseq name 0 (1- (length name))))))))))

(defmethod slot-definition-relational-slot ((slot class+-slot-definition))
  (let* ((type (if (slot-definition-singular-typep slot)
		   (sb-pcl::slot-definition-type slot)
		   (slot-definition-singular-type slot))))
    (class+.find-slot (find-class+ type) (slot-definition-relation slot))))

;; ----------------------------------------------------------------------------
;; Relations
;; ----------------------------------------------------------------------------
(defmethod class+.relations ((class class+))
  (filter (lambda (slot)
	    (not (null (slot-definition-relation slot))))
	  (class+.slots class)))

(defmethod class+.1-to-n-relations ((class class+))  
  (filter (lambda (slot)
	    (and (slot-definition-plural-typep slot)
		 (slot-definition-singular-typep (slot-definition-relational-slot slot))))
	  (class+.relations class)))

(defmethod class+.n-to-1-relations ((class class+))
  (filter (lambda (slot)
	    (and (slot-definition-singular-typep slot)
		 (slot-definition-plural-typep (slot-definition-relational-slot slot))))
	  (class+.relations class)))

(defmethod class+.n-to-n-relations ((class class+))
  (filter (lambda (slot)
	    (and (slot-definition-plural-typep slot)
		 (slot-definition-plural-typep (slot-definition-relational-slot slot))))
	  (class+.relations class)))

(defmethod slot-definition-relation-type ((slot class+-slot-definition))
  (let ((relational-slot (slot-definition-relational-slot slot)))
    (values
     (cond
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
       (t (error "Unknown type of relation slot: ~A" slot)))
     relational-slot)))

;; ----------------------------------------------------------------------------
;; Indexes
;; ----------------------------------------------------------------------------
(defmethod class+.indexes ((class class+))
  (filter (lambda (slot)
	    (with-slotdef (index) slot
	      index))
	  (class+.slots class)))

;; ----------------------------------------------------------------------------
;; Constructor Generation Methods
;; ----------------------------------------------------------------------------
(defmethod class+.ctor-lambda-list ((self class+) &optional (include-supplied-p nil))
  (reduce0 (lambda (acc slot)
	     (with-slotdef (initarg initform supplied-p) slot
	       (let ((symbol (intern (symbol-name initarg))))
		 (if include-supplied-p
		     (cons (list symbol initform supplied-p) acc)
		     (cons (list symbol initform) acc)))))
	   (reverse (class+.local-slots self))))
  
(defmethod class+.ctor-arguments ((self class+) &optional lambda-list)      
  (reduce0 (lambda (acc slot)
	     (with-slotdef (initarg) slot	       
	       (cons initarg (cons (intern (symbol-name initarg)) acc))))
	   (aif (extract-argument-names lambda-list)
		(filter (lambda (slot)
			  (with-slotdef (name) slot
			    (member name it)))
			 (class+.local-slots self))
		(reverse (class+.local-slots self)))))

(defmethod class+.all-ctor-lambda-list ((self class+) &optional (include-supplied-p nil))
  (reduce0 (lambda (acc slot)
	     (with-slotdef (initarg initform supplied-p) slot
	       (let ((symbol (intern (symbol-name initarg))))
		 (if include-supplied-p
		     (cons (list symbol initform supplied-p) acc)
		     (cons (list symbol initform) acc)))))
	   (reverse (class+.slots self))))

(defmethod class+.all-ctor-arguments ((self class+) &optional lambda-list)      
  (reduce0 (lambda (acc slot)
	     (with-slotdef (initarg) slot	       
	       (cons initarg (cons (intern (symbol-name initarg)) acc))))
	   (aif (extract-argument-names lambda-list)
		(filter (lambda (slot)
			  (with-slotdef (name) slot
			    (member name it)))
			 (class+.slots self))
		(reverse (class+.slots self)))))

(defmethod class+.%ctor ((self class+))
  (caar (slot-value self 'ctor)))

(defmethod class+.ctor ((self class+))
  (let ((name (class+.name self)))
    (aif (class+.%ctor self)
	 `(defun ,name ,it
	    (make-instance ',name ,@(class+.ctor-arguments self it)))
	 (let ((keywords (class+.ctor-lambda-list self t)))
	   `(defun ,name (&key ,@keywords)
	      (declare (ignorable ,@(mapcar #'caddr keywords)))
	      (make-instance ',name ,@(class+.ctor-arguments self)))))))

(defmacro defclass+-ctor (name)
  (class+.ctor (find-class+ name)))

(defmacro defclass+-print-object (name)
  (let* ((class+ (find-class+ name))
	 (slots (filter #'slot-definition-print (class+.slots class+))))
    `(defprint-object (self ,name)
       (format t "~{~A~^ ~}" (list ,@(nreverse
				      (reduce0 (lambda (acc slot)
						 (cons `(list ',(slot-definition-name slot)
							      (slot-value self ',(slot-definition-name slot)))
						       acc))
					       slots)))))))

;; ----------------------------------------------------------------------------
;; defclass+ Macro
;; ----------------------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %fix-slot-definition (class-name slot-definition)
    (if (not (member :initform slot-definition))
	(setf (getf (cdr slot-definition) :initform) nil))

    (if (not (member :initarg slot-definition))
	(setf (getf (cdr slot-definition) :initarg)
	      (make-keyword (car slot-definition))))

    (if (not (or (member :accessor slot-definition)
		 (member :reader slot-definition)
		 (member :writer slot-definition)))
	(setf (getf (cdr slot-definition) :accessor)
	      (intern (format nil "~A.~A" class-name (car slot-definition)))))

    slot-definition)
  
  (defun %filter-rest (rest)
    (append (list `(:ctor ,(cdr (assoc :ctor rest))))
	    (list `(:metaclass ,(or (cadr (assoc :metaclass rest)) 'class+)))
	    (list `(:rest ',rest))
	    (filter (lambda (a)
		      (and (not (eq :ctor (car a)))
			   (not (eq :metaclass (car a)))))
		    rest))))

(defclass class+-object ()
  ()
  (:metaclass class+))

(defmacro defclass+ (name supers slots &rest rest)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (deftype ,(intern (format nil "~A*" name)) ()
	 '(or null cons))
       (defclass ,name (,@supers class+-object;; object-with-id
				 )
	 ,(mapcar (lambda (slot) (%fix-slot-definition name slot)) slots)
	 ,@(%filter-rest rest))       
       (fmakunbound ',name)
       (defclass+-ctor ,name))
     (defclass+-print-object ,name)
     (find-class ',name)))

;; (defclass+ user1 ()
;;   ((username :host local)
;;    (password :host remote)
;;    (email)
;;    (address :host none))
;;   (:documentation "eben"))

;; Core Server: Web Application Server

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

