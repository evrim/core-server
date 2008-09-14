(in-package :core-server)

;; +----------------------------------------------------------------------------
;; | Class+ Implementation
;; +----------------------------------------------------------------------------
(defclass class+ (standard-class)
  ((rest :initarg :rest :initform nil :accessor class+.rest)
   (ctor :initarg :ctor :initform nil :accessor class+.%ctor)
   (methods :initarg :methods :initform nil :accessor class+.%methods)
   (%timestamp))
  (:documentation "Metaclass of Class+ classes"))

(defprint-object (self class+ :identity t :type t)
  (format t "~A" (class-name self)))

(defmethod shared-initialize :after ((class class+) slot-names &key &allow-other-keys)
  (prog1 class (setf (slot-value class '%timestamp) (get-universal-time))))

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
  (error "Not implemented yet."))

(defmethod class+.subclasses ((class class+))
  (error "Not implemented yet."))

(defmethod class+.slots ((class class+))
  (when (not (class-finalized-p class))
    (finalize-inheritance class))

  (filter (lambda (s) (typep s 'class+-slot-definition)) (class-slots class)))

(defmethod class+.methods ((class class+))
  (error "Not implemented yet."))

;; ----------------------------------------------------------------------------
;; Extra Definitions
;; ----------------------------------------------------------------------------
(defmethod class+.slot-search ((class class+) match-p)
  (filter match-p (class+.slots class)))

(defmethod class+.local-slots ((class class+))
  (class+.slot-search class (lambda (s) (eq 'local (slot-value s 'host)))))

(defmethod class+.remote-slots ((self class+))
  (class+.slot-search self (lambda (s) (eq 'remote (slot-value s 'host)))))

(defmethod class+.local-methods ((class class+))
  (error "Not implemented yet."))

(defmethod class+.remote-methods ((class class+))
  (error "Not implemented yet."))

;; -----------------------------------------------------------------------------
;; MOP Implementation
;; -----------------------------------------------------------------------------
(defmethod validate-superclass ((class class+) (super standard-class)) t)
(defmethod validate-superclass ((class standard-class) (super class+)) nil)

(defclass class+-slot-definition (standard-slot-definition)
  ((host :initarg :host :initform 'local :accessor slot-definition-host)
   (client-type :initarg :client-type :initform 'primitive :accessor slot-definition-client-type)
   (relation :initarg :relation :initform nil :accessor slot-definition-relation)
   (index :initarg :index :initform nil :accessor slot-definition-index)))

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
  '(host client-type sb-pcl::readers sb-pcl::writers relation index))

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
	  :writer (slot-definition-writer-function slot)
	  :reader (slot-definition-reader-function slot)
	  :index (slot-definition-index slot))))

(defmacro with-slotdef (arglist slot &body body)
  `(destructuring-bind (&key ,@arglist &allow-other-keys)
       (slot-definition-to-plist ,slot)
     ,@body))

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

(defmacro defclass+ (name supers slots &rest rest)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (deftype ,(intern (format nil "~A*" name)) ()
	 '(or null cons))
       (defclass ,name (,@supers object-with-id)
	 ,(mapcar (lambda (slot) (%fix-slot-definition name slot)) slots)
	 ,@(%filter-rest rest))       
       (fmakunbound ',name)
       (defclass+-ctor ,name))
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

