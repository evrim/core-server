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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :core-server)

;;+----------------------------------------------------------------------------
;;| Class+ Framework for [Core-serveR]
;;+----------------------------------------------------------------------------
;;
;; Class+ framework is used to save several class definition parameters
;; so that they can be reached during compile/execution time. MOP can
;; also provide us some of the information but since there are numerous
;; incompatible Common-lisp implementations, we prefered a new framework.
;;
;; The parameters that Class+ saves are:
;;
;;     * Local Slots
;;     * Remote Slots
;;     * Local Methods
;;     * Remote Methods
;;     * Default Initial Arguments (default-initargs)
;;     * Value of a new keyword called 'host'
;;
;; Generally, those parameters are accessed during compile time. Several
;; frameworks inside [Core-serveR] uses Class+:
;; i)  Command Framework - Used to execute unix shell commands
;; ii) Component Framework - Used to generate remote proxies for web widgets
;; 
;; defclass+ is the main macro for class+ framework. Interface functions are
;; as follows:
;;
;; - local-slots-of-class
;; - remote-slots-of-class
;; - default-initargs-of-class
;; - client-type-of-slot
;; - local-methods-of-class
;; - remote-methods-of-class
;;
;; See http://labs.core.gen.tr/#classplus for more documentation.

(defvar +class-registry+ (make-hash-table :test #'equal)
  "Registry of the Class+ Framework")

(defun find-class+ (class+)
  "Returns the instance of class+ associated with the symbol named 'class+'."
  (etypecase class+
    (symbol (gethash class+ +class-registry+))
    (class+ class+)))

(defgeneric class+.direct-superclasses (class+)
  (:documentation "Returns direct super classes+ of this class+")
  (:method ((self t)) nil))

(defgeneric class+.superclasses (class+)
  (:documentation "Returns super classes+ of this class+")
  (:method ((self t)) nil))

(defgeneric class+.direct-subclasses (class+)
  (:documentation "Returns direct sub classes+ of this class+")
  (:method ((self t)) nil))

(defgeneric class+.subclasses (class+)
  (:documentation "Returns sub classes+ of this class+")
  (:method ((self t)) nil))

(defgeneric class+.slots (class+)
  (:documentation "Returns all slots of this class+")
  (:method ((self t)) nil))

(defgeneric class+.slot-search (class+ match-p)
  (:documentation "Returns all slots that satisfies match-p lambda")
  (:method ((self t) match-p) nil))

(defgeneric class+.local-slots (class+)
  (:documentation "Returns an assoc list of local-slots with corresponding default values")
  (:method ((self t)) nil))

(defgeneric class+.remote-slots (class+)
  (:documentation "Returns an assoc list of remote-slots with corresponding default values")
  (:method ((self t)) nil))

(defgeneric class+.methods (class+)
  (:documentation "Returns list of all method-names and associated lambda lists")
  (:method ((self t)) nil))

(defgeneric class+.local-methods (class+)
  (:documentation "Returns list of local method-names and associated lambda lists")
  (:method ((self t)) nil))

(defgeneric class+.remote-methods (class+)
  (:documentation "Returns list of remote method-names and associated lambda lists ")
  (:method ((self t)) nil))

(defgeneric class+.search (class+ goal-p &optional successors combiner)
  (:documentation "Convenient search method that by default applies goal-p lambda to
all superclasses of this class+ until goal-p returns a value other than nil"))

(defgeneric class+.rest (class+)
  (:method ((self t)) nil))

(defgeneric class+.slots (class+)
  (:method ((self t)) nil))

(defgeneric class+.slot-search (class+ match-p)
  (:method ((self t) match-p) nil))

(defclass class+ ()
  ((name :accessor class+.name :initarg :name :initform (error "Please specify the name of the Class+")
	 :documentation "Name of this class+")
   (superclasses :accessor class+.%superclasses :initarg :superclasses :initform nil
		 :documentation "Symbols of superclasses of this class+")
   (subclasses :accessor class+.%subclasses :initarg :subclasses :initform nil
	       :documentation "Symbols of subclasses of this class+")
   (slots :accessor class+.%slots :initarg :slots :initform nil
	  :documentation "Slots of this class+")
   (rest :accessor class+.rest :initarg :rest :initform nil)
   (methods :accessor class+.%methods :initform nil :initarg :methods))
  (:documentation "Class+ is the base class of Class+ Framework. It
  provides similar functionality like mop metaclasses on the user
  land. Class+ instances can be queried using class+ protocol."))

(defmethod print-object ((self class+) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~A" (class+.name self))))

(defmethod class+.direct-superclasses ((self class+))
  (nreverse
   (reduce0 (lambda (acc atom)
	      (aif (find-class+ atom)
		   (cons it acc)
		   acc))
	    (class+.%superclasses self))))

(defmethod class+.direct-subclasses ((self class+))
  (mapcar #'find-class+ (class+.%subclasses self)))

(defmethod class+.search ((class+ class+) goal-p
			  &optional (successors #'class+.direct-superclasses)
		                    (combiner #'append))  
  (core-search (list class+) goal-p successors combiner))

(defmethod class+.superclasses ((self class+))
  (let (lst)    
    (class+.search self
		   (lambda (class+)
		     (prog1 nil (pushnew class+ lst))))
    lst))

(defmethod class+.subclasses ((self class+))
  (let (lst)    
    (class+.search self
		   (lambda (class+)
		     (prog1 nil (pushnew class+ lst)))
		   #'class+.direct-subclasses)
    lst))

(defmethod class+.default-initargs ((self class+))
  (let (lst)
    (class+.search self
		   (lambda (class+)
		     (mapc (lambda (initarg)
			     (let ((keyword (make-keyword (car initarg))))
			       (if (not (member keyword lst))
				   (setf (getf lst (make-keyword (car initarg)))
					 (cdr initarg)))))
			   (plist-to-alist (cdr (assoc :default-initargs (class+.rest class+)))))
		     nil))
    lst))

(defmethod class+.slot-search ((self class+) match-p)
  (let ((lst)
	(initargs (class+.default-initargs self)))
    (class+.search self
		   (lambda (class+)
		     (mapc (lambda (slot)
			     (when (funcall match-p slot)
			       (let ((initarg (getf (cdr slot) :initarg)))
				 (if (member initarg initargs)
				     (let ((slot (copy-list slot)))
				       (setf (getf (cdr slot) :initform) (getf initargs initarg))
				       (pushnew slot lst :key #'car))
				     (pushnew slot lst :key #'car)))))
			   (class+.%slots class+))
		     nil))
    (nreverse lst)))

(defmethod class+.slots ((self class+))
  (class+.slot-search self (lambda (slot)
			     (declare (ignore slot))
			     t)))

(defmethod class+.local-slots ((self class+))
  (class+.slot-search self
		      (lambda (slot)
			(or (eq (getf (cdr slot) :host) 'local)
			    (eq (getf (cdr slot) :host) 'both)))))

(defmethod class+.remote-slots ((self class+))
  (class+.slot-search self
		      (lambda (slot)
			(or (eq (getf (cdr slot) :host) 'remote)
			    (eq (getf (cdr slot) :host) 'both)))))

(defmethod class+.register-remote-method ((self class+) name args)
  (setf (class+.%methods self)
	(cons (cons name (cons :remote args))
	      (remove name (class+.%methods self) :key #'car)))
  self)

(defmethod class+.register-local-method ((self class+) name args)
  (setf (class+.%methods self)
	(cons (cons name (cons :local args))
	      (remove name (class+.%methods self) :key #'car)))
  self)

(defmethod class+.methods ((self class+))
  (let (lst)
    (class+.search self
		   (lambda (class+)
		     (mapc (lambda (method)
			     (pushnew method lst :key #'car))
			   (class+.%methods class+))
		     nil))
    lst))

(defmethod class+.local-methods ((self class+))
  (filter (lambda (method)
	    (eq (cadr method) :local))
	  (class+.methods self)))

(defmethod class+.remote-methods ((self class+))
  (filter (lambda (method)
	    (eq (cadr method) :remote))
	  (class+.methods self)))

(defun class+.%fix-slot-definition (class-name slot-definition)
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

  (if (not (member :client-type slot-definition))
      (setf (getf (cdr slot-definition) :client-type) 'primitive))

  slot-definition)

(defun class+.%apply-default-initargs (slot-definition default-initargs)  
  (if (member (getf (cdr slot-definition) :initarg) default-initargs)
      (setf (getf (cdr slot-definition) :initform)
	    (getf (cdr default-initargs) (getf (cdr slot-definition) :initarg))))
  slot-definition)

(defun class+.%slot-definitions (class-name slots default-initargs)
  (mapcar (lambda (slot)
	    (class+.%apply-default-initargs
	     (class+.%fix-slot-definition class-name (copy-list slot))
	     default-initargs))
	  slots))

(defparameter +class-slot-keywords+
  '(:reader :writer :accessor :allocation :initarg :initform :type :documentation))

(defun class+.%filter-slot-definitions (slots)
  (mapcar (lambda (slot)
	    (cons (car slot)
		  (alist-to-plist
		   (filter (lambda (slot)
			     (member (car slot) +class-slot-keywords+ :test #'string=))
			   (plist-to-alist (cdr slot))))))
	  slots))

(defparameter +class-rest-keywords+
  '(:default-initargs :documentation :metaclass))

(defun class+.%filter-rest (rest)
  (filter (lambda (atom)
	    (member (car atom) +class-rest-keywords+ :test #'string=))
	  rest))

(defmethod class+.ctor-keywords ((self class+))
  (reduce0 (lambda (acc atom)
	     (cons (list (intern (symbol-name (getf (cdr atom) :initarg)))
			 (getf (cdr atom) :initform))
		   acc))
	   (reverse (class+.local-slots self))))
  
(defmethod class+.ctor-arguments ((self class+) &optional lambda-list)      
  (reduce0 (lambda (acc slot)
	     (cons (getf (cdr slot) :initarg) (cons (car slot) acc)))
	   (aif (extract-argument-names lambda-list)
		(reduce0 (lambda (acc atom)
			   (if (member (car atom) it) (cons atom acc) acc))
			 (class+.local-slots self))
		(reverse (class+.local-slots self)))))

(defmethod class+.ctor ((self class+))
  (let ((name (class+.name self)))
    (aif (cdr (assoc :ctor (class+.rest self)))
	 `(defun ,name ,@it
	    (make-instance ',name ,@(class+.ctor-arguments self)))
	 `(defun ,name (&key ,@(class+.ctor-keywords self))
	    (make-instance ',name ,@(class+.ctor-arguments self))))))

(defun class+ (name supers slots rest)
  "Convenient constructor for Class+"
  (make-instance 'class+
		 :name name
		 :superclasses supers
		 :slots (class+.%slot-definitions name slots (assoc :default-initargs rest))
		 :rest rest))

(defmethod class+.register ((self class+))  
  (aif (find-class+ (class+.name self))
       (setf (class+.%methods self) (class+.%methods it)))
  (setf (gethash (class+.name self) +class-registry+) self)
  (values self
	  (class+.%filter-slot-definitions (class+.%slots self))
	  (class+.%filter-rest (class+.rest self))))

(defmacro defclass+ (name supers slots &rest rest)
  "Defines a class using Class+ Framework"  
  (multiple-value-bind (class+ new-slots new-rest) (class+.register (class+ name supers slots rest))    
    `(progn
       ,(class+.ctor class+)
       (eval-when (:load-toplevel)
	 (class+.register (class+ ',name ',supers ',slots ',rest)))
       (defclass ,name (,@supers) ,new-slots ,@new-rest))))

(deftrace class+
    '(find-class+ class+ class+.direct-superclasses class+.direct-subclasses
      class+.superclasses class+.subclasses class+.slots
      class+.rest class+.default-initargs class+.slot-search
      class+.local-slots class+.remote-slots class+.methods
      class+.local-methods class+.remote-methods class+.search class+.ctor
      class+.register class+.register-remote-method class+.register-local-method
      class+.ctor-keywords class+.ctor-arguments))

;; (defclass+-slot-extension client-type (class+)
;;   (:values '(primitive object list))
;;   (:predicate (lambda (value)
;; 		(member value +some-values+)))
;;   (:default-value 'primitive))

;; (defclass+-slot-extension host (class+)
;;   (:values '(nil local remote both))
;;   (:default-value nil))
