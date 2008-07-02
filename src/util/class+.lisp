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

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defvar +class-registry+ (make-hash-table :test #'equal)
    "Registry of the Class+ Framework")

  (defun class-successor (class)
    "Returns successor of 'class' in the registry"
    (mapcar (lambda (class) (gethash class +class-registry+))
	    (getf class :supers)))

  (defun list-or-atom-key (a)
    (if (listp a) (car a) a))
  
  (defun class-search (name type &optional (key #'list-or-atom-key))
    "Class tree search function for class 'name'. 'type' can be:
i)  :local-slots - Returns local slots of the class
ii) :remote-slots - Returns remote slots of the class 
iii):default-initargs - Returns default-initargs of the class
iv) :client-types - Returns client-types of the class
v)  :local-methods - Returns local-methods of the class
vi) :remote-methods - Return remote-methods of the class"
    (let (lst)
      (core-search (cons (gethash name +class-registry+) nil)
		   (lambda (atom)
		     (aif (getf atom type)
			  (mapcar (lambda (slot) (pushnew slot lst :key key))
				  (ensure-list it)))
		     nil)
		   #'class-successor #'append)
      lst))

  (defun local-slots-of-class (name)
    "Returns local slots of the class 'name'"
    (class-search name :local-slots))

  (defun remote-slots-of-class (name)
    "Returns remote slots of the class 'name'"
    (class-search name :remote-slots))

  (defun local-initargs-of-class (name)
    (class-search name :local-initargs))

  (defun remote-initargs-of-class (name)
    (class-search name :remote-initargs))

  (defun default-initargs-of-class (name)
    "Returns default-initargs of the class 'name'"
    (class-search name :default-initargs))

  (defun client-type-of-slot (name slot)
    "Returns client-type of the slot 'slot' of the class 'name'. Default
client-type is :primitive"
    (core-search (cons (gethash name +class-registry+) nil)
		 (lambda (atom)
		   (aif (cdr (assoc slot (getf atom :client-types)))
			(return-from client-type-of-slot it)))
		 #'class-successor
		 #'append)
    nil)

  (defun local-methods-of-class (name)
    "Returns local-methods of the class 'name'"
    (class-search name :local-methods))

  (defun remote-methods-of-class (name)
    "Returns remote-methods of the class 'name'"
    (class-search name :remote-methods #'identity))

  (defun register-local-method-for-class (name method-name)
    (setf (getf (gethash name +class-registry+) :local-methods)
	  (cons method-name
		(remove method-name
			(getf (gethash name +class-registry+) :local-methods)))))

  (defun register-remote-method-for-class (name method-name)
    (setf (getf (gethash name +class-registry+) :remote-methods)
	  (cons method-name
		(remove method-name
			(getf (gethash name +class-registry+) :remote-methods)))))

  (defun ctor-keywords (name)
    (let* ((local-initargs (local-initargs-of-class name))
	   (default-initargs (mapcar (lambda (a) (cons (make-keyword (car a)) (cdr a)))
				     (default-initargs-of-class name)))
	   (slots (mapcar (lambda (slot)
			    (aif (cdr (assoc (car slot) local-initargs))
				 (aif (cdr (assoc it default-initargs))
				      (list (car slot) it)
				      slot)
				 slot))
			  (class-search name :local-slots))))
      (reduce #'(lambda (acc slot)
		  (aif (keywordp (cdr slot))
		       (cons (cons (intern (symbol-name (cdr slot)))
				   (cdr (assoc (car slot) slots)))
			     acc)
		       (cons (cons (cdr slot)
				   (cdr (assoc (car slot) slots)))
			     acc)))	      
	      (local-initargs-of-class name) 
	      :initial-value nil)))
  
  (defun ctor-arguments (name &optional lambda-list)
    (if lambda-list
	(let ((arguments (extract-argument-names lambda-list)))
	  (reduce #'(lambda (acc slot)
		      (if (member (intern (symbol-name (cdr slot)))
				  arguments)
			  (cons (cdr slot)
				(cons (intern (symbol-name (cdr slot)))
				      acc))
			  acc))
		  (local-initargs-of-class name) :initial-value nil))
	(reduce #'(lambda (acc slot)
		    (cons (cdr slot)
			  (cons (intern (symbol-name (cdr slot)))
				acc)))
		(local-initargs-of-class name) :initial-value nil)))
  
  (defun register-class (name supers slots rest)
    (flet ((filter-slot (slot-definition)
	     (when (or (eq 'local (getf (cdr slot-definition) :host))
		       (eq 'both  (getf (cdr slot-definition) :host)))
	       (unless (getf (cdr slot-definition) :initarg)
		 (setf (getf (cdr slot-definition) :initarg)
		       (make-keyword (car slot-definition)))))
	     (unless (getf (cdr slot-definition) :accessor)
	       (setf (getf (cdr slot-definition) :accessor)
		     (intern (format nil "~A.~A" name (car slot-definition)))))
	     (remf (cdr slot-definition) :host)
	     (remf (cdr slot-definition) :client-type)
	     slot-definition)
	   (filter-rest (acc atom)
	     (if (member (car atom) '(:default-initargs :documetation :metaclass))
		 (cons atom acc)
		 acc))
	   (client-type (slot)
	     (cons (car slot) (or (getf (cdr slot) :client-type) 'primitive)))
	   (local-slots (slots)
	     (nreverse
	      (reduce #'(lambda (acc slot)
			  (if (or (eq 'local (getf (cdr slot) :host))
				  (eq 'both (getf (cdr slot) :host)))
			      (aif (getf (cdr slot) :initform)
				   (cons (list (car slot) it) acc)
				   (cons (list (car slot) nil) acc))
			      acc))
		      slots :initial-value nil)))
	   (remote-slots (slots)
	     (nreverse
	      (reduce #'(lambda (acc slot)
			  (if (or (eq 'remote (getf (cdr slot) :host))
				  (eq 'both (getf (cdr slot) :host)))
			      (aif (getf (cdr slot) :initform)
				   (cons (list (car slot) it) acc)
				   (cons (list (car slot) nil) acc))
			      acc))
		      slots :initial-value nil)))
	   (local-initargs (slots)
	     (nreverse
	      (reduce #'(lambda (acc slot)
			  (if (or (eq 'local (getf (cdr slot) :host))
				  (eq 'both (getf (cdr slot) :host)))
			      (aif (getf (cdr slot) :initarg)
				   (cons (cons (car slot) it) acc)
				   (cons (cons (car slot) (make-keyword (car slot)))
					 acc))
			      acc))
		      slots :initial-value nil)))	   
	   (remote-initargs (slots)
	     (nreverse
	      (reduce #'(lambda (acc slot)
			  (if (or (eq 'remote (getf (cdr slot) :host))
				  (eq 'both (getf (cdr slot) :host)))
			      (aif (getf (cdr slot) :initarg)
				   (cons (cons (car slot) it) acc)
				   (cons (cons (car slot) (make-keyword (car slot)))
					 acc))
			      acc))
		      slots :initial-value nil))))
      (setf (getf (gethash name +class-registry+) :supers) supers 
	    (getf (gethash name +class-registry+) :local-slots) (local-slots slots)
	    (getf (gethash name +class-registry+) :remote-slots) (remote-slots slots)
	    (getf (gethash name +class-registry+) :local-initargs) (local-initargs slots)
	    (getf (gethash name +class-registry+) :remote-initargs) (remote-initargs slots)	    
	    (getf (gethash name +class-registry+) :client-types) (mapcar #'client-type slots)
	    (getf (gethash name +class-registry+) :default-initargs)
	    (plist-to-alist (cdr (assoc :default-initargs rest))))
      (values (mapcar (lambda (slot) (filter-slot (copy-list slot)))
		      slots)
	      (reduce #'filter-rest rest :initial-value nil)))))

(defmacro defclass+ (name supers slots &rest rest)
  "Defines a class using Class+ Framework"  
  (multiple-value-bind (new-slots new-rest) (register-class name supers slots rest)    
    `(prog1 (defclass ,name (,@supers component)
	      ,new-slots
	      (:default-initargs ,@(alist-to-plist (default-initargs-of-class name)))
	      ,@(remove :default-initargs new-rest :key #'car))
       (eval-when (:load-toplevel :compile-toplevel :execute)
	 (register-class ',name ',supers ',slots ',rest))
       ,(aif (cdr (assoc :ctor rest))
	     `(defun ,name ,@it
		(make-instance ',name ,@(ctor-arguments name (car it))))
	     `(defun ,name (&key ,@(ctor-keywords name))
		(make-instance ',name ,@(ctor-arguments name)))))))

(deftrace class+
    '(register-class register-remote-method-for-class register-local-method-for-class
      class-search local-slots-of-class remote-slots-of-class
      default-initargs-of-class client-type-of-slot local-methods-of-class
      remote-methods-of-class))
