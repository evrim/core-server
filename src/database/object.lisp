;; +----------------------------------------------------------------------------
;; | Object Database Interface
;; +----------------------------------------------------------------------------
(in-package :core-server)

;; ----------------------------------------------------------------------------
;; Protocol
;; ----------------------------------------------------------------------------
(defgeneric find-all-objects (server class)
  (:documentation "Returns all objects that are member of class"))

(defmethod find-all-objects ((server database) (class symbol))
  (find-all-objects server (find-class class)))

(defgeneric find-object-with-slot (server class slot value)
  (:documentation "Returns object that is a member of class having slot 'value'"))

(defmethod find-object-with-slot ((server database) (class symbol) (slot symbol) value)
  (find-object-with-slot server (find-class class) slot value))

(defgeneric add-object (server class &rest slots-and-values)
  (:documentation "Creates a new instances of class having slots-and-values"))

(defmethod add-object ((server database) (class symbol) &rest slots-and-values)
  (apply #'add-object server (find-class class) slots-and-values))

(defgeneric update-object (server instance &rest slots-and-values)
  (:documentation "Updates instance with slots-and-values"))

(defgeneric delete-object (server instance)
  (:documentation "Removes instance"))

;; +----------------------------------------------------------------------------
;; | Simple Object Database (no slot index, only class indexes)
;; +----------------------------------------------------------------------------
(defmethod class-index-name ((server database) (class standard-class))
  (class-name class))

(defmethod class-index ((server database) (class standard-class))
  (gethash (class-index-name server class) (database.root server)))

(defmethod (setf class-index) (value (server database) (class standard-class))
  (setf (gethash (class-index-name server class) (database.root server)) value))

(defmethod add-to-class-index ((server database) object)
  (setf (class-index server (class-of object))
	(cons object (class-index server (class-of object)))))

(defmethod delete-from-class-index ((server database) object)
  (setf (class-index server (class-of object))
	(delete object (class-index server (class-of object)))))

(defmethod find-all-objects ((server database) (class standard-class))
  (class-index server class))

(defmethod find-object-with-slot ((server database) (class standard-class) (slot symbol) value)
  (find value (find-all-objects server class)
        :key (lambda (object) (slot-value object slot))
        :test #'equal))

(deftransaction update-object ((server database) (object standard-object) &rest slots-and-values)
  (reduce (lambda (object slot-val)	    
	    (setf (slot-value object (car slot-val)) (cdr slot-val))
	    object)
	  slots-and-values :initial-value object))

(deftransaction add-object ((server database) (class standard-class) &rest slots-and-values)
  (let ((object (allocate-instance class)))
    (apply #'update-object server object slots-and-values)
    (initialize-instance object)
    (add-to-class-index server object)
    object))

(deftransaction delete-object ((server database) (object standard-object))
  (prog1 object (delete-from-class-index server object)))

;; +----------------------------------------------------------------------------
;; | Extended Object Database (class+, slot indexes, relations)
;; +----------------------------------------------------------------------------

;; Object with id is defined in src/class+/class+.lisp
;; (defclass object-with-id ()
;;   ((id :host both :index t :reader get-id :initform -1 :initarg :id :print t))
;;   (:metaclass class+))

(defmethod database.serialize ((self abstract-database) (object object-with-id)
			       &optional (k (curry #'database.serialize self)))
  (declare (ignore k))
  (<db:object-with-id :id (format nil "~D" (slot-value object 'id))))

(defmethod database.deserialize ((self abstract-database) (object <db:object-with-id)
				 &optional (k (curry #'database.deserialize self)))
  (declare (ignore k))
  (find-object-with-slot self 'object-with-id 'id (parse-integer (slot-value object 'id))))

(deftransaction next-id ((server database))
  (let ((current (database.get server :id-counter)))
    (if current
	(setf (database.get server :id-counter) (1+ current))
	(setf (database.get server :id-counter) 1))))

(defmethod slot-index-name ((server database) (class standard-class) slot)
  (any (lambda (class)
	 (if (member slot (mapcar #'slot-definition-name (mopp::class-direct-slots class)))
	     (intern (concat (symbol-name (class-name class)) "-" (symbol-name slot)))))
       (reverse (class-superclasses class))))

(defmethod slot-index-name ((server database) (object standard-object) slot)
  (slot-index-name server (class-of object) slot))

(defmethod slot-index-name ((server database) (class symbol) slot)
  (slot-index-name server (find-class class) slot))

(defmethod (setf slot-index) (value (server database) object slot)
  (setf (gethash (slot-index-name server object slot) (database.root server))
	value))

(defmethod slot-index ((server database) object slot)
  (or (gethash (slot-index-name server object slot) (database.root server))
      (setf (slot-index server object slot) (make-hash-table :test #'equal))))

(defmethod add-to-slot-index ((server database) object slot)
  (when (slot-boundp object slot)
    (setf (gethash (slot-value object slot) (slot-index server object slot))
	  object))
  object)

(defmethod delete-from-slot-index ((server database) object slot)
  (when (slot-boundp object slot)
    (remhash (slot-value object slot) (slot-index server object slot)))
  object)

(defmethod find-object-with-slot ((server database) (class class+) (slot symbol) value)
  (let ((slot (class+.find-slot class slot)))
    (if (slot-definition-index slot)
	(gethash value (slot-index server class (slot-definition-name slot)))
	(call-next-method))))

(defmethod find-object-with-id ((server database) id)
  (find-object-with-slot server (find-class+ 'object-with-id) 'id id))

(deftransaction update-object ((server database) (object class+-object) &rest slots-and-values)
  (reduce
   (lambda (object slot-val)	    
     (destructuring-bind (name . new-value) slot-val
       (let ((old-value (and (slot-boundp object name) (slot-value object name))))		
	 (let ((slot (class+.find-slot (class-of object) name)))
	   (with-slotdef (index relation) slot

	     (cond
	       ((and relation old-value)
		(multiple-value-bind (relation-type relational-slot) (slot-definition-relation-type slot)
		  (case relation-type
		    (:n-to-one
		     (setf (slot-value old-value (slot-definition-name relational-slot))				 
			   (remove object (slot-value old-value (slot-definition-name relational-slot)))))
		    (:one-to-n
		     (reduce (lambda (object old-value)
			       (setf (slot-value old-value (slot-definition-name relational-slot))
				     object)
			       object)
			     (ensure-list old-value) :initial-value object))
		    (:n-to-n
		     (reduce (lambda (object target)
			       (setf (slot-value target (slot-definition-name relational-slot))
				     (remove object (slot-value target (slot-definition-name relational-slot))))
			       object)
			     old-value :initial-value object)))))
	       ((and (null relation) index)
		(delete-from-slot-index server object name)))
		  
	     (setf (slot-value object name) new-value)

	     (cond
	       ((and (null relation) index)
		(add-to-slot-index server object name))
	       ((and relation new-value)		       
		(multiple-value-bind (relation-type relational-slot) (slot-definition-relation-type slot)
		  (case relation-type
		    (:n-to-one
		     (setf (slot-value new-value (slot-definition-name relational-slot))
			   (cons object
				 (remove object (slot-value new-value (slot-definition-name relational-slot))))))
		    (:one-to-n
		     (reduce (lambda (object new-value)
			       (setf (slot-value new-value (slot-definition-name relational-slot))
				     object)
			       object)
			     (ensure-list new-value) :initial-value object))
		    (:n-to-n
		     (reduce (lambda (object target)
			       (setf (slot-value target (slot-definition-name relational-slot))
				     (cons object (slot-value target (slot-definition-name relational-slot))))
			       object)
			     new-value :initial-value object))))))))))
     object)
   slots-and-values :initial-value object))

(deftransaction add-object ((server database) (class class+) &rest slots-and-values)
  (if (member (find-class+ 'object-with-id) (class+.superclasses class))
      (setf slots-and-values
	    (cons (cons 'id (next-id server))
		  (remove 'id slots-and-values :key #'car))))
  
  (let ((object (allocate-instance class)))
    (add-to-class-index server object)
    (apply #'update-object server object slots-and-values)
    (initialize-instance object)
    object))

(deftransaction delete-object ((server database) (object class+-object))
  ;; Remove From Class Index
  (delete-from-class-index server object)

  ;; Remove Indexes
  (reduce (lambda (object slot)
	    (delete-from-slot-index server object (slot-definition-name slot))
	    object)
	  (class+.indexes (class-of object)) :initial-value object)

  ;; Remove From Relational Objects
  (reduce (lambda (object slot)
	    (let ((old-value (and (slot-boundp object (slot-definition-name slot))
				  (slot-value object  (slot-definition-name slot)))))
	      (when old-value
		(multiple-value-bind (relation-type relational-slot) (slot-definition-relation-type slot)
		  (case relation-type
		    (:n-to-one
		     (setf (slot-value old-value (slot-definition-name relational-slot))				 
			   (remove object (slot-value old-value (slot-definition-name relational-slot)))))
		    (:one-to-n
		     (reduce (lambda (object old-value)
			       (setf (slot-value old-value (slot-definition-name relational-slot))
				     nil)
			       object)
			     (ensure-list old-value) :initial-value object))
		    (:n-to-n
		     (reduce (lambda (object target)
			       (setf (slot-value target (slot-definition-name relational-slot))
				     (remove object (slot-value target (slot-definition-name relational-slot))))
			       object)
			     old-value :initial-value object)))))
	      object))
	  (class+.relations (class-of object)) :initial-value object)
  object)

(deftrace object-database
    '(add-object delete-object update-object find-all-objects
      find-object-with-slot))

;; Core Server: Web Application Server

;; Copyright (C) 2006-2008  Metin Evrim Ulu

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
