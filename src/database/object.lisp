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

(defmethod find-objects-with-slot ((server database) (class symbol) (slot symbol) value)
  (find-objects-with-slot server (find-class class) slot value))

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
  (mapcar (lambda (class)
	    (setf (class-index server class)
		  (cons object (class-index server class))))
	  (cons (class-of object)
		(remove (class-of object) (class+.superclasses (class-of object))))))

(defmethod delete-from-class-index ((server database) object)
  (mapcar (lambda (class)
	    (setf (class-index server class)
		  (delete object (class-index server class))))
	  (cons (class-of object)
		(remove (class-of object) (class+.superclasses (class-of object))))))

(defmethod find-all-objects ((server database) (class standard-class))
  (class-index server class))

(defmethod find-objects-with-slot ((server database) (class standard-class) (slot symbol) value)
  (nreverse
   (reduce0 (lambda (acc object)
	      (if (equal value (slot-value object slot))
		  (cons object acc)
		  acc))
	    (find-all-objects server class))))

(defmethod find-object-with-slot ((server database) (class standard-class) (slot symbol) value)
  (car (find-objects-with-slot server class slot value)))

(deftransaction update-object ((server database) (object standard-object) &rest slots-and-values)
  (reduce (lambda (object slot-val)
  	    (if (slot-exists-p object (car slot-val))
		(setf (slot-value object (car slot-val)) (cdr slot-val))
		(warn "Slot ~A not found in ~A" (car slot-val)
		      object))
  	    object)
  	  slots-and-values :initial-value object))

(deftransaction add-object ((server database) (class standard-class) &rest slots-and-values)
  (let ((object (apply #'allocate-instance class
		       (class-default-initarg-values class)))
	(initargs (reduce0 #'append
		   (uniq
		    (filter (lambda (a)
			      (not (member (car a) slots-and-values :key #'car :test #'string=)))
			    (mapcar (lambda (a) (list (car a) (cadr a)))
				    (class-default-initarg-values class)))
		    :key #'car))))
    (apply #'update-object server object slots-and-values)
    (apply #'initialize-instance object initargs)
    (add-to-class-index server object)

;; -------------------------------------------------------------------------
;; Add this object to cache so that it wont be serialized again. -evrim.
;; -------------------------------------------------------------------------
    (with-slots (cache counter) (slot-value server 'database-cache)
      (let ((counter (incf counter)))
	(setf (gethash counter cache) object)
	(setf (gethash object cache) counter)))
    object))


;; -------------------------------------------------------------------------
;; We cannot remove an object from cache since logging will take place
;; after this execution. -evrim.
;; -------------------------------------------------------------------------
(deftransaction delete-object ((server database) (object standard-object))
  (prog1 object (delete-from-class-index server object)
    ;; (with-slots (cache) (slot-value server 'database-cache)
    ;;   (let ((counter (gethash object cache)))
    ;; 	(remhash counter cache)
    ;; 	(remhash object cache)))
    ))

;; +----------------------------------------------------------------------------
;; | Extended Object Database (class+, slot indexes, relations)
;; +----------------------------------------------------------------------------
(defmethod database.serialize ((self abstract-database) (object class+-instance)
			       &optional (k (curry #'database.serialize self)))
  (assert (null (any (lambda (slot)
		       (eq 'lift (slot-definition-host slot)))
		     (class+.slots (class-of object))))
	  nil "Lifted objects cant go into db: ~A" object)
  (with-slots (cache counter) (slot-value self 'database-cache)
    (multiple-value-bind (id foundp) (gethash object cache)
      (if foundp
	  (<db:ref :id (format nil "~D" id))
	  (let ((counter (incf counter)))
	    (setf (gethash object cache) counter)
	    (<db:instance :id counter
			  (funcall k (class-of object) k) 
			  (mapcar (lambda (slot)
				    (let ((slot (slot-definition-name slot)))
				      (<db:slot :name (symbol->string slot)
						(funcall k (slot-value object slot) k))))
				  (class+.local-slots (class-of object)))))))))


(defmethod database.serialize ((self abstract-database) (object html-element)
			       &optional
			       (k (curry #'database.serialize self) ))
  (if (typep object 'component)
      (call-next-method self object k)
      object))

(deftransaction next-id ((server database))
  (let ((current (database.get server :id-counter)))
    (if current
	(setf (database.get server :id-counter) (1+ current))
	(setf (database.get server :id-counter) 1))))

(defmethod slot-index-name ((server database) (class standard-class) slot)
  (any (lambda (class)
	 (aif (member slot (mapcar #'slot-definition-name
				   (mopp::class-direct-slots class)))
	      (intern (concat (symbol-name (class-name class)) "-"
			      (symbol-name slot))
		      (symbol-package (class-name class)))))
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
  (when (class+.slot-boundp object slot)
    (setf (gethash (slot-value object slot) (slot-index server object slot))
	  (cons object (gethash (slot-value object slot) (slot-index server object slot)))))
  object)

(defmethod delete-from-slot-index ((server database) object slot)
  (when (class+.slot-boundp object slot)
    (if (null (setf (gethash (slot-value object slot) (slot-index server object slot))
		    (cdr (gethash (slot-value object slot) (slot-index server object slot)))))
	(remhash (slot-value object slot) (slot-index server object slot))))
  object)

(defmethod regenerate-slot-indexes ((server database))
  (maphash (lambda (k v)
	     (when (and (find-class k nil)
			(member (find-class 'class+-instance)
				(class+.superclasses (find-class k))))
	       (mapcar
		(lambda (object)
		  (reduce
		   (lambda (object slot)
		     (delete-from-slot-index server object
					     (slot-definition-name slot))
		     (add-to-slot-index server object
					(slot-definition-name slot))
		     object)
		   (class+.indexes (find-class k)) :initial-value object))
		v)))
	   (database.root server)))

(defmethod find-objects-with-slot ((server database) (class class+) (slot symbol) value)
  (let ((slot (class+.find-slot class slot)))
    (if (slot-definition-index slot)
	(gethash value (slot-index server class (slot-definition-name slot)))
	(call-next-method))))

(defmethod find-object-with-slot ((server database) (class class+) (slot symbol) value)
  (car (find-objects-with-slot server class slot value)))

(defmethod find-object-with-id ((server database) id)
  (find-object-with-slot server (find-class+ 'object-with-id) 'database-id id))

(deftransaction change-class-of ((server database) (object class+-instance)
				 (class class+))
  (delete-from-class-index server object)
  (change-class object class)
  (add-to-class-index server object)
  object)

(deftransaction update-object ((server database) (object class+-instance) &rest slots-and-values)
  (reduce
   (lambda (object slot-val)	    
     (destructuring-bind (name . new-value) slot-val
       (let ((old-value (and (class+.slot-boundp object name) (slot-value object name))))
	 (let ((slot (class+.find-slot (class-of object) name)))
	   (when slot
	     (with-slotdef (name index relation) slot
	       (cond
		 ((and relation old-value)
		  (multiple-value-bind (relation-type relational-slot) (slot-definition-relation-type slot)
		    (case relation-type
		      (:n-to-one
		       (setf (slot-value old-value (slot-definition-name relational-slot))
			     (remove object
				     (slot-value old-value (slot-definition-name relational-slot)))))
		      (:one-to-n
		       (reduce (lambda (object old-value)
				 (setf (slot-value old-value (slot-definition-name relational-slot))
				       object)
				 object)
			       (ensure-list old-value) :initial-value object))
		      (:n-to-n
		       (reduce (lambda (object target)
				 (setf (slot-value target (slot-definition-name relational-slot))
				       (remove object
					       (slot-value target
							   (slot-definition-name relational-slot))))
				 object)
			       old-value :initial-value object))
		      (:one-to-one
		       (setf (slot-value old-value (slot-definition-name relational-slot))
			     nil)))))
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
				   (remove object
					   (slot-value new-value
						       (slot-definition-name relational-slot))))))
		      (:one-to-n
		       (reduce (lambda (object new-value)
				 (setf (slot-value new-value (slot-definition-name relational-slot))
				       object)
				 object)
			       (ensure-list new-value) :initial-value object))
		      (:n-to-n
		       (reduce (lambda (object target)
				 (setf (slot-value target (slot-definition-name relational-slot))
				       (cons object
					     (slot-value target
							 (slot-definition-name relational-slot))))
				 object)
			       new-value :initial-value object))
		      (:one-to-one
		       (setf (slot-value new-value (slot-definition-name relational-slot))
			     object)))))))))))
     object)
   slots-and-values :initial-value object))

(deftransaction add-object ((server database) (class class+) &rest slots-and-values)
  (let ((lifted (any (lambda (slot)
		       (eq (slot-definition-host slot) 'lift))
		     (class+.slots class))))
    (assert (null lifted) nil "This is lifted, should not go into db ~A"
	    class))
  (if (member (find-class+ 'object-with-id) (class+.superclasses class))
      (setf slots-and-values
	    (cons (cons 'database-id (next-id server))
		  (remove 'database-id slots-and-values :key #'car))))
  
  (let ((object (allocate-instance class))
	(initargs (reduce0 #'append
		   (uniq
		    (filter (lambda (a)
			      (not (member (car a) slots-and-values :key #'car :test #'string=)))
			    (mapcar (lambda (a) (list (car a) (cadr a)))
				    (class-default-initarg-values class)))
				 :key #'car))))

    (apply #'update-object server object slots-and-values)
    (apply #'initialize-instance object initargs)
    (add-to-class-index server object)
    
;; -------------------------------------------------------------------------
;; Add this object to cache so that it wont be serialized again. -evrim.
;; -------------------------------------------------------------------------
    (with-slots (cache counter) (slot-value server 'database-cache)
      (let ((counter (incf counter)))
	(setf (gethash counter cache) object)
	(setf (gethash object cache) counter)))

    object))

(deftransaction delete-object ((server database) (object class+-instance))
  ;; Remove From Class Index
  (delete-from-class-index server object)

  ;; Remove From Slot Indexes
  (reduce (lambda (object slot)
	    (delete-from-slot-index server object (slot-definition-name slot))
	    object)
	  (class+.indexes (class-of object)) :initial-value object)

  ;; Remove From Relational Objects
  (reduce (lambda (object slot)
	    (let ((old-value (and (class+.slot-boundp object (slot-definition-name slot))
				  (slot-value object (slot-definition-name slot)))))
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
			     old-value :initial-value object))
		    (:one-to-one
		     (setf (slot-value old-value (slot-definition-name relational-slot))
			   nil)))))
	      object))
	  (class+.relations (class-of object)) :initial-value object)

  ;; Process Leafs
  ;; FIXME: do typesafe version of the below ie. clos dispatch -evrim.
  (reduce (lambda (object slot)
	    (let ((value (and (class+.slot-boundp object (slot-definition-name slot))
			      (slot-value object (slot-definition-name slot)))))
	      (prog1 object
		(when (slot-definition-leaf slot)
		  (typecase value
		    (class+-instance
		     (delete-object server value))
		    (list
		     (mapcar (lambda (value)
			       (if (typep value 'class+-instance)
				   (delete-object server value)))
			     value))
		    (t
		     ;; do nothing
		     ))))))
	  (class+.slots (class-of object)) :initial-value object)
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
