;; ----------------------------------------------------------------------------
;; Prevalence Isomorphism
;; ----------------------------------------------------------------------------
(in-package :core-server)

;; --------------------------------------------------------------------------
;; Lisp->XML
;; --------------------------------------------------------------------------
(defmethod xml-serialize ((object t) &optional (k #'xml-serialize))
  (declare (ignore k))
  (error "Cannot serialize ~A" object))

;; --------------------------------------------------------------------------
;; XML->Lisp
;; --------------------------------------------------------------------------
(defmethod xml-deserialize ((xml t) &optional (k #'xml-deserialize))
  (declare (ignore k))
  (error "Cannot deserialize ~A" xml))

(defxml <db:object-with-id id)


;; -------------------------------------------------------------------------
;; HTML Elements
;; -------------------------------------------------------------------------
(defmethod xml-deserialize ((xml html-element)
			    &optional (k #'xml-deserialize))
  (declare (ignore k))
  xml)

;; --------------------------------------------------------------------------
;; Null
;; --------------------------------------------------------------------------
(defxml <db:null)

(defmethod xml-deserialize ((xml <db:null) &optional (k #'xml-deserialize))
  (declare (ignore k))
  nil)

(defmethod xml-serialize ((object null) &optional (k #'xml-serialize))
  (declare (ignore k))
  (<db:null))

;; --------------------------------------------------------------------------
;; True
;; --------------------------------------------------------------------------
(defxml <db:true)

(defmethod xml-deserialize ((xml <db:true) &optional (k #'xml-deserialize))
  (declare (ignore k))
  t)

(defmethod xml-serialize ((object (eql 't)) &optional (k #'xml-serialize))
  (declare (ignore k))
  (<db:true))

;; ----------------------------------------------------------------------------
;; Symbol
;; ----------------------------------------------------------------------------
(defxml <db:symbol)

(defmethod xml-deserialize ((xml <db:symbol) &optional (k #'xml-deserialize))
  (declare (ignore k))
  (read-from-string (car (slot-value xml 'children))))

(defmethod xml-serialize ((object symbol) &optional (k #'xml-serialize))
  (declare (ignore k))
  (<db:symbol (symbol->string object)))

;; ----------------------------------------------------------------------------
;; Character
;; ----------------------------------------------------------------------------
(defxml <db:character)

(defmethod xml-deserialize ((xml <db:character) &optional (k #'xml-deserialize))
  (declare (ignore k))
  (aref (read-from-string (car (slot-value xml 'children))) 0))

(defmethod xml-serialize ((object character) &optional (k #'xml-serialize))
  (declare (ignore k))
  (<db:character (format nil "\"~C\"" object)))

;; ----------------------------------------------------------------------------
;; Integer
;; ----------------------------------------------------------------------------
(defxml <db:integer)

(defmethod xml-deserialize ((xml <db:integer) &optional (k #'xml-deserialize))
  (declare (ignore k))
  (read-from-string (car (slot-value xml 'children))))

(defmethod xml-serialize ((object integer) &optional (k #'xml-serialize))
  (declare (ignore k))
  (<db:integer (format nil "~D" object)))

;; ----------------------------------------------------------------------------
;; Ratio
;; ----------------------------------------------------------------------------
(defxml <db:ratio)

(defmethod xml-deserialize ((xml <db:ratio) &optional (k #'xml-deserialize))
  (declare (ignore k))
  (read-from-string (car (slot-value xml 'children))))

(defmethod xml-serialize ((object ratio) &optional (k #'xml-serialize))
  (declare (ignore k))
  (<db:ratio (format nil "~A" object)))

;; ----------------------------------------------------------------------------
;; Complex
;; ----------------------------------------------------------------------------
(defxml <db:complex)

(defmethod xml-deserialize ((xml <db:complex) &optional (k #'xml-deserialize))
  (declare (ignore k))
  (read-from-string (car (slot-value xml 'children))))

(defmethod xml-serialize ((object complex) &optional (k #'xml-serialize))
  (declare (ignore k))
  (<db:complex (format nil "~A" object)))

;; ----------------------------------------------------------------------------
;; Float
;; ----------------------------------------------------------------------------
(defxml <db:float)

(defmethod xml-deserialize ((xml <db:float) &optional (k #'xml-deserialize))
  (declare (ignore k))
  (read-from-string (car (slot-value xml 'children))))

(defmethod xml-serialize ((object float) &optional (k #'xml-serialize))
  (declare (ignore k))
  (<db:float (format nil "~A" object)))

;; ----------------------------------------------------------------------------
;; String
;; ----------------------------------------------------------------------------
(defxml <db:string)

(defmethod xml-deserialize ((xml <db:string) &optional (k #'xml-deserialize))
  (declare (ignore k))
  (read-from-string (car (slot-value xml 'children))))

(defmethod xml-serialize ((object string) &optional (k #'xml-serialize))
  (declare (ignore k))
  (<db:string (format nil "~S" object)))

;; ----------------------------------------------------------------------------
;; Vector
;; ----------------------------------------------------------------------------
(defxml <db:vector length id)

(defmethod xml-deserialize ((xml <db:vector) &optional (k #'xml-deserialize))
  (declare (ignore k))
  (with-slots (length) xml
    (let ((object (read-from-string (car (slot-value xml 'children)))))
      (assert (= length (length object)))
      object)))

(defmethod xml-serialize ((object vector) &optional (k #'xml-serialize))
  (declare (ignore k))
  (<db:vector :length (length object)
	      (format nil "~A" object)))

;; ----------------------------------------------------------------------------
;; Cons
;; ----------------------------------------------------------------------------
(defxml <db:cons consp length id)

(defmethod xml-deserialize ((xml <db:cons) &optional (k #'xml-deserialize))
  (with-slots (consp length) xml
    (let ((object (apply (if consp #'cons #'list)
			 (mapcar (rcurry k k) (slot-value xml 'children)))))
      (if (null consp) (assert (= (parse-integer length) (length object))))
      object)))

(defmethod xml-serialize ((object list) &optional (k #'xml-serialize))
  (if (atom (cdr object))
      (<db:cons :consp t
		(funcall k (car object) k)
		(funcall k (cdr object) k))
      (<db:cons :length (length object)
		(mapcar (rcurry k k) object))))

;; ----------------------------------------------------------------------------
;; Hash Table
;; ----------------------------------------------------------------------------
(defxml <db:hash-table-key)
(defmethod xml-deserialize ((xml <db:hash-table-key) &optional (k #'xml-deserialize))
  (funcall k (car (slot-value xml 'children)) k))

(defxml <db:hash-table-value)
(defmethod xml-deserialize ((xml <db:hash-table-value) &optional (k #'xml-deserialize))
  (funcall k (car (slot-value xml 'children)) k))

(defxml <db:hash-table-entry)
(defmethod xml-deserialize ((xml <db:hash-table-entry) &optional (k #'xml-deserialize))
  (with-slots (children) xml
    (values (funcall k (car children) k) (funcall k (cadr children) k))))

(defxml <db:hash-table test size id)
(defmethod xml-deserialize ((xml <db:hash-table) &optional (k #'xml-deserialize))
  (with-slots (test size) xml
    (let ((table (make-hash-table :test (read-from-string test))))
      (reduce (lambda (table entry)
		(multiple-value-bind (key value) (funcall k entry k)
		  (setf (gethash key table) value)
		  table))
	      (slot-value xml 'children)
	      :initial-value table))))

(defmethod xml-serialize ((object hash-table) &optional (k #'xml-serialize))
  (<db:hash-table :test (symbol->string (hash-table-test object))
		  :size (hash-table-size object)
		  (let ((result))
		    (maphash (lambda (key value)
			       (push (<db:hash-table-entry
				      (<db:hash-table-key (funcall k key k))
				      (<db:hash-table-value (funcall k value k)))
				     result))
			     object)
		    (nreverse result))))

;; ----------------------------------------------------------------------------
;; Object/Structure Slot
;; ----------------------------------------------------------------------------
(defxml <db:slot name)
(defmethod xml-deserialize ((xml <db:slot) &optional (k #'xml-deserialize))
  (values
   (read-from-string (slot-value xml 'name))
   (funcall k (car (slot-value xml 'children)) k)))

;; ----------------------------------------------------------------------------
;; Structure Object
;; ----------------------------------------------------------------------------
(defxml <db:struct class id)
(defmethod xml-deserialize ((xml <db:struct) &optional (k #'xml-deserialize))
  (with-slots (class) xml
    (let* ((class (read-from-string class)))
      (reduce (lambda (object slot)
		(multiple-value-bind (name value) (funcall k slot k)
		  (setf (slot-value object name) value)
		  object))
	      (slot-value xml 'children)
	      :initial-value (funcall (intern (format nil "MAKE-~A" (symbol-name class))
					      (symbol-package class)))))))

(defmethod xml-serialize ((object structure-object) &optional (k #'xml-serialize))
  (<db:struct :class (symbol->string (class-name (class-of object)))
	      (mapcar (lambda (slot)
			(<db:slot :name (symbol->string slot)
				  (funcall k (slot-value object slot) k)))
		      (slots-of object))))

;; ----------------------------------------------------------------------------
;; Class
;; ----------------------------------------------------------------------------
(defxml <db:class id)

(defmethod xml-deserialize ((xml <db:class) &optional (k #'xml-deserialize))
  (declare (ignore k))
  (find-class (read-from-string (car (slot-value xml 'children)))))

(defmethod xml-serialize ((object standard-class) &optional (k #'xml-serialize))
  (declare (ignore k))
  (<db:class (symbol->string (class-name object))))

;; ----------------------------------------------------------------------------
;; Standard Object
;; ----------------------------------------------------------------------------
(defxml <db:instance id class)

(defmethod xml-deserialize ((xml <db:instance) &optional (k #'xml-deserialize))
  (with-slots (class children) xml
    (let ((instance (allocate-instance (find-class (read-from-string class)))))
      (initialize-instance
       (reduce (lambda (instance slot)
		 (multiple-value-bind (name value) (funcall k slot k)
		   (setf (slot-value instance name) value)
		   instance))
	       children :initial-value instance)))))

(defmethod xml-serialize ((object standard-object) &optional (k #'xml-serialize))
  (<db:instance :class (symbol->string (class-name (class-of object)))
		(mapcar (lambda (slot)
			  (<db:slot :name (symbol->string slot)
				    (funcall k (slot-value object slot) k)))
			(slots-of object))))

;; +-------------------------------------------------------------------------
;; | Pathname
;; +-------------------------------------------------------------------------
(defxml <db:pathname id name type)

(defmethod xml-deserialize ((xml <db:pathname) &optional (k #'xml-deserialize))
  (with-slots (children name type) xml
    (make-pathname :directory (mapcar (lambda (c) (funcall k c k)) children)
		   :name name
		   :type type)))

(defmethod xml-serialize ((object pathname) &optional (k #'xml-serialize))
  (<db:pathname :name (pathname-name object)
		:type (pathname-type object)
		(mapcar (lambda (directory) (funcall k directory k))
			(pathname-directory object))))

;; TODO: serialize hashtable rehash size -evrim.

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

