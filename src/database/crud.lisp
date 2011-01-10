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

;; +----------------------------------------------------------------------------
;; | CRUD Boilerplate
;; +----------------------------------------------------------------------------

(defmacro redeftransaction (name args &body body)
  `(progn
     (fmakunbound ',name)
     (deftransaction ,name ,args ,@body)))

(defmacro defcrud (class &optional prefix)
  (let ((class+ (find-class+ class)))
    (let ((all (class+.list-function class+ prefix))
	  (find (class+.find-function class+ prefix))
	  (query (class+.query-function class+ prefix))
	  (add (class+.add-function class+ prefix))
	  (delete (class+.delete-function class+ prefix))
	  (update (class+.update-function class+ prefix)))
      (with-unique-names (server)
	(let ((server (intern (symbol-name server))))
	  `(progn
	     (redefmethod ,all ((,server database)) (find-all-objects ,server ',class))
	     (redefmethod ,query ((,server database) &key ,@(class+.ctor-lambda-list class+ t))
	       (flet ((find-object (slot value)
			(core-server::find-objects-with-slot ,server ',class slot value)))
		 (let ((set (filter (compose #'not #'null)
				    (list
				     ,@(mapcar (lambda (slot)
						 `(if ,(caddr slot)
						      (find-object ',(car slot) ,(car slot))))
					       (class+.ctor-lambda-list class+ t))))))
		   (nreverse
		    (reduce (lambda (set1 set2)
			      (intersection (ensure-list set2) (ensure-list set1)))
			    (cdr set)
			    :initial-value (car set))))))
	     (redefmethod ,find ((,server database) &key ,@(class+.ctor-lambda-list class+ t))
	       (flet ((find-object (slot value)
			(find-object-with-slot ,server ',class slot value)))
		 (or
		  ,@(mapcar (lambda (slot)
			      `(if ,(caddr slot)
				   (find-object ',(car slot) ,(car slot))))
			    (class+.ctor-lambda-list class+ t)))))
	     (redefmethod ,add ((,server database) &key ,@(class+.ctor-lambda-list class+))
	       (add-object ,server ',class
			   ,@(mapcar (lambda (slot)
				       `(cons ',(car slot) ,(car slot)))
				     (class+.ctor-lambda-list class+))))
	     (redefmethod ,update ((,server database) (instance ,class)
				   &key ,@(class+.ctor-lambda-list class+ t))
	       (apply #'update-object ,server instance
		      (filter (lambda (a) (not (null a)))
			      (list
			       ,@(reduce (lambda (acc slot)
					   (cons
					    `(if ,(caddr slot)
						 (cons ',(car slot) ,(car slot)))
					    acc))
					 (class+.ctor-lambda-list class+ t) :initial-value nil)))))
	     (redefmethod ,delete ((,server database) (instance ,class))
	       (delete-object ,server instance))
	     (defmethod database.clone ((,server database) (instance ,class))
	       (,add ,server ,@(reduce0
				(lambda (acc slot)
				  (with-slotdef (initarg reader) slot
				    (cons initarg
					  (cons `(database.clone ,server (,reader instance)) acc))))
				(remove 'id (class+.local-slots class+)
					:key #'slot-definition-name))))))))))

