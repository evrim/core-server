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
	  (update (class+.update-function class+ prefix))
	  (lambda-list1 (class+.ctor-lambda-list class+ t))
	  (lambda-list2 (class+.ctor-lambda-list class+ nil))
	  (lambda-list3 (class+.ctor-query-lambda-list class+ t)))
      (with-unique-names (server)
	(let ((server (intern (symbol-name server))))
	  `(progn
	     (redefmethod ,all ((,server abstract-database))
	       (find-all-objects ,server ',class))
	     (redefmethod ,query ((,server abstract-database) &key ,@lambda-list3)
	       (flet ((find-object (slot value)
			(find-objects-with-slot ,server ',class slot value)))
		 (let ((set (reduce (lambda (acc atom)
				      (if (car atom)
					  (cons (cdr atom) acc)
					  acc))
				    (list
				     ,@(mapcar (lambda (slot)
						 `(if ,(caddr slot)
						      (cons ,(caddr slot)
							    (find-object ',(car slot) ,(car slot)))
						      (cons nil nil)))
					       lambda-list3))
				    :initial-value nil)))
		   (nreverse
		    (reduce (lambda (set1 set2)
			      (intersection (ensure-list set2) (ensure-list set1)))
			    (cdr set)
			    :initial-value (car set))))))
	     (redefmethod ,find ((,server abstract-database) &key ,@lambda-list3)
	       (let ((args (reduce (lambda (acc slot)
				     (if (caddr slot)
					 (cons (make-keyword (car slot))
					       (cons (cadr slot) acc))
					 acc))
				   (list
				    ,@(mapcar (lambda (a)
						`(list ',(car a) ,(car a) ,(caddr a)))
					      lambda-list3))
				   :initial-value nil)))
		 (car (apply #',query ,server args))))
	     (redefmethod ,add ((,server abstract-database) &key ,@lambda-list2)
	       (add-object ,server ',class
			   ,@(mapcar (lambda (slot) `(cons ',(car slot) ,(car slot)))
				     lambda-list2)))
	     (redefmethod ,update ((,server abstract-database) (instance ,class)
				   &key ,@lambda-list3)
	       (apply #'update-object ,server instance
		      (filter (lambda (a) (not (null a)))
			      (list
			       ,@(reduce (lambda (acc slot)
					   (cons
					    `(if ,(caddr slot)
						 (cons ',(car slot) ,(car slot)))
					    acc))
					 lambda-list3 :initial-value nil)))))
	     (redefmethod ,delete ((,server abstract-database) (instance ,class))
	       (delete-object ,server instance))
	     (defmethod database.clone ((,server abstract-database) (instance ,class))
	       (,add ,server ,@(reduce0
				(lambda (acc slot)
				  (with-slotdef (initarg reader leaf) slot
				    (cons initarg
					  (cons (if leaf
						    `(database.clone ,server (,reader instance))
						    `(,reader instance))
						acc))))
				(remove 'database-id (class+.local-slots class+)
					:key #'slot-definition-name))))))))))

(defmacro defcrud/lift (target-class source-class &optional prefix)
  (let ((source+ (find-class+ source-class)))
    (let ((delete (class+.delete-function source+ prefix))
	  (update (class+.update-function source+ prefix)))
      `(progn
	 (defmethod/lift ,update ((server abstract-database) (instance ,target-class)
				  &key ,@(class+.ctor-lambda-list source+ t)))
	 (defmethod/lift ,delete ((server abstract-database) (instance ,target-class)))))))