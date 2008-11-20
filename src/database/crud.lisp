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

(defmacro defcrud (class &optional (prefix ""))
  (let ((class+ (find-class+ class))
	(all (intern (format nil "~A~A-LIST" prefix class)))
	(find (intern (format nil "~AFIND-~A" prefix class)))
	(add (intern (format nil "~A~A-ADD"  prefix class)))
	(delete (intern (format nil "~A~A-DELETE" prefix class)))
	(update (intern (format nil "~A~A-UPDATE" prefix class))))
    `(progn
       (redefmethod ,all ((server database)) (find-all-objects server ',class))
       (redefmethod ,find ((server database) &key ,@(class+.ctor-lambda-list class+ t))
	 (flet ((find-object (slot value)
		  (find-object-with-slot server ',class slot value)))
	   (let ((set (append
		       ,@(mapcar (lambda (slot)
				   `(if ,(caddr slot)
					(find-object ',(car slot) ,(car slot))))
				 (class+.ctor-lambda-list class+ t)))))
	     (reduce (lambda (set1 set2)
		       (if set2
			   (intersection set1 set2)
			   set1))
		     (cdr set)
		     :initial-value (car set)))))
       (redefmethod ,add ((server database) &key ,@(class+.ctor-lambda-list class+))
	 (add-object server ',class
		     ,@(mapcar (lambda (slot)
				 `(cons ',(car slot) ,(car slot)))
			       (class+.ctor-lambda-list class+))))
       (redefmethod ,update ((server database) (instance ,class)
			   &key ,@(class+.ctor-lambda-list class+ t))
	 (apply #'update-object server instance
		(filter (lambda (a) (not (null a)))
			(list
			 ,@(reduce (lambda (acc slot)
				     (cons
				      `(if ,(caddr slot)
					   (cons ',(car slot) ,(car slot)))
				      acc))
				   (class+.ctor-lambda-list class+ t) :initial-value nil)))))
       (redefmethod ,delete ((server database) (instance ,class))
	 (delete-object server instance)))))


;; Ibreti kendime asagidakilerin -evrim.

;; (defun crud.all-tx (class prefix)
;;   (intern (string-upcase (format nil "~A~A-LIST" (or prefix "") class))
;; 	  (symbol-package class)))

;; (defun crud.find-tx (class prefix)
;;   (intern (string-upcase (format nil "~AFIND-~A" (or prefix "") class))
;; 	  (symbol-package class)))

;; (defun crud.add-tx (class prefix)
;;   (intern (string-upcase (format nil "~A~A-ADD" (or prefix "") class))
;; 	  (symbol-package class)))

;; (defun crud.delete-tx (class prefix)
;;   (intern (string-upcase (format nil "~A~A-DELETE" (or prefix "") class))
;; 	  (symbol-package class)))

;; (defun crud.update-tx (class prefix)
;;   (intern (string-upcase (format nil "~A~A-UPDATE" (or prefix "") class))
;; 	  (symbol-package class)))

;; (defmacro defcrud (class &optional (prefix nil))
;;   (flet ((filter-id-slot (lambda-list)
;; 	     (filter (lambda (slot)
;; 		       (not (string= (car slot) 'id)))
;; 		     lambda-list)))
;;     (let* ((class+ (find-class+ class))
;; 	   (lambda-list-with-initforms (class+.ctor-lambda-list class+ t))
;; 	   (lambda-list-without-initforms (mapcar (lambda (argument)
;; 						    (list (car argument) nil (caddr argument)))
;; 						  lambda-list-with-initforms))
;; 	   (add-lambda-list (filter-id-slot lambda-list-with-initforms))
;; 	   (add-arguments (reduce0 (lambda (acc slot)
;; 				     (with-slotdef (name initarg) slot
;; 				       (if (string= name 'id)
;; 					   acc
;; 					   (cons `(list ',name ,(intern (symbol-name initarg)))
;; 						 acc))))
;; 				   (class+.local-slots class+)))
;; 	   (update-lambda-list (filter-id-slot lambda-list-with-initforms))
;; 	   (update-arguments (reduce0 (lambda (acc slot)
;; 					(with-slotdef (name supplied-p initarg) slot
;; 					  (if (string= name 'id)
;; 					      acc
;; 					      (cons `(if ,supplied-p
;; 							 (list ',name ,(intern (symbol-name initarg))))
;; 						    acc))))
;; 				      (class+.local-slots class+)))
;; 	   ;; (tx-arguments (reduce0 (lambda (acc slot)
;; ;; 				    (with-slotdef (name supplied-p initarg) slot			      
;; ;; 				      (cons `(if ,supplied-p
;; ;; 						 (list ',name ,(intern (symbol-name initarg))))
;; ;; 					    acc)))
;; ;; 				  (class+.local-slots class+)))
;; 	   (n-to-1 (class+.n-to-1-relations class+))
;; 	   (1-to-n (class+.1-to-n-relations class+))
;; 	   (n-to-n (class+.n-to-n-relations class+)))    
;;       `(progn
;; 	 ;; ----------------------------------------------------------------------------
;; 	 ;; All Instances Method (r/o)
;; 	 ;; ----------------------------------------------------------------------------
;; 	 (redefmethod ,(crud.all-tx class prefix) ((server database-server))
;; 	   (find-all-objects server ',class))

;; 	 ;; ----------------------------------------------------------------------------
;; 	 ;; Finder Method (r/o)
;; 	 ;; ----------------------------------------------------------------------------
;; 	 (redefmethod ,(crud.find-tx class prefix)
;; 	     ((server database-server) &key ,@lambda-list-without-initforms)	 
;; 	   (cond
;; 	     ,@(mapcar
;; 		(lambda (slot)
;; 		  (with-slotdef (name supplied-p initarg) slot		
;; 		    `(,supplied-p
;; 		      (find-object-with-slot server ',class ',name
;; 					     ,(intern (symbol-name initarg))))))
;; 		(class+.local-slots class+))))

;; 	 ;; ----------------------------------------------------------------------------
;; 	 ;; Add Method (r/w)
;; 	 ;; ----------------------------------------------------------------------------
;; 	 (redefmethod ,(crud.add-tx class prefix) ((server database-server) &key ,@add-lambda-list)
;; 	   (declare (ignorable ,@(mapcar #'caddr add-lambda-list)))
;; 	   (with-transaction (server)
;; 	     (let ((object (tx-create-object server ',class (list ,@add-arguments))))
;; 	       ;; Handle n-to-1 Relations: Add this object to the slot of relational object
;; 	       ,@(mapcar (lambda (slot)
;; 			   (with-slotdef (name type relation) slot
;; 			     (destructuring-bind (name initform supplied-p) (assoc name lambda-list-with-initforms)
;; 			       (declare (ignore initform))
;; 			       `(when ,supplied-p ;; supplied-p varible
;; 				  (tx-change-object-slots
;; 				   server ',type (get-id ,name)
;; 				   (list (list ',relation
;; 					       (cons object
;; 						     (slot-value ,name ',relation)))))))))
;; 			 n-to-1)
;; 	       ;; Hadnle 1-to-n Relations: Set pointers to object
;; 	       ,@(mapcar (lambda (slot)
;; 			   (with-slotdef (name relation) slot
;; 			     (destructuring-bind (name initform supplied-p) (assoc name lambda-list-with-initforms)
;; 			       (declare (ignore initform))
;; 			       `(when ,supplied-p
;; 				  (mapcar (lambda (target)
;; 					    (tx-change-object-slots
;; 					     server ',(slot-definition-singular-type slot) (get-id target)
;; 					     (list (list ',relation object))))
;; 					  ,name)))))
;; 			 1-to-n)

;; 	       ;; Handle n-to-n relations
;; 	       ,@(mapcar (lambda (slot)
;; 			   (with-slotdef (name relation) slot
;; 			     (destructuring-bind (name initform supplied-p) (assoc name lambda-list-with-initforms)
;; 			       (declare (ignore initform))
;; 			       `(when ,supplied-p
;; 				  (mapcar (lambda (target)
;; 					    (tx-change-object-slots
;; 					     server ',(slot-definition-singular-type slot) (get-id target)
;; 					     (list (list ',relation
;; 							 (cons object
;; 							       (slot-value target ',relation))))))
;; 					  ,name)))))
;; 			 n-to-n)

;; 	       object)))

;; 	 ;; ----------------------------------------------------------------------------
;; 	 ;; Delete Method (r/w)
;; 	 ;; ----------------------------------------------------------------------------
;; 	 (redefmethod ,(crud.delete-tx class prefix) ((server database-server) (,class ,class))
;; 	   (with-transaction (server)
;; 	     ;; Handle n-to-1 Relations: Remove this object from the slot of relational object
;; 	     ,@(mapcar (lambda (slot)
;; 			 (with-slotdef (name type relation) slot
;; 			   `(let ((target (slot-value ,class ',name)))
;; 			      (when target
;; 				(tx-change-object-slots
;; 				 server ',type (get-id target)
;; 				 (list (list ',relation
;; 					     (remove target (slot-value target ',relation)))))))))
;; 		       n-to-1)
;; 	     ;; Handle 1-to-n relations: Remove all related objects
;; 	     ,@(mapcar (lambda (slot)
;; 			 (with-slotdef (name relation) slot
;; 			   `(mapcar (lambda (target)
;; 				      (tx-change-object-slots
;; 				       server ',(slot-definition-singular-type slot) (get-id target)
;; 				       (list (list ',relation nil)))
;; 				      (,(crud.delete-tx (slot-definition-singular-type slot) prefix) server target))
;; 				    (slot-value ,class ',name))))
;; 		       1-to-n)
;; 	     ;; Handle n-to-n relations: Remove itself from relational list
;; 	     ,@(mapcar (lambda (slot)
;; 			 (with-slotdef (name relation) slot
;; 			   `(mapcar (lambda (target)
;; 				      (tx-change-object-slots
;; 				       server ',(slot-definition-singular-type slot) (get-id target)
;; 				       (list (list ',relation
;; 						   (remove ,class (slot-value target ',relation))))))
;; 				    (slot-value ,class ',name))))
;; 		       n-to-n)
;; 	     (tx-delete-object server ',class (get-id ,class))))

;; 	 ;; ----------------------------------------------------------------------------
;; 	 ;; Update Method (r/w)
;; 	 ;; ----------------------------------------------------------------------------
;; 	 (redefmethod ,(crud.update-tx class prefix)
;; 	     ((server database-server) (,class ,class) &key ,@update-lambda-list)
;; 	   (with-transaction (server)
;; 	     ;; Handle n-to-1 Relations:
;; 	     ,@(mapcar (lambda (slot)
;; 			 (with-slotdef (name type relation) slot
;; 			   (destructuring-bind (name initform supplied-p) (assoc name lambda-list-with-initforms)
;; 			     (declare (ignore initform))
;; 			     `(if ,supplied-p
;; 				  (let ((target (slot-value ,class ',name)))
;; 				    (when (not (eq ,name target))
;; 				      (tx-change-object-slots
;; 				       server ',type (get-id target)
;; 				       (list (list ',relation
;; 						   (cons ,name
;; 							 (remove target (slot-value target ',relation))))))))))))
;; 		       n-to-1)
;; 	     ;; Handle 1-to-n Rleations:
;; 	     ,@(mapcar (lambda (slot)
;; 			 (with-slotdef (name relation) slot
;; 			   (destructuring-bind (name initform supplied-p) (assoc name lambda-list-with-initforms)
;; 			     (declare (ignore initform))
;; 			     `(when ,supplied-p
;; 				(mapcar (lambda (target)
;; 					  (tx-change-object-slots
;; 					   server ',(slot-definition-singular-type slot) (get-id target)
;; 					   (list (list ',relation ,class))))
;; 					,name)))))
;; 		       1-to-n)

;; 	     ;; Handle n-to-n Relations:
;; 	     ,@(mapcar (lambda (slot)
;; 			 (with-slotdef (name relation) slot
;; 			   (destructuring-bind (name initform supplied-p) (assoc name lambda-list-with-initforms)
;; 			     (declare (ignore initform))
;; 			     `(when ,supplied-p
;; 				(let ((del-list (set-difference (ensure-list (slot-value ,class ',name))
;; 								(ensure-list ,name)))
;; 				      (add-list (set-difference (ensure-list ,name)
;; 								(ensure-list (intersection (slot-value ,class ',name)
;; 											   ,name)))))
;; 				  (describe del-list)
;; 				  (describe add-list)
;; 				  (mapc (lambda (target)
;; 					  (tx-change-object-slots
;; 					   server ',(slot-definition-singular-type slot) (get-id target)
;; 					   (list (list ',relation (remove ,class (slot-value target ',name))))))
;; 					del-list)
;; 				  (mapc (lambda (target)
;; 					  (tx-change-object-slots
;; 					   server ',(slot-definition-singular-type slot) (get-id target)
;; 					   (list (list ',relation (cons ,class (slot-value target ',name))))))
;; 					add-list)
;; 				  ;; (mapcar (lambda (target)
;; ;; 					    (tx-change-object-slots
;; ;; 					     server ',(slot-definition-singular-type slot) (get-id target)
;; ;; 					     (list (list ',relation
;; ;; 							 (append ,name
;; ;; 								 (set-difference (slot-value ,class ',relation)
;; ;; 										 (slot-value ,class ',name)))))))
;; ;; 					  ,name)
;; 				  )))))
;; 		       n-to-n)
	     
;; 	     (tx-change-object-slots server ',class (get-id ,class)
;; 				     (remove-if #'null (list ,@update-arguments)))))))))

;; (defclass+ user ()
;;   ((name)
;;    (password)
;;    (blogs :type blog* :relation user)))

;; (defclass+ blog ()
;;   ((user :type user :relation blogs)
;;    (title)
;;    (text)))

;; (defcrud user)
;; (defcrud blog)

;; (defmethod class+.tx-create-object-arguments ((self class+))
;;   (mapcar #'car (class+.local-slots self)))

;; (defclass+ user (object-with-id)
;;   ((username :host local :initform (error "moo"))
;;    (password :host local)
;;    (blogs :relation blog*)))

;; (defclass+ blog (object-with-id)
;;   ((user :relation user)
;;    (title)
;;    (text)))

;; (defparameter *s
;;   (make-instance 'database-server :directory #P"/tmp/eben/"))

;; (defcrud user)
;; (defcrud blog)
;; (deftransaction find-gee ((self database-server) &key (name nil) (password nil))
;;   (declare (ignore password))
;;   (find-object-with-slot self 'muser 'name name))

;; (defclass+ muser ()
;;   ((name :host local :initarg :name)))

