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
(defun crud.all-tx (class prefix)
  (intern (string-upcase (format nil "~A~AS" (or prefix "") class))
	  (symbol-package class)))

(defun crud.find-tx (class prefix)
  (intern (string-upcase (format nil "~AFIND-~A" (or prefix "") class))
	  (symbol-package class)))

(defun crud.add-tx (class prefix)
  (intern (string-upcase (format nil "~A~A-ADD" (or prefix "") class))
	  (symbol-package class)))

(defun crud.delete-tx (class prefix)
  (intern (string-upcase (format nil "~A~A-DELETE" (or prefix "") class))
	  (symbol-package class)))

(defun crud.update-tx (class prefix)
  (intern (string-upcase (format nil "~A~A-UPDATE" (or prefix "") class))
	  (symbol-package class)))

(defmacro redefmethod (name args &body body)
  "Macro that makes method unbound before defining it."
  `(progn
     (fmakunbound ',name)
     (defmethod ,name ,args ,@body)))

(defmethod class+.relations ((class class+))
  (filter (lambda (slot)
	    (not (null (slot-definition-relation slot))))
	  (class+.slots class)))

(defmethod class+.1-to-n-relations ((class class+))
  (filter (lambda (slot)
	    (with-slotdef (type) slot
	      (and (symbolp type)
		   (ends-with (symbol-name type) "*"))))
	  (class+.relations class)))

(defmethod class+.n-to-1-relations ((class class+))
  (filter (lambda (slot)
	    (with-slotdef (type) slot
	      (and (symbolp type)
		   (not (ends-with (symbol-name type) "*")))))
	  (class+.relations class)))

(defmethod class+.indexes ((class class+))
  (filter (lambda (slot)
	    (with-slotdef (index) slot
	      index))
	  (class+.slots class)))

(defmethod slot-definition-singular-type ((slot class+-slot-definition))
  (with-slotdef (type) slot
    (intern (subseq (symbol-name type) 0 (1- (length (symbol-name type))))
	    (symbol-package type))))

(defmacro defcrud (class &optional (prefix nil))
  (let* ((class+ (find-class+ class))
	 (lambda-list-with-initforms (class+.ctor-lambda-list class+ t))
	 (lambda-list-without-initforms (mapcar (lambda (argument)
						  (list (car argument) nil (caddr argument)))
						lambda-list-with-initforms))
	 (tx-arguments (reduce0 (lambda (acc slot)
				  (with-slotdef (name supplied-p initarg) slot
				    (cons `(if ,supplied-p
					       (list ',name ,(intern (symbol-name initarg))))
					  acc)))
				(class+.slots class+)))
	 (n-to-1 (class+.n-to-1-relations class+))
	 (1-to-n (class+.1-to-n-relations class+)))    
    `(progn
       ;; ----------------------------------------------------------------------------
       ;; All Instances Method (r/o)
       ;; ----------------------------------------------------------------------------
       (redefmethod ,(crud.all-tx class prefix) ((server database-server))
	 (find-all-objects server ',class))

       ;; ----------------------------------------------------------------------------
       ;; Finder Method (r/o)
       ;; ----------------------------------------------------------------------------
       (redefmethod ,(crud.find-tx class prefix)
	   ((server database-server) &key ,@lambda-list-without-initforms)	 
	 (cond
	   ,@(mapcar
	      (lambda (slot)
		(with-slotdef (name supplied-p initarg) slot		
		  `(,supplied-p
		    (find-object-with-slot server ',class ',name
					   ,(intern (symbol-name initarg))))))
	      (class+.local-slots class+))))

       ;; ----------------------------------------------------------------------------
       ;; Add Method (r/w)
       ;; ----------------------------------------------------------------------------
       (redefmethod ,(crud.add-tx class prefix) ((server database-server) &key ,@lambda-list-with-initforms)
	 (with-transaction (server)
	   (let ((object (tx-create-object server ',class (remove-if #'null (list ,@tx-arguments)))))
	     ;; Handle n-to-1 Relations: Add this object to the slot of relational object
	     ,@(mapcar (lambda (slot)
			 (with-slotdef (name type relation) slot
			   (destructuring-bind (name initform supplied-p) (assoc name lambda-list-with-initforms)
			     (declare (ignore initform))
			     `(when ,supplied-p ;; supplied-p varible
				(tx-change-object-slots
				 server ',type (get-id ,name)
				 (list (list ',relation
					     (cons object
						   (slot-value ,name ',relation)))))))))
		       n-to-1)
	     ;; Hadnle 1-to-n Relations: Set pointers to object
	     ,@(mapcar (lambda (slot)
			 (with-slotdef (name relation) slot
			   (destructuring-bind (name initform supplied-p) (assoc name lambda-list-with-initforms)
			     (declare (ignore initform))
			     `(when ,supplied-p
				(mapcar (lambda (target)
					  (tx-change-object-slots
					   server ',(slot-definition-singular-type slot) (get-id target)
					   (list (list ',relation object))))
					,name)))))
		       1-to-n)
	     object)))

       ;; ----------------------------------------------------------------------------
       ;; Delete Method (r/w)
       ;; ----------------------------------------------------------------------------
       (redefmethod ,(crud.delete-tx class prefix) ((server database-server) (,class ,class))
	 (with-transaction (server)
	   ;; Handle n-to-1 Relations: Remove this object from the slot of relational object
	   ,@(mapcar (lambda (slot)
		       (with-slotdef (name type relation) slot
			 `(let ((target (slot-value ,class ',name)))
			    (when target
			      (tx-change-object-slots
			       server ',type (get-id target)
			       (list (list ',relation
					   (remove target (slot-value target ',relation)))))))))
		     n-to-1)
	   ;; Handle 1-to-n relations: Remove all related objects
	   ,@(mapcar (lambda (slot)
		       (with-slotdef (name relation) slot
			 `(mapcar (lambda (target)
				    (tx-change-object-slots
				     server ',(slot-definition-singular-type slot) (get-id target)
				     (list (list ',relation nil)))
				    (,(crud.delete-tx (slot-definition-singular-type slot) prefix) server target))
				  (slot-value ,class ',name))))
		     1-to-n)
	   (tx-delete-object server ',class (get-id ,class))))

       ;; ----------------------------------------------------------------------------
       ;; Update Method (r/w)
       ;; ----------------------------------------------------------------------------
       (redefmethod ,(crud.update-tx class prefix) ((server database-server) (,class ,class)
						    &key ,@lambda-list-without-initforms)
	 (with-transaction (server)
	   ;; Handle n-to-1 Relations:
	   ,@(mapcar (lambda (slot)
		       (with-slotdef (name type relation) slot
			 (destructuring-bind (name initform supplied-p) (assoc name lambda-list-with-initforms)
			   (declare (ignore initform))
			   `(if ,supplied-p
				(let ((target (slot-value ,class ',name)))
				  (when (not (eq ,name target))
				    (tx-change-object-slots
				     server ',type (get-id target)
				     (list (list ',relation
						 (cons ,name
						       (remove target (slot-value target ',relation))))))))))))
		     n-to-1)
	   ;; Handle 1-to-n Rleations:
	   ,@(mapcar (lambda (slot)
		       (with-slotdef (name relation) slot
			 (destructuring-bind (name initform supplied-p) (assoc name lambda-list-with-initforms)
			   (declare (ignore initform))
			   `(when ,supplied-p
			      (mapcar (lambda (target)
					(tx-change-object-slots
					 server ',(slot-definition-singular-type slot) (get-id target)
					 (list (list ',relation ,class))))
				      ,name)))))
		     1-to-n)
	   
	   (tx-change-object-slots server ',class (get-id ,class)
				   (remove-if #'null (list ,@tx-arguments))))))))

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

