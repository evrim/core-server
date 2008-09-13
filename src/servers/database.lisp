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

(in-package :tr.gen.core.server)

;;+----------------------------------------------------------------------------
;;| Cl-Prevalence Database Server
;;+----------------------------------------------------------------------------
;;
;; This file contains base in-memory database server.
;;
(deftransaction set-creation-date ((system database-server) date)
  "Transaction function that is used to set creation date of the model."
  (setf (get-root-object system :creation-date) date))

(defmethod snapshot :after ((self database-server))
  (cl-prevalence::open-transaction-log-stream self))

(defmethod initialize-instance :after ((system prevalence-system) &rest initargs &key &allow-other-keys)
  "After a system is initialized, derive its file paths and try to restore it"
  (declare (ignore initargs))
  (when (not (typep system 'database-server))
    (format t "restroing")
    (restore system)))

(defmethod database-server.status ((self database-server))
  (and (slot-value self 'cl-prevalence::prevalence.transaction-log-stream) t))

(defmethod status ((self database-server))
  (database-server.status self))

(defmethod start ((self database-server))
  "Starts database server"
  (restore self)
  (cl-prevalence::open-transaction-log-stream self)

  ;; Create ID counter
  (unless (get-root-object self :id-counter)
    (create-id-counter self))

  ;; Set creation date
  (unless (get-root-object self :creation-date)
    (set-creation-date self (get-universal-time))))

(defmethod stop ((self database-server))
  "Stops database server"
  (when (database-server.status self)
    (cl-prevalence::close-open-streams self)
    (setf (cl-prevalence::get-root-objects self) (make-hash-table :test 'eq))
    (setf (slot-value self 'cl-prevalence::prevalence.serialization-state)
	  (cl-prevalence::make-serialization-state))))

(defun update-slots (instance lst)
  (let ((clazz (class-of instance))
	(key nil))
    (mapc #'(lambda (item)
	      (cond
	       ((and (null key) (keywordp item))
		(setf key item))
	       ((not (null key))
		(let ((slot-name (intern (symbol-name key) (symbol-package (class-name clazz)))))
		  (if (sb-pcl::find-slot-definition clazz slot-name)
		      (setf (slot-value instance slot-name) item
			    key nil)
		    (error "slot ~A not found in class ~A" slot-name (class-name clazz)))))
	       (t (error "Malformed update-slots slot-vals list"))))
	  lst)
    instance))

(defun filter-object (an-atom &optional (key #'get-id))
  (etypecase an-atom
    (object-with-id
     (funcall key an-atom))
    (standard-object
     (cerror "Continue?" "Unknown object found when entering transaction: ~A " an-atom)
     (funcall key an-atom))
    (t an-atom)))

(defun filter-objects (lst &optional (key #'get-id))
  (if (atom lst)
      (filter-object lst key)
      (nreverse
       (reduce #'(lambda (acc item)
                   (cons (filter-object item key) acc))
               lst :initial-value nil))))

;; ;;; Convenient Macros
;; (defun find-free-variables (form)
;;   (let (lst)
;;     (mapc (lambda (vars)
;; 	    (pushnew (name vars) lst))
;; 	  (ast-search-type form 'free-variable-reference))
;;     lst))

;; (defmacro with-transaction ((server) &body body)
;;   (with-unique-names (tx)
;;     (let* ((form (walk-form `(progn ,@body)))
;; 	   (vars (filter (lambda (a)
;; 			   (not (eq server a)))
;; 			 (find-free-variables form)))
;; 	   (thunk (walk-form `(lambda (,server ,@vars)
;; 				,@body))))  
;;       `(let ((,tx (transaction ,thunk (list ,@vars))))
;; 	 (execute ,server ,tx)))))

;; ;; (defmacro deftransaction (name args &body body)
;; ;;   (let ((arg-names (extract-argument-names args :allow-specializers t))
;; ;; 	(tx (intern (format nil "TX-~A" name))))
;; ;;     `(progn
;; ;;        (defun ,tx ,arg-names ,@body)
;; ;;        (defmethod ,name ,args
;; ;; 	 (execute ,(car arg-names)
;; ;; 		  (transaction ',tx (list ,@(cdr arg-names))))))))

;; ;; (defmacro defcrud (class)
;; ;;   (let ((class+ (find-class+ class)))
;; ;;     `(progn
;; ;;        (defmethod find-user ((self database-server) &key ,@(class+.ctor-keywords class+))
;; ;; 	 ))))

;; ;; (deftransaction find-gee ((self database-server) &key (name nil) (password nil))
;; ;;   (declare (ignore password))
;; ;;   (find-object-with-slot self 'muser 'name name))

;; ;; (defclass+ muser ()
;; ;;   ((name :host local :initarg :name)))
