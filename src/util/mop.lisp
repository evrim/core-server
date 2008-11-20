; Core Server: Web Application Server

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
;;| Utilities for meta-object-protocol (mop)
;;+----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; SysV Method Combination
;;-----------------------------------------------------------------------------
;;
;; SysV method combination is used to define a server protocol for several
;; methods like start,stop,status.
;;
;; start and stop methods of an object are runned inside a 'progn'
;; status method is runned inside an 'and'
;;
(define-method-combination sysv-standard (&key (type :start))
  ((around (:around)) (primary () :required t))  
  (labels ((specializer (method)
	     (car (sb-mop:method-specializers method)))
	   (specializers (methods)
	     (mapcar #'(lambda (m) (cons (specializer m) m)) methods))
	   (sort-by-specializers (methods)
	     (mapcar #'cdr (sort (specializers methods) #'equal :key #'car)))
	   (status-method (method)
	     (find-method #'status '() (list (specializer method)) nil))
	   (call-methods (methods)
	     (mapcar #'(lambda (method)
			 (aif (status-method method)
			      (cond
				((eq type :start)
				 `(if (not (call-method ,it))
				      (call-method ,method)
				      t))
				((eq type :stop)
				 `(if (call-method ,it)
				      (call-method ,method)
				      t))
				((eq type :status)
				 `(call-method ,method))
				((or (eq type :register)
				     (eq type :unregister))
				 `(call-method ,method)))
			      `(call-method ,method)))
		     methods)))
    (let* ((around (nreverse around))
	   (primary (sort-by-specializers primary))
	   (primary (cond
		      ((eq type :start)
		       `(apply #'values (nreverse (list ,@(call-methods (nreverse primary))))))
		      ((eq type :stop)
		       `(values ,@(call-methods primary)))
		      ((eq type :status)
		       `(and ,@(call-methods (nreverse primary))))
		      ((eq type :register)
		       `(apply #'values (nreverse (list ,@(call-methods (nreverse primary))))))
		      ((eq type :unregister)
		       `(values ,@(call-methods primary))))))
      (if around
	  `(call-method ,(first around)
			(,@(rest around)
			   (make-method ,primary)))
	  primary))))

(deftrace sysv '(start stop status))

;;-----------------------------------------------------------------------------
;; Other MOP Utilities
;;-----------------------------------------------------------------------------
(defun class-direct-superclasses (class)
  "Returns direct superclasses of the 'class'"
  (let ((class (if (symbolp class) (find-class class) class)))
    (sb-mop:class-direct-superclasses class)))

;; INSTALL> (class-superclasses 'c)
;; (#<STANDARD-CLASS C> #<STANDARD-CLASS B> #<STANDARD-CLASS A>
;;  #<STANDARD-CLASS COMMAND>)
(defun class-superclasses (class &aux lst)
  "Returns all superclasses of the given 'class'"
  (let ((class (if (symbolp class) (find-class class) class)))
    (core-search (list class)
		 (lambda (atom) (pushnew atom lst) nil) 
		 #'class-direct-superclasses
		 #'append)
    (nreverse lst)))

(defun class-direct-subclasses (class)
  (let ((class (if (symbolp class) (find-class class) class)))
    (sb-mop::class-direct-subclasses class)))

(defun class-subclasses (class &aux lst) 
  (let ((class (if (symbolp class) (find-class class) class)))
    (core-search (list class)
		 (lambda (class) (pushnew class lst) nil)
		 #'class-direct-subclasses
		 #'append)
    (cdr (nreverse lst))))

;; INSTALL> (class-default-initargs 'c)
;; ((:ARG-B 'ARG-B-OVERRIDEN-BY-C #<FUNCTION {BC06125}>)
;;  (:ARG-A 'ARG-A-OVERRIDEN-BY-C #<FUNCTION {BC06195}>))
(defun class-default-initargs (class &aux lst)
  "Returns default-initargs of the given 'class'"
  (let ((class (if (symbolp class) (find-class class) class)))
    (core-search (list class)
		 #'(lambda (atom)
		     (let ((args (copy-list
				  (sb-mop:class-direct-default-initargs atom))))
		       (when args (setf lst (append args lst))))
		     nil)
		 #'class-direct-superclasses
		 #'append))
  (nreverse lst))

(defmethod slots-of ((object standard-object))
  #+openmcl
  (mapcar #'ccl:slot-definition-name
	  (#-openmcl-native-threads ccl:class-instance-slots
	   #+openmcl-native-threads ccl:class-slots
	   (class-of object)))
  #+cmu
  (mapcar #'pcl:slot-definition-name (pcl:class-slots (class-of object)))
  #+sbcl
  (mapcar #'sb-pcl:slot-definition-name (sb-pcl:class-slots (class-of object)))
  #+lispworks
  (mapcar #'hcl:slot-definition-name (hcl:class-slots (class-of object)))
  #+allegro
  (mapcar #'mop:slot-definition-name (mop:class-slots (class-of object)))
  #+sbcl
  (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots (class-of object)))
  #-(or openmcl cmu lispworks allegro sbcl)
  (error "not yet implemented"))

(defmethod slots-of ((object structure-object))
  #+openmcl
  (let* ((sd (gethash (class-name (class-of object)) ccl::%defstructs%))
	 (slots (if sd (ccl::sd-slots sd))))
    (mapcar #'car (if (symbolp (caar slots)) slots (cdr slots))))
  #+cmu
  (mapcar #'pcl:slot-definition-name (pcl:class-slots (class-of object)))
  #+sbcl
  (mapcar #'sb-pcl:slot-definition-name (sb-pcl:class-slots (class-of object)))
  #+lispworks
  (structure:structure-class-slot-names (class-of object))
  #+allegro
  (mapcar #'mop:slot-definition-name (mop:class-slots (class-of object)))
  #+sbcl
  (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots (class-of object)))
  #-(or openmcl cmu lispworks allegro sbcl)
  (error "not yet implemented"))

(defmacro redefmethod (name args &body body)
  "Macro that makes method unbound before defining it."
  `(progn
     (fmakunbound ',name)
     (defmethod ,name ,args ,@body)))
