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
	   (call-methods (methods)
	     (mapcar #'(lambda (method) `(call-method ,method))  methods)))
    (let ((around (nreverse around)))
      (cond ((eq type :start)
	     (setf primary (cons 'progn
				 (call-methods (nreverse (sort-by-specializers primary))))))
	    ((eq type :stop)
	     (setf primary (cons 'progn 
				 (call-methods (sort-by-specializers primary)))))
	    ((eq type :status)
	     (setf primary (cons 'and 
				 (call-methods (nreverse (sort-by-specializers primary)))))))
      (if around
	  `(call-method ,(first around)
			(,@(rest around)
			   (make-method ,primary)))
	  primary))))

(deftrace sysv '(start stop status))

;;-----------------------------------------------------------------------------
;; Other MOP Utilities
;;-----------------------------------------------------------------------------
(defun class-direct-superclasses (class &optional (base 'component))
  "Returns direct superclasses of the 'class'"
  (let ((class (if (symbolp class) (find-class class) class)))
    (if (eq class (find-class base nil))
	nil
	(sb-mop:class-direct-superclasses class))))

;; INSTALL> (class-superclasses 'c)
;; (#<STANDARD-CLASS C> #<STANDARD-CLASS B> #<STANDARD-CLASS A>
;;  #<STANDARD-CLASS COMMAND>)
(defun class-superclasses (class &optional (base 'component) &aux lst)
  "Returns all superclasses of the given 'class'"
  (let ((class (if (symbolp class) (find-class class) class)))
    (core-search (cons class (copy-list (sb-mop:class-direct-superclasses class)))
		 #'(lambda (atom) (pushnew atom lst) nil) 
		 #'(lambda (class) (class-directory-superclasses class base))
		 #'append)
    (nreverse lst)))

;; INSTALL> (class-default-initargs 'c)
;; ((:ARG-B 'ARG-B-OVERRIDEN-BY-C #<FUNCTION {BC06125}>)
;;  (:ARG-A 'ARG-A-OVERRIDEN-BY-C #<FUNCTION {BC06195}>))
(defun class-default-initargs (class &optional (base 'component) &aux lst)
  "Returns default-initargs of the given 'class'"
  (core-search (cons (find-class class)
		     (copy-list
		      (sb-mop:class-direct-superclasses (find-class class))))
	       #'(lambda (atom)
		   (let ((args (copy-list
				(sb-mop:class-direct-default-initargs atom))))
		     (when args (setf lst (append args lst))))
		   nil)
	       #'(lambda (class) (class-direct-superclasses class base))
	       #'append)
  lst)
