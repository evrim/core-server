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

;;+-----------------------------------------------------------------------------
;;| Javascript AST Transformers
;;+-----------------------------------------------------------------------------
;;
;; This file contains transformations that makes javascript more lispy.
;;

;;-----------------------------------------------------------------------------
;; Javascript Functionalization
;;-----------------------------------------------------------------------------
;; This is a preprocessor to solve the return problem of javascript ie:
;;
;; (print ((lambda (a) a) 1))
;;
;; should print 1, but javascript is broken, so we add returns implicitly.
;;
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defgeneric find-form-leaf (form)
    (:documentation "Returns list of termination subforms the 'form'"))

  (defmethod find-form-leaf ((form t))
    form)

  (defmethod find-form-leaf ((form implicit-progn-mixin))
    (find-form-leaf (last1 (body form))))

  (defmethod find-form-leaf ((form if-form))
    (append (ensure-list (find-form-leaf (then form)))
	    (ensure-list (find-form-leaf (else form)))))

  (defun fix-javascript-returns (form)
    "Alters form destructive and adds implicit return statements to 'form'"
    (flet ((transform-to-implicit-return (form)
	     (let ((new-form (walk-form-no-expand (unwalk-form form))))
	       (unless (and (typep form 'application-form)
			    (eq 'return (operator form)))
		 (change-class form 'application-form)
		 (setf (operator form) 'return)
		 (setf (arguments form) (list new-form)))
	       new-form)))
      (let ((funs (ast-search-type form 'lambda-function-form)))
	(mapcar #'transform-to-implicit-return
		(flatten (mapcar #'find-form-leaf funs)))
	form)))

  (defun fix-javascript-methods (form self)
    (let ((applications (ast-search-type form 'application-form)))
      (mapcar (lambda (app)
		(when (and (typep (car (arguments app)) 'variable-reference)
			   (eq (name (car (arguments app))) self))
		  (setf (operator app) (intern (format nil "THIS.~A" (operator app)))
			(arguments app) (cdr (arguments app)))))
	      applications)
      form))

;; TODO: Handle (setf a b c d)
  (defun fix-javascript-accessors (form accessors &optional (get-prefix "GET-") (set-prefix "SET-"))
    (let ((applications (ast-search-type form 'application-form)))
      (mapcar (lambda (app)
		(cond				
		  ((and (eq 'setf (operator app))
			(typep (car (arguments app)) 'application-form)
			(member (operator (car (arguments app))) accessors))
		   (describe app)
		   (setf (operator app) (intern (format nil "~A~A" set-prefix (operator (car (arguments app)))))
			 (arguments app) (cons (car (arguments (car (arguments app))))
					       (cdr (arguments app)))))
		  ((and (member (operator app) accessors)
			(not (and (typep (parent app) 'application-form)
				  (eq 'setf (operator (parent app))))))
		   (setf (operator app) (intern (format nil "~A~A" get-prefix (operator app)))))))
	      applications)
      form)))


;; ;;-----------------------------------------------------------------------------
;; ;; External Variable For Fast Rendering
;; ;;-----------------------------------------------------------------------------
;; (eval-when (:load-toplevel :compile-toplevel :execute)
;;   (defclass external-variable-reference (variable-reference)
;;     ())
 
;;   (defjavascript-expander external-variable-reference (name)
;;     `(:json! ,name))

;; ;; FIxme: put something meaningful here. -evrim.
;; ;; This transformer overrides free-variable-references. Arguments of defrender/js
;; ;; are transformed into exnternal-variable-references to that they can
;; ;; the client. We use json serialized to be fast.
;;   (defun fix-javascript-external-variables (form variables)
;;     (let ((references (reduce (lambda (acc var)				
;; 				(mapcar (lambda (reference)
;; 					  (if (or (eq (name reference) var) (boundp (name reference)))
;; 					      (pushnew reference acc)))
;; 					(ast-search-type form 'free-variable-reference))
;; 				acc)
;; 			      variables :initial-value nil)))
;;       (mapcar (lambda (reference)	      
;; 		(prog1 (change-class reference 'external-variable-reference)
;; 		  (describe reference)))
;; 	      references)
;;       form)))

