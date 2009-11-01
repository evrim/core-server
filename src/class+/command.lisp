(in-package :core-server)

;; +----------------------------------------------------------------------------
;; | Command Framework
;; +----------------------------------------------------------------------------
(defclass command+ (class+)
  ()
  (:documentation "Metaclass of Command Framework"))

(defmethod class+.ctor ((class command+))
  (let ((name (class+.name class)))
    (aif (class+.%ctor class)
	 `(progn
	    (defun ,name ,@it
	      (run (make-instance ',name ,@(class+.ctor-arguments class it))))
	    (defun ,(intern (format nil "~AM" name) (symbol-package name)) ,@it
	      (make-instance ',name ,@(class+.ctor-arguments class (car it)))))
	 `(progn
	    (defun ,name (&key ,@(class+.ctor-lambda-list class))
	      (run (make-instance ',name ,@(class+.ctor-arguments class))))
	    (defun ,(intern (format nil "~AM" name) (symbol-package name))
		(&key ,@(class+.ctor-lambda-list class))
	      (make-instance ',name ,@(class+.ctor-arguments class)))))))

(defmethod class+.ctor-lambda-list ((self command+) &optional (include-supplied-p nil))
  (reduce0 (lambda (acc slot)
	     (with-slotdef (initarg initform supplied-p) slot
	       (if initarg
		   (let ((symbol (intern (symbol-name initarg))))
		     (if include-supplied-p
			 (cons (list symbol initform supplied-p) acc)
			 (cons (list symbol initform) acc)))
		   acc)))
	   (reverse (class+.local-slots self))))
  
(defmethod class+.ctor-arguments ((self command+) &optional lambda-list)      
  (reduce0 (lambda (acc slot)
	     (with-slotdef (initarg) slot
	       (if initarg
		   (cons initarg (cons (intern (symbol-name initarg)) acc))
		   acc)))
	   (aif (extract-argument-names lambda-list)
		(filter (lambda (slot)
			  (with-slotdef (name) slot
			    (member name it :test #'string=)))
			(class+.slots self))
		(class+.local-slots self))))

;; ----------------------------------------------------------------------------
;; defcommand Macro
;; ----------------------------------------------------------------------------
(defmacro defcommand (name supers slots &rest rest)
  `(defclass+ ,name (,@supers command)
     ,slots
     ,@(append rest '((:metaclass command+)))))

;; ----------------------------------------------------------------------------
;; Command Base Class
;; ----------------------------------------------------------------------------
(defvar +verbose+ t
  "make command executions verbose during installation.")

(defclass command ()
  ((input-stream :accessor command.input-stream :initarg :input-stream)
   (output-stream :accessor command.output-stream :initarg :output-stream
		  :initform nil)
   (verbose :accessor command.verbose :initarg :verbose :initform +verbose+)
   (verbose-stream :accessor command.verbose-stream :initarg :verbose-stream
		   :initform *standard-output*))
  (:metaclass command+))

;; ----------------------------------------------------------------------------
;; Protocol
;; ----------------------------------------------------------------------------
(defgeneric run (command)
  (:documentation "Run this command with given args."))

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>

