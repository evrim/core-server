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

