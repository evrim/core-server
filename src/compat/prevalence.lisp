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

(in-package :cl-prevalence)

;;+----------------------------------------------------------------------------
;;| Cl-Prevalence Compat Functions
;;+----------------------------------------------------------------------------
;;
;; This file overrides some properties of cl-prevalence database.

(defun cl-prevalence::get-objects-slot-index-name (class &optional (slot 'id))
  (any (lambda (class)
	 (if (member slot (mapcar #'slot-definition-name (mopp::class-direct-slots class)))
	     (intern (concatenate 'string (symbol-name (class-name class))
				  "-" (symbol-name slot) "-INDEX")
		     :keyword)))
       (class-superclasses class 'standard-class)))

(defun cl-prevalence::class-compile-timestamp (system class)
  (or (get-root-object
       system (intern (symbol-name (class+.name (find-class+ class))) :keyword))
      0))

(defsetf cl-prevalence::class-compile-timestamp (system class) (value)
  `(setf (get-root-object
	  ,system (intern (symbol-name (class+.name (find-class+ ,class))) :keyword))
	 ,value))

(defun cl-prevalence::class-expired (system class)
  (let ((class+ (find-class+ class)))
    (when class+
      (if (> (slot-value class+ '%timestamp)
	     (cl-prevalence::class-compile-timestamp system (class+.name class)))
	  t))))

(defun cl-prevalence::create-object-slot-indexes (system class)
  (let ((class+ (find-class+ class)))
    (when (and class+ (cl-prevalence::class-expired system class+))
      (mapcar (lambda (slot)
		(cl-prevalence::tx-create-objects-slot-index system class slot #'equalp))
	      (mapcar #'slot-definition-name (class+.indexes class+))))
      (setf (cl-prevalence::class-compile-timestamp system class)
	    (slot-value class+ '%timestamp))))

(defun cl-prevalence::tx-create-object (system class &optional slots-and-values)
  "Create a new object of class in system, assigning it a unique id, optionally setting some slots and values"
  (cl-prevalence::create-object-slot-indexes system class)
  (let* ((id (next-id system))
	 (object (let ((object (allocate-instance (find-class class))))
		   (setf (slot-value object 'id) id)
		   object))
	 (index-name (cl-prevalence::get-objects-slot-index-name class 'id))
	 (index (or (get-root-object system index-name)
		    (setf (get-root-object system index-name) (make-hash-table)))))
    (push object (get-root-object system (cl-prevalence::get-objects-root-name class)))
    (setf (gethash id index) object)
    (tx-change-object-slots system class id slots-and-values)
    (initialize-instance object)
    object))
