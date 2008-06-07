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
;;| Default Application Implementation
;;+----------------------------------------------------------------------------
;;
;; This file contains methods that every application would need.
;;

(defmethod sieve-initargs ((self application) initargs)
  "Remove unserializable initargs from 'initargs'"
  (let (current-keyword)
    (remf initargs :dispatchers)
    (reduce #'(lambda (acc arg)
		(cond 
		  ((eq 0 (mod (position arg initargs) 2))
		   (setf current-keyword arg)
		   acc)
		  ((eq 1 (mod (position arg initargs) 2))		     
		   (cond
		     ((or (functionp arg) (typep arg 'standard-object))
		      acc)		     
		     (t
		      (append acc (list current-keyword arg)))))))
	    initargs :initial-value '())))

(defmethod shared-initialize :after ((self application) slot-names 
				     &rest initargs &key &allow-other-keys)
  "Saves primitive initargs into initargs slot so that application
serialization can occur"
  (setf (application.initargs self) (sieve-initargs self initargs)))