(in-package :core-server)

;; +----------------------------------------------------------------------------
;; | Class+ Protocol
;; +----------------------------------------------------------------------------
(defun find-class+ (class+)
  "Returns the instance of class+ associated with the symbol named 'class+'."
  (etypecase class+
    (symbol (find-class class+ nil))
    (class+ class+)))

(defgeneric class+.name (class+)
  (:documentation "Name of this class+"))

(defgeneric class+.direct-superclasses (class+)
  (:documentation "Returns direct super classes+ of this class+")
  (:method ((self t)) nil))

(defgeneric class+.superclasses (class+)
  (:documentation "Returns super classes+ of this class+")
  (:method ((self t)) nil))

(defgeneric class+.direct-subclasses (class+)
  (:documentation "Returns direct sub classes+ of this class+")
  (:method ((self t)) nil))

(defgeneric class+.subclasses (class+)
  (:documentation "Returns sub classes+ of this class+")
  (:method ((self t)) nil))

(defgeneric class+.slots (class+)
  (:documentation "Returns all slots of this class+")
  (:method ((self t)) nil))

(defgeneric class+.rest (class+)
  (:method ((self t)) nil))

(defgeneric class+.methods (class+)
  (:documentation "Returns list of all method-names and associated lambda lists")
  (:method ((self t)) nil))

;; ----------------------------------------------------------------------------
;; Extra Definitions
;; ----------------------------------------------------------------------------
(defgeneric class+.slot-search (class+ match-p)
  (:documentation "Returns all slots that satisfies match-p lambda")
  (:method ((self t) match-p) nil))

;; (defgeneric class+.search (class+ goal-p &optional successors combiner)
;;   (:documentation "Convenient search method that by default applies goal-p lambda to
;; all superclasses of this class+ until goal-p returns a value other than nil"))

(defgeneric class+.local-slots (class+)
  (:documentation "Returns an assoc list of local-slots with corresponding default values")
  (:method ((self t)) nil))

(defgeneric class+.remote-slots (class+)
  (:documentation "Returns an assoc list of remote-slots with corresponding default values")
  (:method ((self t)) nil))

(defgeneric class+.local-methods (class+)
  (:documentation "Returns list of local method-names and associated lambda lists")
  (:method ((self t)) nil))

(defgeneric class+.remote-methods (class+)
  (:documentation "Returns list of remote method-names and associated lambda lists ")
  (:method ((self t)) nil))

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
