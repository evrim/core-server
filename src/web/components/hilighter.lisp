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

(defcomponent hilighter ()
  ((menu-query :host remote :accessor menu-query :initarg :menu-query
	       :initform ".menu a")
   (active-class :host remote :initarg :active-class :initform "active")
   (passive-class :host remote :initarg :passive-class :initform "")))

(defmethod/remote hilight ((self hilighter) anchor)
  (dolist (item (dojo.query this.menu-query))
    (if (= anchor (item.hash.substr 1))
	(setf item.parent-node.class-name this.active-class
	      item.class-name this.active-class)
	(setf item.parent-node.class-name this.passive-class
	      item.class-name this.passive-class))))