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
;;| Stream DSL Utilities
;;+----------------------------------------------------------------------------

(defun operator-search (operator goal-p &optional (successor #'operator-successor)
			                          (combiner #'append))
  (core-search (list operator) goal-p successor combiner))

(defun operator-search-type (operator types)
  (let (lst)
    (operator-search operator
		     (lambda (o)
		       (if (atom types)
			   (if (typep o types)
			       (push o lst))
			   (if (any (lambda (a) (typep o a)) types)
			       (push o lst)))
		       nil))
    lst))

