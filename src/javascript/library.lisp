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
;;| Javascript Library
;;+-----------------------------------------------------------------------------
;;
;; This file contains javascript library functions.
;;

(defrender/js core-library! ()
  (defun reduce (fun lst initial-value)
    (let ((result (or (and (not (typep initial-value 'undefined)) initial-value) nil)))
      (dolist (item lst)
	(setf result (fun result item)))
      result))

  (defun reduce0 (fun lst)
    (reduce fun lst nil))

  (defun reverse (lst)
    (.reverse lst))
  
  (defun filter (fun lst)
    (reverse
     (reduce0 (lambda (acc atom)
		(if (fun atom)
		    (cons atom acc)
		    acc))
	      lst)))

  (defun cons (atom lst)
    (if (null lst)
	(array atom)
	(.concat (array atom) lst)))

  (defun car (lst)
    (if (null lst)
	nil
	(aref lst 0)))

  (defun cdr (lst)
    (if (null lst)
	nil
	(.slice lst 1)))
  
  (defun mapcar (fun lst)
    (reverse
     (reduce (lambda (acc atom)
	       (cons (fun atom) acc))
	     lst))))
