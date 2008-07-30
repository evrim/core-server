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

(in-package :tr.gen.core.server)

;;+----------------------------------------------------------------------------
;;| Tree Search Utilities
;;+----------------------------------------------------------------------------

;; This is the generic search algorithm which is given in PAIP and
;; AIMA books.
(defun core-search (states goal-p successors combiner)
  "Find a state that satisfies the goal-p. Start with states, and
  search according to successors and combiner"
  (cond ((null states) 'fail)
	((funcall goal-p (car states)) (car states))
	(t (core-search
	    (funcall combiner
		     (funcall successors (car states))
		     (rest states))
	    goal-p successors combiner))))

;; Standard successors
(defun string-search (str selector)
  "Returns a lambda that would match the string 'str'"
  #'(lambda (x)
      (string= str (funcall selector x))))

(defun integer-search (int selector)
  "Returns a lambda that would match the integer 'int'"
  #'(lambda (x)
      (eq int (funcall selector x))))
