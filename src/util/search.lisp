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
