(in-package :tr.gen.core.server)

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

(defun string-search (str selector)
  #'(lambda (x)
      (string= str (funcall selector x))))

(defun integer-search (int selector)
  #'(lambda (x)
      (eq int (funcall selector x))))
