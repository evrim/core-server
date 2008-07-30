(in-package :tr.gen.core.server)

;;-----------------------------------------------------------------------------
;; AST Search Utilities
;;-----------------------------------------------------------------------------
;;
;; These utilities are used to search arnesi lisp2 forms.
;; see core-server/lib/arnesi/src/walk.lisp
;;
(defun ast-search (form goal-p &optional (successor #'form-successor)
		                         (combiner #'append))
  (core-search (list form) goal-p successor combiner))

(defun ast-search-type (form types)
  (let (lst)
    (ast-search form
		(lambda (f)
		  (if (atom types)
		      (if (typep f types)
			  (push f lst))
		      (if (any (lambda (a) (typep f a)) types)
			  (push f lst)))		  
		  nil))
    lst))
