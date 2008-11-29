(in-package :core-server)

;; +----------------------------------------------------------------------------
;; | Javascript CPS Conversion
;; +----------------------------------------------------------------------------
;;
;; Author: Evrim Ulu <evirm@core.gen.tr>
;; Date: 29/11/2008

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar +javascript-cps-functions+ (make-hash-table)))


;; ----------------------------------------------------------------------------
;; Function Homomorphism
;; ----------------------------------------------------------------------------
(fmakunbound 'javascript->cps)

(defgeneric javascript->cps (form expand k env)
  (:documentation "Expanders for lisp to javascript cps transformation"))

(defmacro defcps-expander/js (class (&rest slots) &body body)
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (defmethod javascript->cps ((form ,class) expand k env)
       (declare (ignorable expand))
;;;        (format t "Inside ~A~%" ',class)
       (with-slots ,slots form
	 ,@body))))

(defcps-expander/js form (source)
  source)

(defcps-expander/js constant-form (value)
  `(,k ,value))

(defcps-expander/js variable-reference (name)
  `(,k ,name))

(defcps-expander/js lambda-function-form (arguments declares body)
  (with-unique-names (k)
    `(lambda (,k ,@(unwalk-lambda-list arguments))
       ,(call-next-method form expand k env))))

(defcps-expander/js implicit-progn-mixin (body)
  (case (length body)
    (1 (funcall expand (car body) expand k nil))
    (t
     (funcall expand (car body) expand
	      (reduce (lambda (k form)
			`(lambda (value)
			   ,(funcall expand form expand k env)))
		      (cdr body)
		      :initial-value k)
	      env))))

(defcps-expander/js application-form (operator arguments)  
  (flet ((constant-p (form)
	   (or (typep form 'constant-form) (typep form 'variable-reference))))
    (let* ((arguments (mapcar (lambda (arg)
				(if (constant-p arg)
				    (cons arg (unwalk-form arg))
				    (cons arg (gensym))))
			      arguments))
	   (lazy-arguments (reverse (filter (compose #'not #'constant-p #'car) arguments))))
      (reduce (lambda (k arg)
		(let ((form (car arg))
		      (symbol (cdr arg)))
		  (funcall expand form expand `(lambda (,symbol) ,k) env)))
	      lazy-arguments
	      :initial-value 
	      (cond
		((typep operator 'lambda-function-form)
		 `(,(funcall expand operator expand nil nil)
		    ,k ,@(mapcar #'cdr arguments)))
		((gethash operator +javascript-cps-functions+)
		 `(,operator ,k ,@(mapcar #'cdr arguments)))
		(t
		 `(,k (,operator ,@(mapcar #'cdr arguments)))))))))

(defcps-expander/js let-form (binds body)
  (funcall expand
	   (make-instance
	    'application-form
	    :operator (make-instance 'lambda-function-form
				     :arguments (walk-lambda-list (mapcar #'car binds)
								  (parent form) nil)
				     :body body)
	    :arguments (mapcar #'cdr binds)
	    :parent (parent form))
	   expand k env))

(defcps-expander/js let*-form (binds body)
  (funcall expand
	   (make-instance
	    'application-form
	    :operator (make-instance 'lambda-function-form
				     :arguments (walk-lambda-list (mapcar #'car binds)
								  (parent form) nil)
				     :body body)
	    :arguments (mapcar #'cdr binds)
	    :parent (parent form))
	   expand k env))

(defcps-expander/js if-form (consequent then else)
  (with-unique-names (value)
    (funcall expand consequent expand
	     `(lambda (,value)
		(if ,value
		    ,(funcall expand then expand k env)
		    ,(funcall expand else expand k env)))
	     env)))

(defcps-expander/js setq-form (var value)
  (with-unique-names (temp)
    (funcall expand value expand
	     `(lambda (,temp)
		(,k (setq ,(slot-value var 'source) ,temp)))
	     env)))


(defcps-expander/js defun-form (name arguments body)
  (setf (gethash name +javascript-cps-functions+) t)
  (with-unique-names (k1)
    `(progn
       (defun ,name (,k1 ,@(unwalk-lambda-list arguments))
	 ,(funcall expand (car body) expand
		   (reduce (lambda (k form)
			     `(lambda (value)
				,(funcall expand form expand k env)))
			   (cdr body)
			   :initial-value k1)
		   env))
       (,k nil))))

;; ----------------------------------------------------------------------------
;; Interface
;; ----------------------------------------------------------------------------
(defmacro/js with-call/cc (&body body)
  (javascript->cps (walk-js-form `(progn ,@body))
		   #'javascript->cps `(lambda (value) value) nil))

(defmacro/js defun/cc (name args &body body)
  (setf (gethash name +javascript-cps-functions+) t)
  (with-unique-names (k)
    `(defun ,name (,k ,@args)
       ,(javascript->cps (walk-js-form `(progn ,@body))
			 #'javascript->cps k nil))))

;; (let ((a (fun-a)))
;;   (list a a a))

;; (fun-a (lambda (a)
;; 	 (list a a a)))