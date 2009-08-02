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
       (with-slots ,slots form
	 ,@body))))

(defcps-expander/js form (source)
  source)

(defcps-expander/js constant-form (value)
  `(,k ,value))

(defcps-expander/js variable-reference (name)
  `(,k ,name))

(defcps-expander/js lambda-function-form (arguments declares body)
  (with-unique-names (k1)
    `(,k (lambda (,@(unwalk-lambda-list arguments) ,k1)
	   (setf ,k1 (or ,k1 'window.k))	   
	   ,(call-next-method form expand k1 env)))))

(defcps-expander/js implicit-progn-mixin (body)
  (case (length body)
    (1 (funcall expand (car body) expand k env))
    (t (funcall expand (car body) expand
		(reduce (lambda (k form)
			  (with-unique-names (value)
			    `(lambda (,value)
			       ,(funcall expand form expand k env))))
			(butlast (reverse body))
			:initial-value k)
		env))))

(defcps-expander/js application-form (operator arguments)  
  (case operator
    (let/cc
	(let ((k-arg (unwalk-form (car arguments))))
	  `(let ((,k-arg ,k))
	     ,(funcall expand
		       (make-instance 'implicit-progn-mixin
				      :body (cdr arguments))
		       expand k-arg env))))    
    (new
     (let ((ctor (car arguments)))
       `(make-instance ,k  ,(operator ctor) ,@(unwalk-forms (arguments ctor)))
       ;; (funcall expand
       ;; 		(walk-js-form
       ;; 		 )
       ;; 		expand k env)
       ))
    (method
     (with-unique-names (k1)
       `(,k (lambda (,@(slot-value (car arguments) 'source) ,k1)
	      (let ((self (or self this)))
		,(funcall expand
			  (make-instance 'implicit-progn-mixin
					 :body (cdr arguments))
			  expand k1 env))))))
    (suspend
     nil)
    (event
     `(,k (lambda (,@(slot-value (car arguments) 'source))
	    ,@(mapcar (lambda (a)
			(slot-value a 'source))
		      (cdr arguments)))))
    (t (flet ((constant-p (form)
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
		      (funcall expand operator expand
			       `(lambda (fun)
				  (fun ,@(mapcar #'cdr arguments) ,k)) env))
		     ((gethash operator +javascript-cps-functions+)
		      `(,operator ,@(mapcar #'cdr arguments) ,k))
		     (t
;;		      (describe operator)
		      `(,k (,operator ,@(mapcar #'cdr arguments)))))))))))

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
		    ,(if else
			 (funcall expand else expand k env)
			 `(,k nil))))
	     env)))

(defcps-expander/js cond-form (conditions)
  (if (typep (caar conditions) 'constant-form)
      (funcall expand
	       (make-instance 'implicit-progn-mixin
			      :body (cdar conditions))
	       expand k env) 
      (funcall expand
	       (walk-js-form (macroexpand-1 (slot-value form 'source)))
	       expand k env)))

(defcps-expander/js setq-form (var value)
  (with-unique-names (temp)
    (funcall expand value expand
	     `(lambda (,temp)
		(,k (setq ,(slot-value var 'source) ,temp)))
	     env)))


(defcps-expander/js defun-form (name arguments body)
  (error "Please use defun/cc outside with-call/cc."))

;; ----------------------------------------------------------------------------
;; Interface
;; ----------------------------------------------------------------------------
(defmacro/js with-call/cc (&body body)
  (javascript->cps (walk-js-form `(progn ,@body))
		   #'javascript->cps 'k nil))

(defmacro/js defun/cc (name args &body body)
  (setf (gethash name +javascript-cps-functions+) t)
  (with-unique-names (k)
    `(defun ,name (,@args ,k)
       (setf ,k (or ,k window.k))
       ,(javascript->cps (walk-js-form `(progn ,@body))
			 #'javascript->cps k nil))))

;; (let ((a (fun-a)))
;;   (list a a a))

;; (fun-a (lambda (a)
;; 	 (list a a a)))