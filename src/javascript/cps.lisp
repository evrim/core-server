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
    (if k
	`(,k (lambda (,@(unwalk-lambda-list arguments) ,k1)
	       ,(call-next-method form expand k1 env)))
	`(lambda (,@(unwalk-lambda-list arguments) ,k1)
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
    ;; (new
    ;;  (let ((ctor (car arguments)))
    ;;    `(make-instance ,k  ,(operator ctor) ,@(unwalk-forms (arguments ctor)))
    ;;    ;; (funcall expand
    ;;    ;; 		(walk-js-form
    ;;    ;; 		 )
    ;;    ;; 		expand k env)
    ;;    ))
    (javascript-cps-method-body
     `(let ((self (or self this))
	    ;; (,k (or ,k window.k))
	    )
	,(funcall expand
		  (make-instance 'implicit-progn-mixin
				 :body arguments)
		  expand k env))
     ;; (with-unique-names (k1)
     ;;   `(,k (lambda (,@(slot-value (car arguments) 'source) ,k1)
     ;; 	      (let ((self (or self this)))
     ;; 		,(funcall expand
     ;; 			  (make-instance 'implicit-progn-mixin
     ;; 					 :body (cdr arguments))
     ;; 			  expand k1 env)))))
     )
    (suspend
     nil)
    (reset
     (arnesi::extend env :let 'reset k)
     (funcall expand
	      (make-instance 'implicit-progn-mixin
			       :body  arguments)
	      expand k env))
    (shift
     (let ((r (arnesi::lookup env :let 'reset)))
       (assert (not (null r)) nil "Reset is not found in js cps env")
       (funcall expand
		(make-instance 'implicit-progn-mixin
			       :body arguments)
		expand r env)))
    (event
     `(,k (lambda (,@(slot-value (car arguments) 'source))
	    ,@(mapcar (lambda (a)
			(slot-value a 'source))
		      (cdr arguments)))))
    (:catch
	`(:catch ,(slot-value (car arguments) 'source)
	   ,(funcall expand (make-instance 'implicit-progn-mixin
	   				   :body (cdr arguments))
	   	     expand k env)))
    (try
     `(try
       ,(funcall expand
		 (make-instance 'implicit-progn-mixin
				:body (butlast arguments))
		 expand k env)
       ,(funcall expand (last1 arguments) expand k env)))
    (t (flet ((constant-p (form)
		(or (typep form 'constant-form)
		    (typep form 'variable-reference))))
	 (let* ((arguments (mapcar (lambda (arg)
				     (if (constant-p arg)
					 (cons arg (unwalk-form arg))
					 (cons arg (gensym))))
				   arguments))
		(lazy-arguments (reverse (filter
					  (compose #'not #'constant-p #'car)
					  arguments))))
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
		     ((eq 'call/cc operator)
		      `(,(cdr (car arguments)) ,@(mapcar #'cdr (cdr arguments)) ,k))
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
  (labels ((recursive-let (binds body)	     
	     `(let ((,(caar binds) ,(unwalk-form (cdar binds))))
		,@(if (null (cdr binds))
		      (unwalk-forms body)
		      (list (recursive-let (cdr binds) body))))))
    (funcall expand (walk-form (recursive-let binds body))
    	     expand k env)))

(defcps-expander/js flet-form (binds body)
  `(let ,(mapcar (lambda (bind)
		   `(,(car bind) ,(funcall expand (cdr bind) expand nil env)))
		 binds)
     ,(funcall expand (make-instance 'implicit-progn-mixin
				     :body body)
	       expand k env)))

(defcps-expander/js labels-form (binds body)
  `(let ,(mapcar (lambda (bind)
		   `(,(car bind) ,(funcall expand (cdr bind) expand nil env)))
		 binds)
     ,(funcall expand (make-instance 'implicit-progn-mixin
				     :body body)
	       expand k env)))

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
  (with-unique-names (temp var1 value1)
    (if (not (or (typep var 'constant-form)
		 (typep var 'variable-reference)))
	(funcall expand value expand
		 `(lambda (,value1)
		    ,(funcall expand var expand
			      `(lambda (,var1)
				 (,k (setq ,var1 ,value1)))
			      env))
		 env)
	(funcall expand value expand
		 `(lambda (,temp)
		    (,k (setq ,(slot-value var 'source) ,temp)))
		 env))))

(defcps-expander/js defun-form (name arguments body)
  (error "Please use defun/cc outside with-call/cc."))


;; +----------------------------------------------------------------------------
;; | Poor mans reduction: Fix Excessive Recursions (alpha,beta)
;; +----------------------------------------------------------------------------
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun fix-excessive-recursion (form)
;;    (describe (list 'start (unwalk-form form)))
    (flet ((replace-form (source target)
	     (prog1 target
	       (change-class target (class-name (class-of source)))
	       (mapcar (lambda (slot)
			 (setf (slot-value target slot) (slot-value source slot)))
		       (remove 'parent
			       (mapcar #'slot-definition-name (class-slots (class-of source))))))))
      (let ((applications (ast-search-type form 'lambda-application-form)))
	(mapcar (lambda (application)
		  (let ((operator (slot-value application 'operator)))
		    (mapcar (lambda (arg value)
			      (cond
				((typep value 'lambda-function-form)
				 (mapcar (lambda (ref) (replace-form value ref))
					 (filter (lambda (ref) (eq arg (slot-value ref 'name)))
						 (ast-search-type operator 'variable-reference)))
				 (mapcar (lambda (ref)
					   (change-class ref 'lambda-application-form)
					   (let ((new-form (walk-js-form (unwalk-form value))))
					     (setf (slot-value ref 'operator) new-form
						   (slot-value new-form 'parent) ref))
					   (fix-excessive-recursion ref))
					 (filter (lambda (ref) (eq arg (slot-value ref 'operator)))
						 (ast-search-type operator 'application-form))))
				((typep value 'constant-form)
				 (mapcar (lambda (ref) (replace-form value ref))
					 (filter (lambda (ref) (eq arg (slot-value ref 'name)))
						 (ast-search-type operator 'variable-reference))))
				((typep value 'variable-reference)
				 (mapcar (lambda (ref) (replace-form value ref))
					 (filter (lambda (ref) (eq arg (slot-value ref 'name)))
						 (ast-search-type operator 'variable-reference)))
				 (mapcar (lambda (ref) (setf (slot-value ref 'operator) (unwalk-form value)))
					 (filter (lambda (ref) (eq arg (slot-value ref 'operator)))
						 (ast-search-type operator 'application-form))))
				(t
				 (let ((refs (append
					      (filter (lambda (ref) (eq arg (slot-value ref 'operator)))
						      (ast-search-type operator 'application-form))
					      (filter (lambda (ref)
							(eq arg (slot-value ref 'name)))
						      (ast-search-type operator 'variable-reference)))))
				   (cond
				     ((eq 0 (length refs))
				      (setf (slot-value operator 'body)
				     	    (cons value (slot-value operator 'body))))
				     ((and (eq 1 (length refs)) (not (typep (car refs) 'application-form)))
				      (replace-form value (car refs)))
				     (t
				      (setf (slot-value operator 'body)
					    (list
					     (make-instance 'let-form
							    :binds (list (cons arg value))
							    :body (slot-value operator 'body)
							    :parent operator)))))))))
			    (if (slot-boundp operator 'arguments)
				(mapcar #'unwalk-form (slot-value operator 'arguments)))
			    (if (slot-boundp application 'arguments)
				(slot-value application 'arguments)))
		    (change-class application 'progn-form)
		    (setf (slot-value application 'body)
		    	  (slot-value operator 'body))))
		applications)))
;;     (describe (list 'end (unwalk-form form)))
    form)

;; +----------------------------------------------------------------------------
;; | Fix lambda default kontinuations
;; +----------------------------------------------------------------------------
  (defun fix-lambda-k (form)
    (prog1 form
      (let ((funs (filter (lambda (a) (> (length (slot-value a 'arguments)) 1))
			  (ast-search-type form 'lambda-function-form))))
	(mapcar (lambda (form)
		  (let ((last1 (last1 (if (slot-boundp form 'arguments)
					  (slot-value form 'arguments)))))
		    (when last1
		      (setf (slot-value form 'body)
			    (list
			     (make-instance 'let-form
			       :binds (let ((k-arg (unwalk-form last1)))
					(list `(,k-arg . ,(walk-js-form `(or ,k-arg window.k)))))
			       :body (slot-value form 'body)
			       :parent (parent form)))))))
		funs))))

  (defun fix-progn (form)
    (cond
      ((and (typep form 'progn-form)
	    (eq (length (slot-value form 'body)) 1))
       (car (slot-value form 'body)))
      (t form))))

;; ----------------------------------------------------------------------------
;; Interface
;; ----------------------------------------------------------------------------
(defmacro/js with-call/cc (&body body)
  (unwalk-form
   (fix-lambda-k
    (fix-progn
     (fix-excessive-recursion
      (walk-js-form
       (javascript->cps (walk-js-form
			 (if (= 1 (length body))
			     (car body)
			     `(progn ,@body)))
			#'javascript->cps '(lambda (val) val) nil)))))))

(defmacro/js defun/cc (name args &body body)  
  (with-unique-names (k)    
    (eval-when (:compile-toplevel :execute :load-toplevel)
      (setf (gethash name +javascript-cps-functions+) t))
    `(defun ,name (,@args ,k)
       (setf ,k (or ,k window.k))
       ,(unwalk-form
	 (fix-lambda-k
	  (fix-excessive-recursion
	   (walk-js-form
	    (javascript->cps (walk-js-form `(progn ,@body))
			     #'javascript->cps k nil))))))))

;; (let ((a (fun-a)))
;;   (list a a a))

;; (fun-a (lambda (a)
;; 	 (list a a a)))
