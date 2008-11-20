(in-package :core-server)

(fmakunbound 'expand-cps)

(defgeneric expand-cps (form expand k env)
  (:documentation "Expanders for cps transformation"))

(defmacro defcps-expander (class (&rest slots) &body body)
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (defmethod expand-cps ((form ,class) expand k env)
       (declare (ignorable expand))
       ;;      (format t "Inside ~A~%" ',class)
       (with-slots ,slots form
	 ,@body))))

(defparameter +k+
  (lambda (&rest values)
    (throw 'done (apply #'values values))))

(defcps-expander form (source)
;;   (unwalk-form form)
  source
  )

(defcps-expander null ()
;;   (error "Expand-cps got nil")
  nil)

(defcps-expander lambda-function-form (arguments declares body)
  `(lambda (,@(unwalk-lambda-list arguments))
     (declare ,@(unwalk-forms declares))
     ,@(call-next-method)))

(defcps-expander implicit-progn-mixin (body)
  ;; optimize a little bit, remove constants and var refs from progn,
  ;; they are not needed for next evaluation
  (let ((body (reverse
	       (cons (car (reverse body))
		     (filter (lambda (form)
			       (not
				(or (typep form 'constant-form)
				    (typep form 'variable-reference))))
			     (cdr (reverse body)))))))
    
    (reduce (lambda (acc form)
	      (cons (with-unique-names (values)
		      `(write-stream +stream+ (lambda (&rest ,values)
						(declare (ignore ,values))
						,(funcall expand form expand k env))))

		    acc))
	    (cdr body) :initial-value (list (funcall expand (car body) expand k env)))))

(defcps-expander progn-form (body)
  (if (= 1 (length body))
      (car (call-next-method))
      `(progn ,@(call-next-method))))

(defcps-expander application-form (operator arguments)
  (cond
    ((eq operator 'call/cc1)
     (with-unique-names (value stack checkpoints)
       `(let* ((,stack (slot-value +stream+ '%stack))
	       (,checkpoints (slot-value +stream+ '%checkpoints)))
	  (funcall ,(funcall expand (car arguments) expand nil nil)
		   (lambda (,value)
		     (if (not (typep +stream+ 'core-cps-stream))
			 (error "Please call inside call/cc"))
		     (setf (slot-value +stream+ '%stack) ,stack
			   (slot-value +stream+ '%checkpoints) ,checkpoints)
		     ,value)))))
    (t
     (flet ((constant-p (form)
	      (or (typep form 'constant-form)
		  (typep form 'variable-reference))))
       (let* ((arguments (mapcar (lambda (arg)
				   (if (constant-p arg)
				       (cons arg (unwalk-form arg))
				       (cons arg (gensym))))
				 arguments))
	      (lazy-arguments (reverse (filter (compose #'not #'constant-p #'car) arguments))))
	 (reduce (lambda (k arg)
		   (with-unique-names (values)
		     `(progn
			(write-stream +stream+ (lambda (,(cdr arg) &rest ,values)
						 (declare (ignore ,values))
						 ,k))
			,(funcall expand (car arg) expand nil nil))))
		 lazy-arguments
		 :initial-value `(,(if (gethash operator +cps-functions+)
				       (intern (format nil "~A/CC" operator))
				       operator)
				   ,@(mapcar (lambda (arg) (cdr arg)) arguments))))))))

(defcps-expander multiple-value-call-form (func arguments)  
  `(progn
     (write-stream +stream+ ,(funcall expand func expand nil nil))
     ,(funcall expand (car arguments) expand nil nil)))

(defcps-expander let-form (binds declares body)
  (if (> (length binds) 0)
      (funcall expand
	       (make-instance
		'application-form
		:operator 'funcall
		:arguments (cons (make-instance 'lambda-function-form
						:arguments (walk-lambda-list (mapcar #'car binds)
									     (parent form) nil)
						:declares declares
						:body body)
				 (mapcar #'cdr binds))
		:parent (parent form))
	       expand k env)
      `(progn ,@(call-next-method))))

(defcps-expander let*-form (binds body declares)
  (let ((binds (reverse binds)))
    (if (> (length binds) 0)
	(funcall expand		 
		 (reduce (lambda (k bind)
			   (make-instance 'application-form
					  :operator 'funcall
					  :arguments
					  (cons (make-instance 'lambda-function-form
							       :arguments (walk-lambda-list (list (car bind)) (parent form) nil)
							       :body (list k))
						(list (cdr bind)))))
			 (cdr binds)
			 :initial-value
			 (make-instance 'application-form
					:operator 'funcall
					:arguments
					(cons (make-instance 'lambda-function-form
							     :arguments (walk-lambda-list (list (caar binds))
											  (parent form) nil)
							     :declares declares
							     :body body)
					      (list (cdar binds)))))
		 expand k env)
	`(progn ,@(call-next-method)))))

(defcps-expander block-form (name body)
  (with-unique-names (values)
    `(progn
       (checkpoint-stream2 +stream+ ',name)
       (write-stream +stream+ (lambda (&rest ,values)
				(rewind-stream2 +stream+ ',name)
				(apply #'values ,values)))
       ,@(call-next-method form expand nil env))))

(defcps-expander return-from-form (name target-block result)
  `(progn
     (rewind-stream2 +stream+ ',name)
     ,(funcall expand result expand nil env)))

(defcps-expander if-form (consequent then else)
  (with-unique-names (value values)
    `(progn
       (write-stream +stream+ (lambda (,value &rest ,values)
				(declare (ignore ,values))
				(if ,value
				    ,(funcall expand then expand nil env)
				    ,(if else
					 (funcall expand else expand nil env)))))
       ,(funcall expand consequent expand nil env))))

(defcps-expander the-form (type-form value)
  (if (or (typep value 'constant-form)
	  (typep value 'variable-reference))
      (call-next-method)
      (let ((sym (gensym)))
	(funcall expand
		 (walk-form
		  `(let ((,sym ,(unwalk-form value)))
		     (declare (,type-form ,sym))
		     ,sym)
		  (parent form))
		 expand nil nil))))

(defcps-expander tagbody-form (body tags)
  `(labels ,(mapcar (lambda (tag next-tag)
		      (let ((body (walk-form
				   (if next-tag
				       `(progn
					  ,@(unwalk-forms (cdr tag))
					  (,(car next-tag)))
				       `(progn ,@(unwalk-forms (cdr tag))))
				   (parent form))))
			`(,(car tag) ()
			   (checkpoint-stream2 +stream+ ',(car tag))
			   ,(funcall expand body expand (car tag) nil))))
		    tags (append (cdr tags) (list nil)))
     ,@(mapcar (rcurry expand expand nil nil)
	       (let ((collect t))
		 (nreverse
		  (reduce0 (lambda (acc form)
			     (cond
			       ((and collect (typep form 'go-tag-form))
				(setq collect nil)
				acc)
			       ((and collect (typep form 'go-form))
				(setq collect nil)
				(cons form acc))
			       ((not (null collect))
				(cons form acc))
			       (t
				acc)))
			   body))))))

(defcps-expander go-form (name target-progn enclosing-tagbody)
  (if k
      `(progn
	 (rewind-stream2 +stream+ ',k)
	 (,name))
      `(,name)))

(defcps-expander go-tag-form (name)
  (call-next-method))

(defcps-expander setq-form (var value)
  (if (or (typep value 'constant-form)
	  (typep value 'variable-reference))
      `(setq ,(unwalk-form var) ,(unwalk-form value))
      (with-unique-names (sym other-values)
	`(progn
	   (write-stream +stream+
			 (lambda (,sym &rest ,other-values)
			   (declare (ignore ,other-values))
			   (setq ,(unwalk-form var) ,sym)))
	   ,(funcall expand value expand nil nil)))))

(defmacro with-call/cc2 (&body body)
  `(run
    (make-instance 'core-cps-stream
		   :stack (list ,(expand-cps (walk-form `(lambda (&rest values)
							   (declare (ignore values))
							   ,@body)) #'expand-cps '#'identity nil)))))

(defmacro let/cc1 (k &body body)
  `(call/cc1 (lambda (,k) ,@body)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar +cps-functions+ (make-hash-table)) 

  (defmacro defun/cc1 (name args &body body)    
    (setf (gethash name +cps-functions+) t)
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (setf (gethash ',name +cps-functions+) t))
       (defun ,name ,args
	 ,(expand-cps (walk-form `(block ,name ,@body)) #'expand-cps nil nil))))
  
  (defmacro defun/cc2 (name args &body body)
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (setf (gethash ',name +cps-functions+) t))
 ;;       (warn "Compiling CC Function ~A" ',name)
       (defun ,name ,args
	 ,@body)
       (defun ,(intern (format nil "~A/CC" name)) ,args
	 ,(expand-cps (walk-form `(block ,name ,@body)) #'expand-cps nil nil))))

  (defmacro defmethod/cc1 (name args &body body)
    (if (keywordp args)
	`(defmethod ,name ,args ,(pop body)
	   ,(expand-cps (walk-form `(block ,name ,@body)) #'expand-cps nil nil))
	`(defmethod ,name ,args
	   ,(expand-cps (walk-form `(block ,name ,@body)) #'expand-cps nil nil))))
  
  (defmacro defmethod/cc2 (name args &body body)
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (setf (gethash ',name +cps-functions+) t))
       (defmethod ,name ,args ,@body)
       (defmethod ,(intern (format nil "~A/CC" name)) ,args
	 ,(expand-cps (walk-form `(block ,name ,@body)) #'expand-cps nil nil)))))

(setf (gethash 'reduce +cps-functions+) t)

(defun/cc1 reduce/cc (lambda list &key initial-value)
  (if (and list (listp list))
      (reduce lambda (cdr list) :initial-value (funcall lambda initial-value (car list)))
      initial-value))

(setf (gethash 'mapcar +cps-functions+) t)
(defun/cc1 mapcar/cc (lambda list)
  (nreverse
   (reduce (lambda (acc atom) (cons (funcall lambda atom) acc)) list)))

;; (defparser abc1 ()
;;   (:return (values 1 2 3)))

;; (defparser abc2 (a b c)
;;   (:abc1 a b c)
;;   (:return (list a b c)))
;; (defparser test-parser2 ((abc "ebeN"))
;;   (:sci abc)
;;   (:return t))

;; (defparser test-parser1 (c (acc (make-accumulator)))
;;   (:zom (:type visible-char? c)
;; 	(:collect c acc))
;;   (:return (values acc 'test-parser1)))

;; (defparser test-parser2 (a b)
;;   #\% (:test-parser1 a b)
;;   (:return (list a b)))

;; (defun/cc2 test1 ()
;;   (let ((a nil))
;;     (push 'gee a)
;;     (push 'zee a)
;;     a))

;; (defun/cc2 abc ()
;;   (values 1 2 3))

;; (defun/cc2 abc (a b c)
;;   (list a b c))

;; (defmethod/cc2 abc123 ((self core-cps-stream) a)
;;   (list self a))

;; (defun/cc2 test-tagbody (a b c)  
;;   (let ((count 0))
;;     (tagbody
;;        (setq a 'ae)
;;        (go label-2)
;;      label-1
;;        (setq b 'be)
;;        (if (> count 100000)
;; 	   (go label-4)
;; 	   (progn
;; ;; 	     (describe count)
;; 	     (setq count (+ 1 count))
;; 	     (go label-2)))
;;      label-2
;;        (setq c 'ce)
;;        (go label-1)
;;      label-4
;;        (list a b c)))
;;   (list a b c))


;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (defmacro defparser/cc (name args &body body)
;;     (with-unique-names (stream)
;;       (flet ((rule-args ()
;; 	       (if args
;; 		   (list* stream '&aux args)
;; 		   (list stream))))    
;; 	`(defun/cc2 ,name ,(rule-args)
;; 	   (block rule-block
;; 	     (let ((cp (current-checkpoint ,stream)))
;; 	       ,(expand-parser (walk-grammar `(:checkpoint ,@body))
;; 			       #'expand-parser stream)
;; 	       nil)))))))

;; (defmacro defparser2 (name args &body body)
;;   (with-unique-names (stream)
;;     (flet ((rule-args ()
;; 	     (if args
;; 		 (list* stream '&aux args)
;; 		 (list stream))))    
;;       `(defun/cc2 ,name ,(rule-args)
;; 	 ,(unwalk-form
;; 	   (walk-form
;; 	    `(block rule-block
;; 	       (let ((cp (current-checkpoint ,stream)))
;; 		 ,(expand-parser (walk-grammar `(:checkpoint ,@body))
;; 				 #'expand-parser stream)
;; 		 nil))))))))

;; (defparser/cc test-parse1 (c (acc (make-accumulator)))
;;   (:zom (:type visible-char? c)
;; 	(:collect c acc))
;;   (:return acc))

;; (defparser2 test-parse1 (c (acc (make-accumulator)))
;;   (:zom (:type visible-char? c)
;; 	(:collect c acc))
;;   (:return acc))

;; (with-kol/cc
;;   (let ((a 1))
;;     (block moo
;;       (list a 2 3)
;;       (return-from moo 'eben)
;;       (list a a a))
;; ;;     (block eben
;; ;;       (list a 2 3)
;; ;;       ;; (return-from eben
;; ;; ;; 	(list 'a
;; ;; ;; 	      (block block-name
;; ;; ;; 		;;     (list 'a 2 3)
;; ;; ;; 		(return-from block-name (list 1 2 3))
;; ;; ;; 		(list 3 2 1))))
;; ;;       )
;; ;;     (list a 1)
;;     )
;;   )


;; (with-kol/cc
;;   (let ((a (list 1 2)))
;;     (list a (block moo
;; ;;	      (return-from moo 'eben)
;; 	      (list 1 2 3)
;; 	      ))))

;; (let* ((stack (expand-cps (walk-form `(lambda (value)
;; 					(declare (ignore value))
;; 					(list (list 3 2 1) (list 1 2 3))
;; 					(list (list 3 2 1) (list 1 2 3))
;; 					(list (list 3 2 1) (list 1 2 3))
;; 					(list (list 3 2 1) (list 1 2 3))
;; 					(list (list 3 2 1) (list 1 2 3))
;; 					(list (list 3 2 1) (list 1 2 3))
;; 					(list (list 3 2 1) (list 1 2 3))
;; 					(list 1 2 3))) #'expand-cps '#'identity nil))
;;        (stack (eval stack))
;;        (monad (make-instance 'core-cps-stream :stack (list stack))))
;;   (describe stack)
;;   (time
;;    (loop for i from 0 upto 10000
;;       do (run monad)
;;       do (setf (slot-value monad '%stack) (list stack)))))

;; (defvar +cps+ nil)
;; (defparameter +g+
;;   (lambda (value)
;;     (write-stream +stream+
;; 		  (lambda (value)
;; 		    (write-stream +stream+
;; 				  (lambda (value)
;; 				    (list 'a 'b 'c)))
;; 		    (list 3 2 1)))
;;     (list 1 2 3)))

;; (let ((self (make-instance 'core-cps-stream
;; 			   :stack (loop for i from 0 upto 10000 collect +g+))))
;;   (time (run self)))

;; (lambda (value)
;;   (write-stream +stream+
;; 		(lambda (value)
;; 		  (list 1 2 3 value)))
;;   (list 3 2 1))

;; (defcps-expander implicit-progn-mixin (body)
;;   (cond
;;     ((= 1 (length body))
;;      (funcall expand (car body) expand k env))
;;     (t
;;      (funcall expand
;; 	      (walk-form `(progn ,@(unwalk-forms (butlast body))))
;; 	      expand
;; 	      `(lambda (value)
;; 		 ,(funcall expand (car (reverse body)) expand k env))
;; 	      env))))

;; (defvar +cps-functions+ (make-hash-table))
;; (setf (gethash 'do-a +cps-functions+) t)
;; (setf (gethash 'do-b +cps-functions+) t)

;; (defcps-expander application-form (operator arguments)
;;   (let ((k (if (symbolp k)
;; 	       `(function ,k)
;; 	       k)))
;;     (cond
;;       ((gethash operator +cps-functions+)
;;        `(let ((+k+ ,k))
;; 	  (,operator ,@(mapcar (rcurry expand expand k env) arguments))))
;;       ((eq 'call/cc operator)
;;        `(funcall ,(funcall expand (car arguments) expand k env) ,k))
;;       (t
;;        `(funcall ,k (,operator ,@(mapcar (rcurry expand expand k env) arguments)))))))

;; (defmacro with-kol/cc (&body body)
;;   (expand-cps (walk-form `(progn ,@body)) #'expand-cps 'identity nil))

;; (defmacro defun/ccc (name args &body body)
;;   (with-unique-names (k)
;;     `(defun ,name (,@args ,k)
;;        ,(expand-cps (walk-form `(progn ,@body)) #'expand-cps k nil))))

;; (defun/ccc do-a (a b c)
;;   (list a b c))

;; (defmacro defun/cccc (name args &body body)
;;   `(defun ,name ,args
;;      ,(expand-cps (walk-form `(progn ,@body)) #'expand-cps '+k+ nil)))

;; (defun/cccc do-b (a b c)
;;   (list a b c))

;; (with-kol/cc
;; ;;  (do-a 1 2 3)
;;   (do-b 3 2 1)
;;   (do-b 1 2 3)
;;   (let/cc k
;;     (funcall k k))
;;   (identity 5)
;;   ;; (list 'a 'b 'd)
;;   )


;; (progn
;;   (do-a 1 2 3)
;;   (do-b 3 2 1)
;;   (list 'a 'b 'c))

;; (progn
;;   (do-a 1 2 3
;; 	(lambda (value)
;; 	  (do-b 3 2 1
;; 		(lambda (value)
;; 		  (list 'a 'b 'c kont))))))

;; (progn
;;   (do-a 1 2 3 kont)
;;   (do-b 1 2 3 kont)
;;   (list 'a 'b 'c kont))

;; (defmacro defun/ccc (name args &body body)
;;   (with-gensym (k)
;;     `(defun ,name (,k ,args)
;;        ,)))

;; (defcps-expander application-form (operator arguments)
;;   )

;; (with-kol/cc
;;   (let/cc k
;;     ))

;; (defclass core-cps-stream (core-stream)
;;   ((%stack :initarg :stack :initform (error "Stack should not be nil"))))

;; (defmethod read-stream ((self core-cps-stream))
;;   (pop (s-v '%stack)))

;; (defmethod write-stream ((self core-cps-stream) value)
;;   (push value (s-v '%stack)))

;; (defmethod run ((self core-cps-stream))
;;   (catch 'done
;;     (do ((k (read-stream self) (read-stream self))
;; 	 (value nil (funcall k value)))
;; 	((null k) value))))

;; (defcps-expander form ()
;;   (unwalk-form form))

;; (defmacro with-kol/cc (&body body)
;;   `(run
;;     (make-instance 'core-cps-stream
;; 		   :stack (list ,(expand-cps (walk-form `(lambda (k) ,@body)) #'expand-cps)))))

;; (with-kol/cc (list 1 2 3))

;; (defmacro eskeyp (value)
;;  `(throw 'done ,value))

;; (with-kol/cc
;;   (eskeyp (list 3 2 1))
;;   (list 1 2 3))

;; (defmacro with-kol/cc (&body body)
;;   `(catch 'done
;;      (let ((thunk ,(expand-cps (walk-form `(lambda () ,@body)) #'expand-cps)))
;;        (loop (setq thunk (funcall thunk))))))

;; (with-kol/cc  
;;   (throw 'done 'moo))

;; (defcps-expander constant-form (value)
;;   `(lambda ()
;;      (funcall +k+ ,value)))

;; (defcps-expander variable-reference (name)
;;   `(lambda ()
;;      (funcall +k+ ,name)))

;; (with-kol/cc 1)

;; (defcps-expander let-form (binds declares body)
;;   (let ((bind (gensym)))
;;     `(lambda ()
;;        (let ((+k+ (lambda (,bind)		    
;; 		    (let ((,(caar binds) ,bind))
;; 		      ,@(mapcar (rcurry expand expand) body)))))
;; 	 (funcall +k+ ,(unwalk-form (cdar binds)))))))

;; (with-kol/cc
;;   (let ((a 'a))
;;     a))

;; (defmacro let/ccc (k &body body)
;;   `(funcall (lambda (,k) ,@body) +k+))

;; (with-kol/cc
;;   (let ((a 1))
;;     (let/ccc k
;;       (funcall k a))))



;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (defvar +cps-macros+ (make-hash-table))
;;   (defvar +cps-functions+ (make-hash-table)))

;; (defmacro defcpsmacro (name args &body body)
;;   `(setf (gethash ',name +cps-macro+) 
;; 	 (lambda (k ,@args)
;; 	   ,@body)))

;; (defcpsmacro let/ccc (k1 &body body)
;;   `(list ,k1 ,@body))

;; (defmethod expand-cps ((form t) expand env k)
;;   form)

;; (defcps-expander form ()
;;   form)

;; (defcps-expander constant-form (value)
;;   (if k
;;       `(funcall ,k ,value)
;;       value))

;; (defcps-expander variable-reference (name)
;;   (if k
;;       `(funcall ,k ,name)
;;       name))

;; (defun/ccc def (a)
;;   (+ a 1))

;; (defun/ccc abc (a b)
;;   (+ a b))


;; ;; heyoo
;; ;; (fact1 (- m a) (* m a))
;; ;; =>
;; ;; (-- m a (lambda (t13)
;; ;; 	  (** m a (lambda (t14)
;; ;; 		    (fact1 t13 t14 k)))))

;; (defcps-expander application-form (operator arguments)  
;; ;;;   (reduce (lambda (acc arg)
;; ;;; 	    (funcall expand (car arg) expand env `(lambda (,(cdr arg)) (funcall ,acc ,(cdr arg)))))	    
;; ;;; 	  (mapcar (lambda (arg)
;; ;;; 		    (cons arg (gensym "ARG-")))
;; ;;; 		  (reverse arguments))
;; ;;; 	  :initial-value k)
;; ;;;   ;; 	  arguments)
;; ;;;   ;;   (let ((temp-args (mapcar (lambda (a) (gensym "ARG-")) (seq (length arguments)))))
;; ;;;   ;;     `((lambda (,@temp-args)
;; ;;;   ;; 	(,operator ,@temp-args))
;; ;;;   ;;       ,@(mapcar (rcurry expand expand env k) arguments)))
;; ;;;   ;; (if (null (cadr (multiple-value-list (gethash operator +cps-functions+))))
;; ;;;   ;;       `(,operator ,@(mapcar #'unwalk-form arguments))
;;         (labels ((loop1 (x y z)
;;   		  (if (null x)
;;   		      (do ((F (reverse (cons k y))
;;   			      (if (null (car z)) F
;;   				  (funcall expand (car z) expand
;;   					   env `(lambda (,(car y)) ,F))))
;;   			   (y y (cdr y))
;;   			   (z z (cdr z)))
;;   			  ((null z) f))
;;   		      (cond
;;   			((or (null (car x)) (atom (car x)))
;;   			 (loop1
;;   			    (cdr x)
;;   			    (cons (funcall expand (car x) expand env nil) Y)
;;   			    (cons nil z)))
;;   			((eq (caar x) 'quote)
;;   			 (loop1 (cdr x) (cons (car x) y) (cons nil z)))
;;   			((eq (caar x) 'lambda)
;;   			 (loop1
;;   			    (cdr x)
;;   			    (cons (funcall expand (car x) expand env nil) y)
;;   			    (cons nil z)))
;;   			(t
;;   			 (loop1 (cdr x)
;;   			    (cons (gensym "'T") Y)
;;   			    (cons (car x) z)))))))
;;   	(format t "gee!~%")
;;   	(loop1 (mapcar (lambda (a) (if (typep a 'form) (unwalk-form a) a))
;;   		       (cons operator arguments)) nil nil)))
;; ;;)

;; ;; (defcps-expander lambda-application-form (operator arguments)
;; ;;   )

;; (defcps-expander lambda-function-form (arguments body declares)
;;   (let ((arguments (mapcar #'unwalk-form arguments)))
;;     ((lambda (CN)
;;        ((lambda (LX) (if k `(funcall ,k ,lx) lx))
;; 	`(lambda (,@arguments ,CN)
;; 	   ,@declares
;; 	   ,@(mapcar (rcurry expand expand (append arguments (cons CN env)) CN) body))))
;;      (intern "KONT"))))

;; (defcps-expander function-argument-form (name)
;;   name)

;; ;; (defcps-expander specialized-function-argument-form (name specializer)
;; ;;   )

;; ;; (defcps-expander optional-function-argument-form (name default-value supplied-p-parameter)
;; ;;   )

;; ;; (defcps-expander keyword-function-argument-form (keyword-name name default-value supplied-p-parameter)
;; ;;   )

;; ;; (defcps-expander allow-other-keys-function-argument-form ()
;; ;;   (error "bune-olm-allow-other-keys-falan"))

;; ;; (defcps-expander rest-function-argument-form (name)
;; ;;   )

;; ;; (defcps-expander declaration-form ()
;; ;;   )

;; ;;;; BLOCK/RETURN-FROM
;; ;;  `(block ,name ,@(unwalk-forms body))
;; ;; (defcps-expander block-form (name body)
;; ;;   )

;; ;; ;;  `(return-from ,(name target-block) ,(unwalk-form result))
;; ;; (defcps-expander return-from-form (target-block result)
;; ;;   )

;; ;; ;;;; CATCH/THROW
;; ;; ;;  `(catch ,(unwalk-form tag) ,@(unwalk-forms body))
;; ;; (defcps-expander catch-form (tag body)
;; ;;   )

;; ;; ;; `(throw ,(unwalk-form tag) ,(unwalk-form value))
;; ;; (defcps-expander throw-form (tag value)
;; ;;   )

;; ;; ;;;; EVAL-WHEN
;; ;; (defcps-expander eval-when-form (body eval-when-times)
;; ;;   )

;; ;;;; IF
;; (defcps-expander if-form (consequent then arnesi::else)
;;   ((lambda (KN)
;;      `((lambda (,KN)
;; 	 ,(funcall expand consequent
;; 		   expand env
;; 		   ((lambda (PN)
;; 		      `(lambda (,PN)
;; 			 (if ,PN
;; 			     ,(funcall expand then expand env KN)
;; 			     ,(funcall expand arnesi::else expand env KN))))
;; 		    (gensym "KONT-"))))
;;        ,k))
;;    (gensym "KONT-")))

;; ;;;; FLET/LABELS

;; ;; The cdadr is here to remove (function (lambda ...)) of the function
;; ;; bindings.
;; ;; (flet ((unwalk-flet (binds)
;; ;; 	   (mapcar #'(lambda (bind)
;; ;; 		       (cons (car bind)
;; ;; 			     (cdadr (unwalk-form (cdr bind)))))
;; ;; 		   binds)))
;; ;;     `(flet ,(unwalk-flet binds)
;; ;;        ,@(unwalk-declarations declares)
;; ;;        ,@(unwalk-forms body)))
;; (defcps-expander flet-form (binds body declares)
;;   )

;; (defcps-expander labels-form (binds body declares)
;;   (let ((env (append (mapcar #'car binds) env)))
;;     `(labels ,(reduce (lambda (acc bind)
;; 			(cons (cons (car bind)
;; 				    (cdr (funcall expand (cdr bind) expand env nil)))
;; 			      acc))
;; 		      binds :initial-value nil)
;;        ,@(mapcar (rcurry expand expand env k) body))))

;; ;;;; LET/LET*
;; (defcps-expander let-form (binds body declares)
;;   (funcall expand
;; 	   (walk-form
;; 	    `((lambda (,@(mapcar #'car binds))
;; 		,@(mapcar #'unwalk-form declares)
;; 		,@(mapcar (compose #'unwalk-form (rcurry expand expand k)) body))
;; 	      ,@(mapcar (compose #'unwalk-form #'cdr) binds)))
;; 	   expand env k))

;; ;; (defcps-expander let*-form (binds body declares)
;; ;;   )

;; ;;;; LOAD-TIME-VALUE
;; (defcps-expander arnesi::load-time-value-form (value read-only-p)
;;   )

;; ;;;; LOCALLY
;; (defcps-expander locally-form (body declares)
;;   )

;; ;;;; MACROLET
;; (defcps-expander macrolet-form (body binds declares)
;;   ;; We ignore the binds, because the expansion has already taken
;;   ;; place at walk-time.
;;   )

;; ;;;; MULTIPLE-VALUE-CALL
;; (defcps-expander multiple-value-call-form (func arguments)
;;   (error "unimplemented:multiplave-value-call-form"))

;; ;;;; MULTIPLE-VALUE-PROG1
;; (defcps-expander multiple-value-prog1-form (first-form other-forms)
;;   (error "unimplemented:multiple-value-prog1-form"))

;; ;;;; PROGN
;; (defcps-expander progn-form (body)
;;   `(progn
;;      ,@(mapcar (rcurry expand expand env k) body)))

;; ;;;; PROGV
;; ;; (defunwalker-handler progv-form (body vars-form values-form)
;; ;;   `(progv ,(unwalk-form vars-form) ,(unwalk-form values-form) ,@(unwalk-forms body)))

;; ;;;; SETQ
;; (defcps-expander setq-form (var value)
;;   )

;; ;;;; SYMBOL-MACROLET
;; ;; We ignore the binds, because the expansion has already taken
;; ;; place at walk-time.
;; ;;;   (declare (ignore binds))
;; ;;;   `(locally ,@(unwalk-declarations declares) ,@(unwalk-forms body))
;; (defcps-expander symbol-macrolet-form (body binds declares)
;;   )

;; ;;;; TAGBODY/GO
;; ;;   `(tagbody ,@(unwalk-forms body))
;; (defcps-expander tagbody-form (body)
;;   )

;; (defcps-expander go-tag-form (name)
;;   )

;; ;;`(go ,name)
;; (defcps-expander go-form (name)
;;   )

;; ;;;; THE
;; ;;`(the ,type-form ,(unwalk-form value))
;; (defcps-expander the-form (type-form value)
;;   )

;; ;;;; UNWIND-PROTECT
;; (defcps-expander unwind-protect-form (protected-form cleanup-form)
;;   )

;; (defcps-expander implicit-progn-mixin (body)
;;   )

;; (defcps-expander progn-form (body)
;;   `(progn
;;      ,@(mapcar (rcurry expand expand env k) body)))

;; (defcps-expander throw-form (tag value)
;;   )

;; (defmacro cps (&body body)
;;   `(expand-cps (walk-form `(progn ,',@body)) #'expand-cps nil 'k))

;; (defmacro defun/ccc (name args &body body)
;;   (progn
;;     (eval-when (:compile-toplevel :load-toplevel :execute)
;;       (setf (gethash name +cps-functions+) args))
;;     (let ((k (intern "KONT")))
;;       `(progn
;; 	 (defun ,name (,@args ,k)
;; 	   ,@(mapcar (rcurry #'expand-cps #'expand-cps args k) body))))))

;; (defmacro with-call/ccc (&body body)
;;   (let ((result (expand-cps (walk-form `(progn ,@body)) #'expand-cps nil '(lambda (x) x))))
;;     `(list ,result)))

;; (defun/ccc abc ()
;;   1)

