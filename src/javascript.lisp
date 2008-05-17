(in-package :core-server)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *javascript-macros* (make-hash-table))
  (defvar *javascript-infix* (make-hash-table))
  (defvar *javascript-syntax* (make-hash-table))
  (export 'while)
  (export 'regex)
  (export '--)
  (export 'create)
  (export 'typeof)
  (export 'with)
  (export 'doeach))

(defmacro defjsinfix (name &optional (js-operator nil))
  `(setf (gethash ',name *javascript-infix*) ',(or js-operator name)))

(defjsinfix +)
(defjsinfix -)
(defjsinfix *)
(defjsinfix /)
(defjsinfix >)
(defjsinfix >=)
(defjsinfix <)
(defjsinfix <=)
(defjsinfix = ==)
(defjsinfix eq ===)
(defjsinfix eql ===)
(defjsinfix equal ===)

(defmacro defjssyntax (name args &body body)
  `(setf (gethash ',name *javascript-syntax*)
	 #'(lambda (arguments expander)
	     (declare (ignorable expander))
	     (destructuring-bind ,args arguments
	       ,@body))))

(defjssyntax aref (target slot)
  `(:and ,(funcall expander target expander)
	 "[" ,(funcall expander slot expander) "]"))

(defjssyntax incf (arg)
  `(:and "++" ,(funcall expander arg expander)))

(defjssyntax decf (arg)
  `(:and "--" ,(funcall expander arg expander)))

(defjssyntax ++ (arg)
  `(:and ,(funcall expander arg expander) "++"))

(defjssyntax -- (arg)
  `(:and ,(funcall expander arg expander) "--"))

(defjssyntax not (arg)
  (if (and (typep arg 'application-form)
	   (js-infix-op-p (operator arg)))
      `(:and "!(" ,(funcall expander arg expander) ")")
      `(:and "!" ,(funcall expander arg expander))))

(defjssyntax typeof (arg)
  `(:and "typeof " ,@(mapcar (rcurry expander expander) arg)))

(defjssyntax return (arg)
  `(:and "return " ,@(mapcar (rcurry expander expander) arg)))

(defjssyntax delete (arg)
  `(:and "delete " ,(funcall expander arg expander)))

(defjssyntax defvar (var value &optional doc)
  (declare (ignore doc))
  `(:and "var " ,(funcall expander var expander)
	 " = " ,(funcall expander value expander)))

(defjssyntax while (consequent &rest body)
  `(:and "while (" ,(funcall expander consequent expander) ") {" #\Newline
	 (:sep ,(format nil ";~%") ',(mapcar (rcurry expander expander) body))
	 ";" #\Newline "}"))

(defjssyntax regex (expression)
  (format nil "~A" (unwalk-form expression)))

(defjssyntax new (expression)
  `(:and "new " ,(funcall expander expression expander)))

;; TODO: Below syntax should go into walker!
;; TODO: Rewrite lisp2 code walker, aha, it shoudl be trivial.
;; -evrim
(defjssyntax create (&rest arguments)
  `(:and "{ " (:sep (format nil ",~%")
		    ',(reverse
		       (mapcar (lambda (b a)
				 `(:and
				   ,(funcall expander a expander)
				   " : "
				   ,(funcall expander b expander)))
			       (reduce (lambda (acc atom)
					 (if (oddp (position atom arguments))
					     (cons atom acc)
					     acc))
				       arguments :initial-value nil)
			       (reduce (lambda (acc atom)
					 (if (evenp (position atom arguments))
					     (cons atom acc)
					     acc))
				       arguments :initial-value nil))))
	 " }"))

(defjssyntax with (object &rest body)
  `(:and "with ("
	 ,(funcall expander object expander)
	 ") {" #\Newline
	 ,(funcall expander (walk-form-no-expand
			     `(progn
				,@(unwalk-forms body)))
		   expander)
	 #\Newline "}"))

(defjssyntax doeach (iterator &rest body)
  `(:and "for (var " ,(symbol-to-js (operator iterator))
	 " in " ,(funcall expander (car (arguments iterator)) expander)
	 ") {" #\Newline
	 ,(funcall expander (walk-form-no-expand
			     `(progn
				,@(unwalk-forms body)))
		   expander)
	 #\Newline "}"))

(defjssyntax switch (&rest arguments)
  `(:and "switch ("
	 ,(funcall expander (car arguments) expander)
	 ") {" #\Newline
	 ,@(mapcar (lambda (option)
		     (describe option)
		     `(:and ,(funcall expander
				      (if (eql 'default (operator option))
					  "default"
					  (operator option))
				      expander)
			    ,(funcall expander
				      (walk-form-no-expand
				       `(progn
					  ,@(unwalk-forms (arguments option))))
				      expander)))
		   (cdr arguments))))

(defjssyntax slot-value (object slot)
  `(:and ,(funcall expander object expander) "." ,(funcall expander slot expander)))

(defmacro defjsmacr0 (name args &body body)
  (with-unique-names (rest)
    `(setf (gethash ',name *javascript-macros*)
	   #'(lambda (&rest ,rest)
	       (destructuring-bind ,args ,rest
		 ,@body)))))

(defjsmacr0 1+ (arg)
  `(+ ,arg 1))

(defjsmacr0 when (a &rest b)
  `(if ,a (progn ,@b)))

(defjsmacr0 unless (a &rest b)
  `(if (not ,a) (progn ,@b)))

(defjsmacr0 array (&rest a)
  `(quote ,a))

(defjsmacr0 make-array (&rest a)
  `(new (*array ,@a)))

(defjsmacr0 list (&rest arg)
  `(new (*array ,@arg)))

(defjsmacr0 setf (&rest rest)
  `(setq ,@rest))

(defmacro defjavascript-expander (class (&rest slots) &body body)
  `(defmethod expand-javascript ((form ,class) (expand function))
     (declare (ignorable expand))
     (format t "inside ~A~%" ',class)
     (with-slots ,slots form
       ,@body)))

(defjavascript-expander form ()
  form)

(defjavascript-expander constant-form (value)
  (typecase value
    (string
     (format nil "'~A'" value))
    (list
     `(:and "[ " (:sep ", " ',(mapcar (lambda (v)
				       (symbol-to-js (format nil "~S" v)))
				     value))  " ]"))
    (symbol (symbol-to-js value))
    (t (format nil "~A" value))))

(defjavascript-expander variable-reference (name)
  (cond
    ((equal 't name) "true")
    ((equal 'nil name) "null")
    (t (symbol-to-js name))))

(defun js-infix-op-p (operator) (if (gethash operator *javascript-infix*) t))

(defjavascript-expander application-form (operator arguments)
;;  (describe form)
  (acond   
   ((gethash operator *javascript-syntax*) (funcall it arguments expand))
   ((gethash operator *javascript-infix*)
    (if (and (not (null (parent form)))
	     (typep (parent form) 'application-form)
	     (gethash (operator (parent form)) *javascript-infix*))
	`(:and "(" (:sep ,(format nil " ~A " (symbol-name it))
			 ',(mapcar (rcurry expand expand) arguments)) ")")     
	`(:sep ,(format nil " ~A " (symbol-name it))
	       ',(mapcar (rcurry expand expand) arguments))))
   ((gethash operator *javascript-macros*)
    (funcall expand (walk-form-no-expand (apply it (mapcar #'unwalk-form arguments))) expand))
   (t
    `(:and ,(symbol-to-js operator) "("
	   (:sep ", " ',(mapcar (rcurry expand expand) arguments))
	   ")"))))

(defjavascript-expander lambda-application-form (operator arguments)
  `(:and ;;    "("
	 ,(funcall expand operator expand) 
	 "(" (:sep "," ',(mapcar (rcurry expand expand) arguments)) ");"))

(defjavascript-expander lambda-function-form (arguments body declares)
  `(:and "function (" 
	 (:sep ", "
	       ',(mapcar (rcurry expand expand) arguments))
	 ") {" #\Newline
	 (:sep (format nil ";~%")
	       ',(mapcar (rcurry expand expand) body))
	 ";" #\Newline "}"))

(defjavascript-expander function-argument-form (name)
  (symbol-to-js name))

(defjavascript-expander specialized-function-argument-form (name specializer)
  (call-next-method))

(defjavascript-expander optional-function-argument-form (name default-value supplied-p-parameter)
  (call-next-method))

(defjavascript-expander keyword-function-argument-form (keyword-name name default-value supplied-p-parameter)
  (call-next-method))

(defjavascript-expander allow-other-keys-function-argument-form ()
  (error "bune-olm-allow-other-keys-falan"))

(defjavascript-expander rest-function-argument-form (name)
  (call-next-method))

(defjavascript-expander declaration-form ()
  nil)

;;;; BLOCK/RETURN-FROM
;;  `(block ,name ,@(unwalk-forms body))
(defjavascript-expander block-form (name body)
  `(:and "{" ,@(mapcar (rcurry expand expand) body) "}"))

(defjavascript-expander return-from-form (target-block result)
  ;;  `(return-from ,(name target-block) ,(unwalk-form result))
  (error "cant do return-from"))

;;;; CATCH/THROW
;;  `(catch ,(unwalk-form tag) ,@(unwalk-forms body))
(defjavascript-expander catch-form (tag body)
  `(:and "try {" ,@(mapcar (rcurry expand expand) body) "}"))

;; `(throw ,(unwalk-form tag) ,(unwalk-form value))
(defjavascript-expander throw-form (tag value)
  `(:and "throw " ,value))

;;;; EVAL-WHEN
(defjavascript-expander eval-when-form (body eval-when-times)
  nil)

;;;; IF
(defjavascript-expander if-form (consequent then arnesi::else)
  (if (typep (parent form) 'application-form)
      `(:and "(" ,(funcall expand consequent expand)
	" ? " ,(funcall expand then expand)
	" : " ,(if arnesi::else
		   (funcall expand arnesi::else expand)
		   "null") ")")
      `(:and
	"if "
	"(" ,(funcall expand consequent expand) ")"
;;; 	,(if (typep consequent 'constant-form)
;;; 	     `(:and "(" (funcall expand consequent expand) ")")
;;; 	     (funcall expand consequent expand))
	" {" #\Newline
	,(funcall expand then expand)
	,(if (not (typep then 'progn-form))
	     ";")
	#\Newline "}"
	,@(if arnesi::else
	      `(" else {"
		#\Newline
		,(funcall expand arnesi::else expand)
		,(if (not (typep arnesi::else 'progn-form))
		     ";")
		#\Newline
		"}")))))

;;;; FLET/LABELS

;; The cdadr is here to remove (function (lambda ...)) of the function
;; bindings.
;; (flet ((unwalk-flet (binds)
;; 	   (mapcar #'(lambda (bind)
;; 		       (cons (car bind)
;; 			     (cdadr (unwalk-form (cdr bind)))))
;; 		   binds)))
;;     `(flet ,(unwalk-flet binds)
;;        ,@(unwalk-declarations declares)
;;        ,@(unwalk-forms body)))
(defjavascript-expander flet-form (binds body declares)
  `(:and
    ,@(mapcar (lambda (bind)
		`(:and "var " ,(symbol-to-js (car bind))
		       "=" ,(funcall expand (cdr bind) expand)
		       ";" #\Newline))
	      binds)
    (:sep ,(format nil ";~%")
	  ',(mapcar (rcurry expand expand) body))
    ;;    ";"
    ))

(defjavascript-expander labels-form (binds body declares)
  `(:and
    ,@(mapcar (lambda (bind)
		`(:and "var " ,(symbol-to-js (car bind))
		       "=" ,(funcall expand (cdr bind) expand)
		       ";" #\Newline))
	      binds)
    (:sep ,(format nil ";~%")
	  ',(mapcar (rcurry expand expand) body))
    ;;    ";"
    ))

;;;; LET/LET*
(defjavascript-expander let-form (binds body declares)
  `(:and 
    ,@(mapcar (lambda (bind)
		`(:and "var " ,(symbol-to-js (car bind))
		       " = " ,(funcall expand (cdr bind) expand)
		       ";" #\Newline))
	      binds)
    (:sep ,(format nil ";~%")
	  ',(mapcar (rcurry expand expand) body))
    ;;    ";"
    ))

(defjavascript-expander let*-form (binds body declares)
  `(:and
    ,@(mapcar (lambda (bind)
		`(:and "var " ,(symbol-to-js (car bind))
		       " = " ,(funcall expand (cdr bind) expand)
		       ";" #\Newline))
	      binds)
    (:sep ,(format nil ";~%")
	  ',(mapcar (rcurry expand expand) body))
    ;; ";"
    ))

;;;; LOAD-TIME-VALUE
(defjavascript-expander arnesi::load-time-value-form (value read-only-p)
  nil)

;;;; LOCALLY
(defjavascript-expander locally-form (body declares)
  `(:and ,@(mapcar (rcurry expand expand) body)))

;;;; MACROLET
(defjavascript-expander macrolet-form (body binds declares)
  ;; We ignore the binds, because the expansion has already taken
  ;; place at walk-time.
  `(:and ,@(mapcar (rcurry expand expand) body)))

;;;; MULTIPLE-VALUE-CALL
(defjavascript-expander multiple-value-call-form (func arguments)
  (error "unimplemented:multiplave-value-call-form"))

;;;; MULTIPLE-VALUE-PROG1
(defjavascript-expander multiple-value-prog1-form (first-form other-forms)
  (error "unimplemented:multiple-value-prog1-form"))

;;;; PROGN
(defjavascript-expander progn-form (body)
  `(:and "{" #\Newline
	 (:sep ,(format nil ";~%")
	       ',(mapcar (rcurry expand expand) body))
	 ";" #\Newline "}"))

;;;; PROGV
;; (defunwalker-handler progv-form (body vars-form values-form)
;;   `(progv ,(unwalk-form vars-form) ,(unwalk-form values-form) ,@(unwalk-forms body)))

;;;; SETQ
(defjavascript-expander setq-form (var value)
  `(:and ,(symbol-to-js var) "=" ,(funcall expand value expand)))

;;;; SYMBOL-MACROLET
;; We ignore the binds, because the expansion has already taken
;; place at walk-time.
;;;   (declare (ignore binds))
;;;   `(locally ,@(unwalk-declarations declares) ,@(unwalk-forms body))
(defjavascript-expander symbol-macrolet-form (body binds declares)
  (error "unimplemented: symbol-macrolet-form"))

;;;; TAGBODY/GO
;;   `(tagbody ,@(unwalk-forms body))
(defjavascript-expander tagbody-form (body)
  (error "unimplemented: tagbody"))

(defjavascript-expander go-tag-form (name)
  (error "unimplemented: go-tag-form"))

;;`(go ,name)
(defjavascript-expander go-form (name)
  (error "unimplemented: go-form"))

;;;; THE
;; TODO: Value is evaluated twice.
;; No problem if value is a reference or a constant.
;; Aslinda buraya birsey uydur durumu var sanirim biraz
;; -evrim
(defjavascript-expander the-form (type-form value)
  `(:and "(typeof " ,(funcall expand value expand)
	 " == " ,(format nil "~A" type-form) ")"
	 " ? " ,(funcall expand value expand)
	 " : " ,(format nil "throw new Error('~A is not typeof ~A')"
			(unwalk-form value) type-form)))

;;;; UNWIND-PROTECT
(defjavascript-expander unwind-protect-form (protected-form cleanup-form)
  (funcall expand
	   (walk-form-no-expand
	    `(try ))) (error "unimplemented: unwind-protect-form"))

(defjavascript-expander implicit-progn-mixin (body)
  `(:and
    (:sep ,(format nil "; ~%")
	  ',(mapcar (rcurry expand expand) body))
    ";"))

(defjavascript-expander progn-form (body)
  (if (typep (parent form) 'application-form)
      `(:and "(" (:sep ", " ',(mapcar (rcurry expand expand) body)) ")")
      `(:and
	(:sep ,(format nil ";~%")
	      ',(mapcar (rcurry expand expand) body))
	";")))

(defjavascript-expander throw-form (tag value)
  `(:and "throw " ,(funcall expand tag expand)))

(defjavascript-expander dotimes-form (var how-many body)    
  `(:and "for (var " ,(symbol-to-js var)
	 " = 0; " ,(symbol-to-js var)
	 " < " ,(funcall expand how-many expand)
	 "; " ,(symbol-to-js var) " = " ,(symbol-to-js var)
	 " + 1) {" #\Newline
	 ,(funcall expand (walk-form-no-expand
			     `(progn
				,@(unwalk-forms body)))
		   expand)
	 #\Newline "}"))

(defjavascript-expander dolist-form (var lst body)
  `(:and "for (var " ,(symbol-to-js var)
	 " = 0; " ,(symbol-to-js var)
	 " < " ,(funcall expand lst expand)
	 "; " ,(symbol-to-js var) " = " ,(symbol-to-js var)
	 " + 1) {" #\Newline
	 ,(funcall expand (walk-form-no-expand
			   `(progn
			      ,@(unwalk-forms body)))
		   expand)
	 #\Newline "}"))

(defjavascript-expander defun-form (name arguments declares body)
  `(:and "function " ,(symbol-to-js name) "("
	 (:sep ", "
	       ',(mapcar (rcurry expand expand) arguments))
	 ") {" #\Newline
	 (:sep (format nil ";~%")
	       ',(mapcar (rcurry expand expand) body))
	 ";" #\Newline "}"))

(defmacro +js+ (&body body)
  `(with-core-stream (s "")
     (funcall (lambda ()
		(block rule-block		  
		  ,(expand-render
		    (walk-grammar
		     `(:and ,@(mapcar (rcurry #'expand-javascript #'expand-javascript)
				      (mapcar #'walk-form-no-expand body))))
		    #'expand-render 's))))
     (return-stream s)))

(defmacro <:js+ (&body body)
  (with-unique-names (output)    
    `(let ((,output (if +context+ (http-response.stream (response +context+)) *core-output*)))
       (funcall (lambda ()
		  (block rule-block
		    ,(expand-render
		      (walk-grammar
		       `(:and ,@(mapcar (rcurry #'expand-javascript #'expand-javascript)
					(mapcar #'walk-form-no-expand (cdar body)))))
		      #'expand-render output)
		    nil))))))

(defmacro defun/javascript (name params &body body)
  `(prog1
     (defun ,name ,params ,@body)
     (defun ,(intern (string-upcase (format nil "~A!" name))) (s ,@params)
       (block rule-block		  
	 ,(expand-render (walk-grammar
			  (expand-javascript
			   (walk-form-no-expand
			    `(setf ',name
				   (lambda ,params
				     ,@body)))
			   #'expand-javascript))
			 #'expand-render 's)))))

;; (defun/javascript fun1 ()
;;   (setf fun1 (lambda (a b c) (list a b c))))

;; (js+ 
;;   (+ 1 1)
;;   (+ 2 2))
