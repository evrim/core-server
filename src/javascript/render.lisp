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
;;| [Core-serveR] Javascript Renderer
;;+-----------------------------------------------------------------------------

;; Hash tables that hold macros, infix and syntax operators
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *javascript-macros* (make-hash-table))
  (defvar *javascript-infix* (make-hash-table))
  (defvar *javascript-syntax* (make-hash-table)))

;;-----------------------------------------------------------------------------
;; Javascript Infix Operators
;;-----------------------------------------------------------------------------
(defmacro defjsinfix (name &optional (js-operator nil))
  "Define a 'Javascript Infix' operator"
  `(setf (gethash ',name *javascript-infix*) ',(or js-operator name)))

(defjsinfix +)
(defjsinfix -)
(defjsinfix *)
(defjsinfix /)
(defjsinfix >)
(defjsinfix >=)
(defjsinfix <)
(defjsinfix <=)
(defjsinfix = "==")
(defjsinfix eq "===")
(defjsinfix eql "===")
(defjsinfix equal "===")
(defjsinfix or "||")
(defjsinfix and "&&")

(defun seperated-by (seperator args)  
  `(:and
    ,@(nreverse
       (reduce (lambda (acc arg)
		 (cons arg
		       (cons seperator acc)))
	       (cdr args) :initial-value (list (car args))))))

(defun s-b (seperator args)
  (seperated-by seperator args))
;;-----------------------------------------------------------------------------
;; Javascript Syntax operators
;; These are used to make the library more parenscript friendly
;; and to cover javascript hacks.
;;-----------------------------------------------------------------------------
(defmacro defjssyntax (name args &body body)
  "Define a 'Javascript Syntax' operator"
  `(setf (gethash ',name *javascript-syntax*)
	 #'(lambda (arguments expander)
	     (declare (ignorable expander))
	     (destructuring-bind ,args arguments
	       ,@body))))

(defjssyntax array (&rest a)
  `(:and "[ " ,(s-b ", " (mapcar (rcurry expander expander) a))
	 " ]"))

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
  `(:and "typeof " ,(funcall expander arg expander)))

(defjssyntax return (arg)
  `(:and "return " ,(funcall expander arg expander)))

(defjssyntax delete (arg)
  `(:and "delete " ,(funcall expander arg expander)))

(defjssyntax break ()
  "break")

(defjssyntax defvar (var value &optional doc)
  (declare (ignore doc))
  `(:and "var " ,(funcall expander var expander)
	 " = " ,(funcall expander value expander)))

(defjssyntax while (consequent &rest body)
  `(:and "while (" ,(funcall expander consequent expander) ") {"
	 (:indent) #\Newline
	 ,(s-b (format nil ";~%")
	       (mapcar (rcurry expander expander) body))
	 ";" (:deindent) #\Newline "}"))

(defjssyntax regex (expression)
  (format nil "~A" (unwalk-form expression)))

(defjssyntax new (expression)
  `(:and "new " ,(funcall expander expression expander)))

(defjssyntax create (&rest arguments)
  (if arguments
      `(:and "{" (:indent) #\Newline
	     ,(seperated-by (format nil ",~%")
			    (reverse
			     (mapcar (lambda (b a)
				       `(:and
					 ,(funcall expander a expander)
					 ": "
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
	     (:deindent) #\Newline "}")
      `(:and "{}")))

(defjssyntax with (object &rest body)
  `(:and "with ("
	 ,(funcall expander object expander)
	 ") {" (:indent) #\Newline
	 ,(funcall expander (let ((progn (make-instance 'progn-form :body body)))
			      (mapc (lambda (b) (setf (parent b) progn)) body)
			      progn) expander)
	 (:deindent) #\Newline "}"))

(defjssyntax doeach (iterator &rest body)
  `(:and "for (var " ,(symbol-to-js (operator iterator))
	 " in " ,(funcall expander (car (arguments iterator)) expander)
	 ") {" (:indent) #\Newline
	 ,(funcall expander (make-instance 'progn-form :body body) expander)
	 (:deindent) #\Newline "}"))

(defjssyntax slot-value (object slot)
  (if (and (typep slot 'constant-form) (typep (value slot) 'symbol))
      `(:and ,(funcall expander object expander) "." ,(funcall expander slot expander))
      `(:and ,(funcall expander object expander) "[" ,(funcall expander slot expander) "]")))

(defjssyntax switch (object &rest cases)
  `(:and
    "switch (" ,(funcall expander object expander) ") {"
    (:indent) #\Newline
    ,(seperated-by
      #\Newline
      (mapcar (lambda (case)
		(cond
		  ((or (eq 'default (operator case))
		       (eq 't (operator case)))
		   `(:and "default:"
			  (:indent) #\Newline
			  ,(seperated-by (format nil ";~%")
			    (mapcar (rcurry expander expander) (arguments case)))
			  ";" (:deindent)))
		  ((atom (operator case))
		   `(:and "case " ,(funcall expander (walk-js-form (operator case)) expander) ":"
			  (:indent) #\Newline
			  ,(seperated-by (format nil ";~%")
			    (mapcar (rcurry expander expander) (arguments case)))
			  ";" (:deindent)))
		  (t
		   `(:and
		     ,(seperated-by #\Newline
		       (mapcar (lambda (case)
				 (if (or (eq 'default case)
					 (eq 't case))
				     "default"
				     `(:and
				       "case "
				       ,(funcall expander (walk-js-form case) expander)
				       ":")))
			       (operator case)))
		     (:indent) #\Newline
		     ,(seperated-by (format nil ";~%")
		       (mapcar (rcurry expander expander) (arguments case)))
		     ";" (:deindent)))))
	      cases))
    (:deindent) #\Newline "}"))

;; NOTE: This is very ugly but needed only for backward compat with parenscript
;; I'll try to implement handler-bind, catch for the use of the newer compiler.
;; -evrim.
(defjssyntax try (body &optional catch finally)  
  (let ((body (walk-js-form (unwalk-form body)))) ;; Hack to make progn work.
    `(:and "try {" (:indent) #\Newline	   
	   ,(funcall expander body expander)
	   (:deindent) #\Newline "}"
	   ,(when catch
		  (assert (eq :catch (operator catch)))
		  (let* ((op (operator (car (arguments catch))))
			 (catch (walk-js-form
				 `(progn
				    ,@(unwalk-forms (cdr (arguments catch)))))))
		    `(:and " catch ("
			   ,(symbol-to-js op)
			   ") {" (:indent) #\Newline
			   ,(funcall expander catch expander)
			   (:deindent) #\Newline "}")))
	   ,(when finally
		  (assert (eq :finally (operator finally)))
		  (let ((finally (walk-js-form `(progn ,@(unwalk-forms (arguments finally))))))
		    `(:and " finally {" (:indent) #\Newline
			   ,(funcall expander finally expander)
			   (:deindent) #\Newline "}"))))))

;;-----------------------------------------------------------------------------
;; Javascript macros
;;-----------------------------------------------------------------------------
(defmacro defjsmacro (name args &body body)
  "Define a 'Javascript macro' operator"
  (with-unique-names (rest)
    `(setf (gethash ',name *javascript-macros*)
	   #'(lambda (&rest ,rest)
	       (destructuring-bind ,args ,rest
		 ,@body)))))

(defjsmacro 1+ (arg)
  `(+ ,arg 1))

(defjsmacro when (a &rest b)
  `(if ,a (progn ,@b)))

(defjsmacro unless (a &rest b)
  `(if (not ,a) (progn ,@b)))

(defjsmacro make-array (&rest a)
  `(new (*array ,@a)))

(defjsmacro list (&rest arg)
  `(new (*array ,@arg)))

(defjsmacro setf (&rest rest)
  (assert (evenp (length rest)))
  `(progn
     ,@(let (state)
	    (nreverse
	     (reduce (lambda (acc atom)
		       (if (null state)
			   (prog1 acc (setf state atom))
			   (prog1 (cons `(setq ,state ,atom) acc)
			     (setq state nil))))
		     rest :initial-value nil)))))

(defjsmacro case (object &rest cases)
  `(switch ,object
     ,@(mapcar (lambda (case)
		 (append case (list 'break)))
	       (butlast cases))
     ,(last1 cases)))

(defjsmacro with-slots (slots object &body body)
  `(let ,(mapcar (lambda (slot)
		   (list slot `(slot-value ,object ',slot)))
		 slots)
     ,@body))

(defun macroexpand-js-1 (form &optional env)
  (declare (ignore env))
  (aif (and (listp form) (gethash (car form) *javascript-macros*))
       (values (apply it (cdr form)) t)
       (values form nil)))

(defun walk-js-form (form &optional (parent nil) (env (make-walk-env)))
  (let ((arnesi::*walker-expander* #'macroexpand-js-1))
    (walk-form form parent env)))

;;-----------------------------------------------------------------------------
;; Javascript expanders for Lisp2 Forms
;;-----------------------------------------------------------------------------
;;
;; Note: These are used along with walk-js-form
;;
(defmacro defjavascript-expander (class (&rest slots) &body body)
  "Define a javascript expander (ie. unwalker) which is fed to renderer"
  `(defmethod expand-javascript ((form ,class) (expand function))
     (declare (ignorable expand))
;;     (format t "inside ~A~%" ',class)
     (with-slots ,slots form
       ,@body)))

(defjavascript-expander form ()
  (call-next-method))

(defjavascript-expander constant-form (value)
  (if (eq value t)
      "true"
      (typecase value
	(string (format nil "'~A'" value))
	(null "null")
	(symbol (symbol-to-js value))
	(list
	 `(:and "[ " ,(seperated-by ", "
				    (mapcar (lambda (v)
					      (symbol-to-js (format nil "~S" v)))
					    value))  " ]"))
	(t (format nil "~A" value)))))

(defjavascript-expander variable-reference (name)
  (cond
    ((eq 't name) "true")
    ((eq 'nil name) "null")
    (t (symbol-to-js name))))

(defvar +js-free-variables+ nil)
(defjavascript-expander free-variable-reference (name)
  (cond
    ((or (member name +js-free-variables+) (boundp name))
     `(:json! ,name))
    (t (call-next-method))))

(defjavascript-expander implicit-progn-mixin (body)
  `(:and "{"
	 (:indent) #\Newline
	 ,(seperated-by (format nil ";~%")
	   (mapcar (rcurry expand expand) body))
	 ";" (:deindent) #\Newline "}"))

(defun js-infix-op-p (operator)
  (if (gethash operator *javascript-infix*) t))

(defjavascript-expander application-form (operator arguments)
  (acond   
   ((gethash operator *javascript-infix*)
    (if (and (typep (parent form) 'application-form)
	     (gethash (operator (parent form)) *javascript-infix*))
	`(:and "(" ,(seperated-by (format nil " ~A " (if (symbolp it)
							 (format nil "~A" it)
							 it))
		     (mapcar (rcurry expand expand) arguments)) ")")     
	(seperated-by (format nil " ~A " (if (symbolp it)
					     (format nil "~S" it)
					     it))
	  (mapcar (rcurry expand expand) arguments))))   
   ((char= #\. (aref (symbol-to-js operator) 0))
    `(:and ,(funcall expand (car arguments) expand)
	   ,(symbol-to-js operator) "("
	   ,(seperated-by ", " (mapcar (rcurry expand expand) (cdr arguments)))
	   ")"))
   ((gethash operator *javascript-syntax*) (funcall it arguments expand))
   ((gethash operator *javascript-macros*)
    (error "Error while rendering javascript, js macros should be expanded in walker")
    ;; (funcall expand (walk-js-form
;; 		     (apply it (mapcar #'unwalk-form arguments))
;; 		     (parent form))
;; 	     expand)
    )
   (t
    `(:and ,(symbol-to-js operator) "("
	   ,(seperated-by ", " (mapcar (rcurry expand expand) arguments))
	   ")"))))

(defjavascript-expander lambda-application-form (operator arguments)
  `(:and ,(funcall expand operator expand) 
	 "("
	 ,(seperated-by ", " (mapcar (rcurry expand expand) arguments))
	 ,(if (typep (parent form) 'application-form)
	      ")"
	      ");")))

(defjavascript-expander lambda-function-form (arguments body declares)
  `(:and "function (" 
	 ,(seperated-by ", " (mapcar (rcurry expand expand) arguments))
	 ") " ,(call-next-method)))

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
  (warn "Blocks support only scope, not return")
  (call-next-method))

(defjavascript-expander return-from-form (target-block result)
  ;;  `(return-from ,(name target-block) ,(unwalk-form result))
  (error "cant do return-from"))

;;;; CATCH/THROW
;;  `(catch ,(unwalk-form tag) ,@(unwalk-forms body))
;; This is implicit-progn-mixin
(defjavascript-expander catch-form (tag body)
  (error "Implement catch-form")
  `(:and "try {" ,@(mapcar (rcurry expand expand) body) "}"))

;; `(throw ,(unwalk-form tag) ,(unwalk-form value))
(defjavascript-expander throw-form (tag value)
  `(:and "throw " ,value))

;;;; EVAL-WHEN
(defjavascript-expander eval-when-form (body eval-when-times)
  (call-next-method))

;;;; IF
(defjavascript-expander if-form (consequent then else)
  (if (typep (parent form) 'application-form)
      `(:and "(" ,(funcall expand consequent expand)
	     " ? " ,(funcall expand then expand)
	     " : " ,(if else
			(funcall expand else expand)
			"null") ")")
      `(:and
	"if "
	"(" ,(funcall expand consequent expand) ") {"
	(:indent) #\Newline
	,(funcall expand then expand)
	,(if (not (typep then 'progn-form))
	     ";")
	(:deindent) #\Newline "}"
	,@(if else
	      `(" else {"
		(:indent) #\Newline
		,(funcall expand else expand)
		,(if (not (typep else 'progn-form))
		     ";")
		(:deindent) #\Newline
		"}")))))

;;;; COND
(defjavascript-expander cond-form (conditions)
  (if (typep (parent form) 'application-form)
      `(:and
	,@(nreverse
	   (reduce (lambda (acc atom)
		     (cons `(:and ,(funcall expand (car atom) expand)
				  " ? ("
				  ,(seperated-by ", " (mapcar (rcurry expand expand) (cdr atom)))
				  ") : ")
			   acc))
		   (cdr conditions)
		   :initial-value (list
				   `(:and
				     ,(funcall expand (caar conditions) expand)
				     " ? ("
				     ,(seperated-by ", " (mapcar (rcurry expand expand) (cdar conditions)))
				     ") : "))))
	"null")
      `(:and ,@(nreverse
		(reduce (lambda (acc atom)
			  (cons (if (and (typep (car atom) 'constant-form)
					 (eq 't (value (car atom))))
				    `(:and " else {" (:indent) #\Newline
					   ,(seperated-by (format nil ";~%")
					      (mapcar (rcurry expand expand) (cdr atom)))
					   ";" (:deindent) #\Newline "}")
				    `(:and " else if(" ,(funcall expand (car atom) expand)
					   ") {" (:indent) #\Newline
					   ,(seperated-by (format nil ";~%")
					      (mapcar (rcurry expand expand) (cdr atom)))
					   ";" (:deindent) #\Newline "}"))
				acc))
			(cdr conditions)
			:initial-value (list
					`(:and "if(" ,(funcall expand (caar conditions) expand)
					       ") {" (:indent) #\Newline
					       ,(seperated-by (format nil ";~%")
						  (mapcar (rcurry expand expand) (cdar conditions)))
					       ";" (:deindent) #\Newline "}")))))))

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
    ,(seperated-by (format nil ";~%")
      (mapcar (rcurry expand expand) body))))

(defjavascript-expander labels-form (binds body declares)
  `(:and
    ,@(mapcar (lambda (bind)
		`(:and "var " ,(symbol-to-js (car bind))
		       "=" ,(funcall expand (cdr bind) expand)
		       ";" #\Newline))
	      binds)
    ,(seperated-by (format nil ";~%")
      (mapcar (rcurry expand expand) body))))

;;;; LET/LET*
(defjavascript-expander let-form (binds body declares)
  `(:and 
    ,@(mapcar (lambda (bind)
		`(:and "var " ,(symbol-to-js (car bind))
		       " = " ,(funcall expand (cdr bind) expand)
		       ";" #\Newline))
	      binds)
    ,(seperated-by (format nil ";~%")
      (mapcar (rcurry expand expand) body))))

(defjavascript-expander let*-form (binds body declares)
  `(:and
    ,@(mapcar (lambda (bind)
		`(:and "var " ,(symbol-to-js (car bind))
		       " = " ,(funcall expand (cdr bind) expand)
		       ";" #\Newline))
	      binds)
    ,(seperated-by (format nil ";~%")
       (mapcar (rcurry expand expand) body))))

;;;; LOAD-TIME-VALUE
(defjavascript-expander arnesi::load-time-value-form (value read-only-p)
  value)

;;;; LOCALLY
(defjavascript-expander locally-form (body declares)
  (call-next-method))

;;;; MACROLET
;; FIXmE: Implement macro expansion 
(defjavascript-expander macrolet-form (body binds declares)
  ;; We ignore the binds, because the expansion has already taken
  ;; place at walk-time.
  (error "Fix macrolet-form")
  (call-next-method))

;;;; MULTIPLE-VALUE-CALL
(defjavascript-expander multiple-value-call-form (func arguments)
  (error "unimplemented:multiplave-value-call-form"))

;;;; MULTIPLE-VALUE-PROG1
(defjavascript-expander multiple-value-prog1-form (first-form other-forms)
  (error "unimplemented:multiple-value-prog1-form"))

;;;; PROGV
;; (defunwalker-handler progv-form (body vars-form values-form)
;;   `(progv ,(unwalk-form vars-form) ,(unwalk-form values-form) ,@(unwalk-forms body)))

;;;; SETQ
;;;; TODO: FIX setq walker to walk var (easy)
;;;; TODO: FIX call/cc to unwalk var before going in. -evrim.
(defjavascript-expander setq-form (var value)
  `(:and ,(let ((+js-free-variables+ nil))
	    (funcall expand var expand))
	 " = " ,(funcall expand value expand)))

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
  (error "unimplemented: unwind-protect-form"))

;;;; PROGN
(defjavascript-expander progn-form (body)
  (if (typep (parent form) 'application-form)
      `(:and "(" ,(seperated-by ", " (mapcar (rcurry expand expand) body)) ")")
      `(:and
	,(seperated-by (format nil ";~%")
	  (mapcar (rcurry expand expand) body))
	,(if (not (typep (parent form) 'implicit-progn-mixin))
	     ";"))))

(defjavascript-expander throw-form (tag value)
  `(:and "throw " ,(funcall expand tag expand)))

(defjavascript-expander dotimes-form (var how-many body)    
  `(:and "for (var " ,(symbol-to-js var)
	 " = 0; " ,(symbol-to-js var)
	 " < " ,(funcall expand how-many expand)
	 "; " ,(symbol-to-js var) " = " ,(symbol-to-js var)
	 " + 1) "
	 ,(call-next-method)))

(defjavascript-expander dolist-form (var lst body)
  (let ((i (symbol-to-js (gensym "tmp")))
	(j (symbol-to-js (gensym "tmp"))))
    `(:and "for (var " ,i "=0," ,j "="
	   ,(funcall expand lst expand)
	   "; " ,i " < " ,j ".length; " ,i "++) {"
	   (:indent) #\Newline
	   "var " ,(symbol-to-js var) " = " ,j "[" ,i "];"
	   #\Newline
	   ,(seperated-by (format nil ";~%")
	     (mapcar (rcurry expand expand) body))
	   ";" (:deindent) #\Newline "}")))

(defjavascript-expander do-form (varlist endlist declares body)
  `(:and "for (var "
	 ,(seperated-by ", "
	    (mapcar (lambda (var)
		      `(:and ,(symbol-to-js (car var))
			     "="
			     ,(funcall expand (cadr var) expand)))
		    varlist))
	 "; "
	 "!(" ,(funcall expand (car endlist) expand) "); "
	 ,(seperated-by ", "
	    (mapcar (lambda (var)
		      `(:and ,(symbol-to-js (car var)) "="
			     ,(funcall expand (caddr var) expand)))
		    varlist))
	 ") "
	 ,(call-next-method)))

(defjavascript-expander defun-form (name arguments declares body)
  `(:and "function " ,(symbol-to-js name) "("
	 ,(seperated-by ", "
	    (mapcar (rcurry expand expand) arguments))
	 ") {"
	 (:indent) #\Newline
	 ,(seperated-by (format nil ";~%")
	    (mapcar (rcurry expand expand) body))
	 ";" (:deindent) #\Newline "}"))

;;-----------------------------------------------------------------------------
;; Javascript Functionalization
;;-----------------------------------------------------------------------------
;; This is a preprocessor to solve the return problem of javascript ie:
;;
;; (print ((lambda (a) a) 1))
;;
;; should print 1, but javascript is broken, so we add returns implicitly.
;;
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defgeneric find-form-leaf (form)
    (:documentation "Returns list of termination subforms the 'form'"))

  (defmethod find-form-leaf ((form t))
    form)

  (defmethod find-form-leaf ((form implicit-progn-mixin))
    (find-form-leaf (last1 (body form))))

  (defmethod find-form-leaf ((form if-form))
    (append (ensure-list (find-form-leaf (then form)))
	    (ensure-list (find-form-leaf (else form)))))

  (defun fix-javascript-returns (form)
    "Alters form destructive and adds implicit return statements to 'form'"
    (flet ((transform-to-implicit-return (form)
	     (let ((new-form (walk-form-no-expand (unwalk-form form))))
	       (unless (and (typep form 'application-form)
			    (eq 'return (operator form)))
		 (change-class form 'application-form)
		 (setf (operator form) 'return)
		 (setf (arguments form) (list new-form)))
	       new-form)))
      (let ((funs (ast-search-type form 'lambda-function-form)))
	(mapcar #'transform-to-implicit-return
		(flatten (mapcar #'find-form-leaf funs)))
	form)))

  (defun fix-javascript-methods (form self)
    (let ((applications (ast-search-type form 'application-form)))
      (mapcar (lambda (app)
		(when (and (typep (car (arguments app)) 'variable-reference)
			   (eq (name (car (arguments app))) self))
		  (setf (operator app) (intern (format nil "THIS.~A" (operator app)))
			(arguments app) (cdr (arguments app)))))
	      applications)
      form))

;; TODO: Handle (setf a b c d)
  (defun fix-javascript-accessors (form accessors &optional (get-prefix "GET-") (set-prefix "SET-"))
    (let ((applications (ast-search-type form 'application-form)))
      (mapcar (lambda (app)
		(cond				
		  ((and (eq 'setf (operator app))
			(typep (car (arguments app)) 'application-form)
			(member (operator (car (arguments app))) accessors))
		   (describe app)
		   (setf (operator app) (intern (format nil "~A~A" set-prefix (operator (car (arguments app)))))
			 (arguments app) (cons (car (arguments (car (arguments app))))
					       (cdr (arguments app)))))
		  ((and (member (operator app) accessors)
			(not (and (typep (parent app) 'application-form)
				  (eq 'setf (operator (parent app))))))
		   (setf (operator app) (intern (format nil "~A~A" get-prefix (operator app)))))))
	      applications)
      form)))


;;-----------------------------------------------------------------------------
;; Javascript Render Interface
;; This is the line where fun starts
;;-----------------------------------------------------------------------------
(defmacro +js+ (&body body)
  "Convert body to javascript and return it as a string"
  `(let ((s (make-indented-stream "" 2)))
     (funcall (lambda ()
		(block rule-block		  
		  ,(expand-render
		    (walk-grammar
		     `(:and ,@(mapcar (rcurry #'expand-javascript #'expand-javascript)
				      (mapcar #'walk-form-no-expand body))))
		    #'expand-render 's))))
     (return-stream s)))

(defmacro +js2+ (&body body)
  "Convert body to javascript and return it as a string"
  `(let ((s (make-compressed-stream "")))
     (funcall (lambda ()
		(block rule-block		  
		  ,(expand-render
		    (walk-grammar
		     `(:and ,@(mapcar (rcurry #'expand-javascript #'expand-javascript)
				      (mapcar #'walk-form-no-expand body))))
		    #'expand-render 's))))
     (return-stream s)))

(defun <:js+ (&rest body)
  "Convert body to javascript and return it as a string, 
this is parenscript/ucw+ backward compatiblity macro."
  (with-unique-names (output)
    (eval
     `(let ((,output (if +context+ (http-response.stream (response +context+)) *core-output*)))
	(funcall (lambda ()
		   (block rule-block
		     ,(expand-render
		       (walk-grammar
			`(:and ,@(mapcar (rcurry #'expand-javascript #'expand-javascript)
					 (mapcar #'walk-form-no-expand body))
			       #\Newline))
		       #'expand-render output)
		     nil)))))))

(defmacro defun/javascript (name params &body body)
  "Defun a function in both worlds (lisp/javascript)"
  `(prog1
     (defun ,name ,params ,@body)
     (defun ,(intern (string-upcase (format nil "~A!" name))) (s)
       (block rule-block		  
	 ,(expand-render (walk-grammar
			  (expand-javascript
			   (make-instance 'setq-form
					  :var name
					  :value (fix-javascript-returns
						  (walk-form-no-expand
						   `(lambda ,params
						      ,@body))))
			   #'expand-javascript))
			 #'expand-render 's)))))

;; (defun/javascript fun1 (a)
;;   (let ((a (lambda (a b c)
;; 	     (list a b c))))
;;     (return a)))

(defmacro defun/parenscript (name params &body body)
  `(prog1
       (defun ,name ,params ,@body)
     (defun ,(intern (string-upcase (format nil "~A!" name))) (s)
       (write-stream s
		     ,(js:js* `(lambda ,params
				 ,@body))))))

;; (defun/parenscript fun2 (a)
;;   (let ((a (lambda (a b c)
;; 	     (list a b c))))
;;     (return a)))

;; (js+ 
;;   (+ 1 1)
;;   (+ 2 2))


(defjsmacr0 $ (id)
  `(document.get-element-by-id ,id))

(defjsmacr0 debug (&rest rest)
  `(console.debug ,@rest))

(defjsmacr0 mapcar (lambda lst)
  `(dojo.map ,lst ,lambda))


;; (defmacro defjavascript-transformer (class (&rest slots) &body body)
;;   "Define a javascript transformer which is fed to expander"
;;   `(defmethod transform-javascript ((form ,class) (transform function))
;;      (declare (ignorable transform))
;;      ;;     (format t "inside ~A~%" ',class)
;;      (with-slots ,slots form
;;        ,@body)))

;; (defjavascript-transformer form ()
;;   form)

;; (defjavascript-transformer implicit-progn-mixin (body)
;;   (describe body)
;;   form)

;; (defjavascript-transformer lambda-function-form (arguments declares body)
;;   (describe body)
;;   (describe arguments)
;;   form)

;; (defun return-trans (&rest body)
;;   (unwalk-form
;;    (transform-javascript
;;     (walk-js-form `(progn ,@body))
;;     #'transform-javascript)))

;; (defmacro zee ()
;;   `(let ((s (make-indented-stream "" 0)))
;;      ,(expand-render 
;;        (walk-grammar
;; 	(expand-javascript
;; 	 (walk-js-form
;; 	  `(block nil
;; 	     (list 1 2 3)
;; 	     (block nil2
;; 	       (list 3 2 1)
;; 	       (list 5 6 7))
;; 	     (block nil3
;; 	       (list 4 5 6)
;; 	       (list 5 6 7))))
;; 	 #'expand-javascript))
;;        #'expand-render 's)
;;      (return-stream s)))