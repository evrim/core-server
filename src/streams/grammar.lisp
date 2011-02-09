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

;;+----------------------------------------------------------------------------
;;| A DSL For Stream Processing
;;| Author: Evrim Ulu <evrim@core.gen.tr>
;;| Date: 03/2008
;;+----------------------------------------------------------------------------
;;
;; This is the refactor of our old vector stream parser. It is generalized
;; to handle more streams and more operators.
;;

;;-----------------------------------------------------------------------------
;; About Stream DSL:
;;-----------------------------------------------------------------------------
;; i) Every operator starts with ':' to avoid symbol/package name conflicts
;; ii) walk-grammar walks DSL forms
;; ii) Every argument is walked
;; iii) To escape to lisp use (:do ..) operator
;;
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun make-operator-symbol (name)
    (intern (format nil "~A-OPERATOR" name) (find-package :core-server)))

  (defun operator-ctor (operator)
    (symbol-function (make-operator-symbol operator)))
  
  (defun walk-grammar (lst)
    "Walker for Stream Grammar"
    (cond      
      ((and (listp lst) (listp (car lst)) (keywordp (caar lst)))
       (apply (operator-ctor 'bind)
	      (list (intern (symbol-name (caar lst)))
		    :arguments (mapcar #'walk-grammar (cdar lst))
		    :binds (mapcar #'walk-grammar (cdr lst)))))
      ((and (listp lst) (keywordp (car lst)))
       (if (find-class (make-operator-symbol (symbol-name (car lst))) nil)
	   (apply (operator-ctor (symbol-name (car lst))) (mapcar #'walk-grammar (cdr lst)))
	   (apply (operator-ctor 'bind)
		  (list (intern (symbol-name (car lst)))
			:arguments (mapcar #'walk-grammar (cdr lst)))))) 
      (t lst))))

(defclass operator ()
  ()
  (:documentation "Base class for Grammar AST"))

(defgeneric operator-successor (operator)
  (:documentation "Returns successors of this operator"))

(defmethod operator-successor ((operator t))
  nil)

(defmethod operator-successor :around ((operator t))
  (nreverse (flatten (call-next-method))))

(defmacro defoperator (name supers lambda-list)
  "Define a stream operator"
  `(prog1 (defclass ,(make-operator-symbol name) (,@(mapcar #'make-operator-symbol supers) operator)
	    ,(mapcar (lambda (slot)
		       (list slot :accessor slot :initarg (make-keyword slot)))
		     (flatten (extract-argument-names lambda-list))))
     (defun ,(make-operator-symbol name) ,lambda-list
       (make-instance ',(make-operator-symbol name)
		      ,@(reduce (lambda (acc slot)
				  (cons (make-keyword slot)
					(cons slot acc)))
				(flatten (extract-argument-names lambda-list))
				:initial-value nil)))
     (defmethod operator-successor ((operator ,(make-operator-symbol name)))
       (append ,@(mapcar (lambda (arg)
			   (list 'ensure-list `(,arg operator)))
			 (extract-argument-names lambda-list))))))

;;-----------------------------------------------------------------------------
;; Classification of Stream Operators
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; Stream Operators
;;-----------------------------------------------------------------------------
;;   - Bind - Binds the result of the funciton to variables (m-v-b)
;;   - Checkpoint - Checkpoints current stream while executing its body
;;   - Commit - Commits current stream and returns from checkpoint block
;;   - Rewind - Rewinds current stream and returns from checkpoint block
;;   - Rewind-Return - Rewinds current stream, returns from checkpoint
;;     block with the 'value'.
;;-----------------------------------------------------------------------------
(defoperator bind ()
  (func &key arguments binds))

(defoperator checkpoint  ()
  (&rest args))

(defoperator commit ()
  ())

(defoperator rewind% ()
  ())

(defoperator rewind ()
  ())

(defoperator rewind-return ()
  (value))

;;-----------------------------------------------------------------------------
;; Control Flow Operators
;;-----------------------------------------------------------------------------
;;   - And - Executes body in a sequence
;;   - Or - Executes body until it finds a value other than nil
;;   - Not - Inversion operator
;;   - Return - Returns from current function
;;   - Zero-or-more - Loops until body fails to match something
;;   - Zom - Same as zero-or-more
;;   - One-or-more - matches for the first time and loops the rest
;;   - Oom - Same as one-or-more
;;   - Cond - Usual condition operator
;;   - If - Usual if operator
;;   - Do - Escape operator, use to escape to lisp2
;;-----------------------------------------------------------------------------
(defoperator and ()
  (&rest args))

(defoperator or ()
  (&rest args))

(defoperator not ()
  (&rest args))

(defoperator return ()
  (value))

(defoperator zero-or-more ()
  (&rest children))

(defoperator zom (zero-or-more)
  (&rest children))

(defoperator one-or-more ()
  (&rest children))

(defoperator oom (one-or-more)
  (&rest children))

(defoperator if ()
  (consequent then &optional else))

(defoperator cond ()
  (&rest conditions))

(defoperator do ()
  (&rest children))

(defoperator map ()
  (target-list &rest body))

(defoperator sep ()
  (seperator children))

(defoperator any (map)
  ())

(defoperator all (map)
  ())

(defoperator optional ()
  (&rest children))

;;-----------------------------------------------------------------------------
;; Debugging Operators
;;-----------------------------------------------------------------------------
;;   - Debug - Prints the current peeked element
;;   - Current - Describes stream object
;;-----------------------------------------------------------------------------
(defoperator debug ()
  ())

(defoperator current ()
  ())

;;-----------------------------------------------------------------------------
;; Vector Stream Operators
;;-----------------------------------------------------------------------------
;;   - Type - Checks whether the peeked element is a type of type-name
;;   - Sequence-case-sensitive - matches a sequence of characters (ie. String)
;;   - Scs - Same as sequence-case-sensitive
;;   - Sequence-case-insensitive - matches a sequence of characters case
;;     insensitively.
;;   - Sci - Same as sequeence-case-insensitive
;;   - Collect - Collect source into target, used to make temporary arrays
;;-----------------------------------------------------------------------------
(defoperator type ()
  (type-name &optional target))

(defoperator sequence-case-sensitive ()
  (value))

(defoperator scs (sequence-case-sensitive)
  (value))

(defoperator seq (sequence-case-sensitive)
  (value))

(defoperator sequence-case-insensitive ()
  (value))

(defoperator sci (sequence-case-insensitive)
  (value))

(defoperator collect ()
  (source target))

(defoperator indent ()
  (&optional how-many))

(defoperator deindent ()
  (&optional how-many))

;;-----------------------------------------------------------------------------
;; Object Stream Operators
;;-----------------------------------------------------------------------------
;;   - class? - Binds class of the current object stream
;;   - class! - Sets the class of the current object stream
;;-----------------------------------------------------------------------------
(defoperator class? ()
  (klass))

(defoperator class! ()
  (klass))

(defoperator slot! ()
  (slot-name value))

(defoperator slot? ()
  (slot-name &optional bind))

(defoperator oneof ()
  (vals &optional target))

(defoperator noneof ()
  (vals &optional target))

(defoperator satisfy ()
  (slambda &optional target))

;;-----------------------------------------------------------------------------
;; Stream DSL Operator Compilers
;;-----------------------------------------------------------------------------
;;
;; All stream DSL forms are compiled to lisp2 forms in order to be executable.
;; 
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defgeneric expand-grammar (form expander stream-symbol &optional continue checkpoint)
    (:documentation "Compile grammar body"))

  (defmethod expand-grammar ((form t) (expander function) (stream symbol)
                            &optional (continue nil) (checkpoint nil))
    (declare (ignorable continue checkpoint))
    (error "Please implement appropriate method for expand-grammar")))

(defmacro defgrammar-expander (name (form expander stream continue checkpoint) &body body)
  "Define a stream operator to lisp2 compiler"
  `(defmethod expand-grammar ((,form ,(make-operator-symbol name)) (,expander function) (,stream symbol)
			      &optional (,continue nil) (,checkpoint nil))
     (declare (ignorable ,continue ,checkpoint))
     ,@body))

(defgrammar-expander bind (form expander stream continue checkpoint)
  (if (not (fboundp (func form)))
      (setf (func form) (intern (symbol-name (func form)) :core-server)))
  
  (if (< (length (arguments form)) 1)
      `(,(func form) ,stream)
      `(multiple-value-setq ,(arguments form) (,(func form) ,stream))))

;;-----------------------------------------------------------------------------
;; Stream Operator Compilers
;;-----------------------------------------------------------------------------
(defgrammar-expander checkpoint (form expander stream continue checkpoint)
  (with-unique-names (checkpoint)
    `(block ,checkpoint
       (checkpoint-stream ,stream)
       ,(funcall expander (apply (operator-ctor 'and) (args form))
		 expander stream continue checkpoint)
       (rewind-stream ,stream)
        ,continue)))

(defgrammar-expander commit (form expander stream continue checkpoint)
  `(progn
     (commit-stream ,stream)
     (return-from ,checkpoint ,continue)))

(defgrammar-expander rewind% (form expander stream continue checkpoint)
  `(prog1 ,continue (rewind-stream ,stream)))

(defgrammar-expander rewind (form expander stream continue checkpoint)
  `(progn
     ,(funcall expander (walk-grammar `(:rewind%)) expander stream continue checkpoint)
     (return-from ,checkpoint ,continue)))

(defgrammar-expander rewind-return (form expander stream continue checkpoint)
  (funcall expander (walk-grammar `(:and (:rewind%) (:return ,(value form)))) 
	   expander stream continue checkpoint))

;;-----------------------------------------------------------------------------
;; Control Operator Compilers
;;-----------------------------------------------------------------------------
(defgrammar-expander and (form expander stream continue checkpoint)
  (cond
    ((= 0 (length (args form)))
     nil)
    ((= 1 (length (args form)))
     (funcall expander (car (args form)) expander stream continue checkpoint))
    (t
     `(and ,@(mapcar (rcurry expander expander stream t checkpoint) (args form))))))

(defgrammar-expander or (form expander stream continue checkpoint)
  `(or ,@(mapcar (rcurry expander expander stream nil checkpoint) (args form))))

(defgrammar-expander not (form expander stream continue checkpoint)
  `(not ,@(mapcar (rcurry expander expander stream continue checkpoint) (args form))))

(defgrammar-expander return (form expander stream continue checkpoint)  
  `(progn
     (if (< (current-checkpoint ,stream) cp)
	 (error "This parser rule is not functional"))

     (do ((i (current-checkpoint ,stream)
	     (current-checkpoint ,stream)))
	 ((= (the fixnum i) (the fixnum cp)) nil)
       (commit-stream ,stream))
     
     (return-from rule-block ,(value form))))

(defgrammar-expander zero-or-more (form expander stream continue checkpoint)
  `(not (do ()
	    ((not ,(funcall expander (apply (operator-ctor 'and) (children form))
			    expander stream continue checkpoint))))))

(defgrammar-expander one-or-more (form expander stream continue checkpoint)
  (funcall expander (apply (operator-ctor 'and)
			   (append (children form)
				   (list (apply (operator-ctor 'zom) (children form)))))
	   expander stream continue checkpoint))

(defgrammar-expander if (form expander stream continue checkpoint)
  `(if ,(consequent form)
       ,(funcall expander (then form) expander stream continue checkpoint)
       ,(if (arnesi::else form)
	    (funcall expander (arnesi::else form) expander stream continue checkpoint))))

(defgrammar-expander cond (form expander stream continue checkpoint)
  `(prog1 ,continue
     (cond
       ,@(mapcar #'(lambda (atom)
		     (list (car atom)
			   (funcall expander (apply (operator-ctor 'and)
						    (mapcar #'walk-grammar (cdr atom)))
				    expander stream continue checkpoint)))
		 (conditions form)))))

(defgrammar-expander do (form expander stream continue checkpoint)
  `(prog1 ,continue ,@(children form)))

(defgrammar-expander map (form expander stream continue checkpoint)
  `(prog1 ,continue
     (mapcar (lambda (it)
	       ,@(mapcar (rcurry expander expander stream continue checkpoint)
			 (body form)))
	     ,(target-list form))))

(defgrammar-expander any (form expander stream continue checkpoint)
  `(any (lambda (it)
	  ,@(mapcar (rcurry expander expander stream continue checkpoint)
		    (body form)))
	,(target-list form)))

(defgrammar-expander all (form expander stream continue checkpoint)
  `(all (lambda (it)
	  ,@(mapcar (rcurry expander expander stream continue checkpoint)
		    (body form)))
	,(target-list form)))

;;-----------------------------------------------------------------------------
;; Debug Operator Compilers
;;-----------------------------------------------------------------------------
(defgrammar-expander debug (form expander stream continue checkpoint)
  `(prog1 ,continue
     (format t "current:~A~%" (peek-stream ,stream))))

(defgrammar-expander current (form expander stream continue checkpoint)
  `(prog1 ,continue (describe ,stream)))

;;-----------------------------------------------------------------------------
;; Vector Operator Compilers
;;-----------------------------------------------------------------------------
(defgrammar-expander type (form expander stream continue checkpoint)
  `(if (typep (peek-stream ,stream) ',(type-name form))
       ,(if (target form)
	    `(setq ,(target form) (read-stream ,stream))
	    `(read-stream ,stream))))

(defgrammar-expander sequence-case-sensitive (form expander stream continue checkpoint)
  (with-unique-names (str len count match element)
    `(let* ((,str ,(if (stringp (value form))
		       (string-to-octets (value form) :utf-8)
		       `(string-to-octets ,(value form) :utf-8)))
	    (,len (length ,str)))
       (checkpoint-stream ,stream)
       (do ((,element (peek-stream ,stream) (peek-stream ,stream))
	    (,count 0 (+ ,count 1))
	    (,match t (eq ,element (aref ,str ,count))))
	   ((or (null ,match) (>= ,count ,len))
	    (if ,match
		(prog1 t (commit-stream ,stream))
		(prog1 nil (rewind-stream ,stream))))
	 (read-stream ,stream)))))

(defgrammar-expander sequence-case-insensitive (form expander stream continue checkpoint)
  (with-unique-names (str-upcase str-downcase len count match element)
    `(let* ((,str-upcase ,(if (stringp (value form))
			      (string-to-octets (string-upcase (value form)) :utf-8)
			      `(string-to-octets (string-upcase ,(value form)) :utf-8)))
	    (,str-downcase ,(if (stringp (value form))
				(string-to-octets (string-downcase (value form)) :utf-8)
				`(string-to-octets (string-downcase ,(value form)) :utf-8)))
	    (,len (length ,str-upcase)))
       (checkpoint-stream ,stream)
       (do ((,element (peek-stream ,stream) (peek-stream ,stream))
	    (,count 0 (+ ,count 1))
	    (,match t (or (eq ,element (aref ,str-upcase ,count))
			  (eq ,element (aref ,str-downcase ,count)))))
	   ((or (null ,match) (>= ,count ,len))
	    (if ,match
		(prog1 t (commit-stream ,stream))
		(prog1 nil (rewind-stream ,stream))))
	 (read-stream ,stream)))))

(defgrammar-expander collect (form expander stream continue checkpoint)
  `(or (push-atom ,(source form) ,(target form)) t))

(defgrammar-expander oneof (form expander stream continue checkpoint)
  (cond
    ((listp (vals form))
     (error 'notimplementedyet))
    ((stringp (vals form))
     (funcall expander
	      (walk-grammar
	       `(:or ,@(mapcar #'(lambda (x)
				   `(:and ,x (:do ,(if (target form)
						       `(setq ,(target form) ,x)))))
			       (nreverse
				(reduce #'(lambda (acc item)
					    (cons item acc))
					(vals form) :initial-value nil)))))
	      expander stream continue checkpoint))
    ((numberp (vals form))
     (error 'notimplementedyet))))

(defgrammar-expander noneof (form expander stream continue checkpoint)
  (let ((peeq (gensym)))
    (cond
      ((stringp (vals form))
       `(let ((,peeq (peek-stream ,stream)))
	  (if (not (or ,@(mapcar #'(lambda (x)
				     `(= ,peeq ,(char-code x)))
				 (nreverse
				  (reduce #'(lambda (acc item)
					      (cons item acc))
					  (vals form) :initial-value nil)))))
	      (prog1 t
		(setq ,(target form) (read-stream ,stream)))))))))

(defgrammar-expander satisfy (form expander stream continue checkpoint)
  `(if (funcall ,(slambda form) (peek-stream ,stream))
       ,(if (target form)
	    `(prog1 ,continue
	       (setq ,(target form) (read-stream ,stream)))
	    continue)
       ,(not continue)))

(defgrammar-expander optional (form expander stream continue checkpoint)
  (funcall expander
	   (walk-grammar
	    `(:checkpoint
	      ,@(children form)
	      (:commit)))
	   expander stream continue checkpoint))

(defoperator type2 ()
  (type-name &optional target))

(defgrammar-expander type2 (form expander stream continue checkpoint)
  (if (target form)
      `(setq ,(target form) (read-stream2 ,stream ',(type-name form)))
      `(read-stream2 ,stream ',(type-name form))))

(defoperator type3 ()
  (type-name &optional target))

(defgrammar-expander type3 (form expander stream continue checkpoint)
  (if (target form)
      `(if (,(type-name form) (peek-stream ,stream))
	   (setq ,(target form) (read-stream ,stream)))
      `(if (,(type-name form) (peek-stream ,stream))
	   (read-stream ,stream))))


;;-----------------------------------------------------------------------------
;; Object Stream Operator Compilers
;;-----------------------------------------------------------------------------
(defgrammar-expander class? (form expander stream continue checkpoint)
  `(setq ,(klass form) (slot-value ,stream '%clazz)))

(defgrammar-expander class! (form expander stream continue checkpoint)
  `(setf (clazz ,stream) ,(klass form)))

(defgrammar-expander slot! (form expander stream continue checkpoint)
  `(write-stream ,stream (cons ,(slot-name form) ,(value form))))

(defgrammar-expander slot? (form expander stream continue checkpoint)
  `(when (eq (car (peek-stream ,stream)) ,(slot-name form))
     ,(if (bind form)
	  `(setq ,(bind form) (cdr (read-stream ,stream)))
	  `(read-stream ,stream))))

;; (defparser crlf? ()
;;   (:or (:checkpoint #\Return #\Newline (:commit)) #\Newline)
;;   (:return t))

;; (defrender crlf! ()
;;   #\Return #\Newline)

;; (defrender hex-value! (hex)
;;   (:byte! (aref +hex-alphabet+ (floor (/ hex 16))))
;;   (:byte! (aref +hex-alphabet+ (rem hex 16))))

;; (defrender gee! ()
;;   "gee")
;; (defparser lwsp? ()  
;;   (:zom (:type (or space? tab? carriage-return? linefeed?)))
;;   (:return t))

;; (defparser hex-value? (a b)  
;;   (:or (:and (:type digit? a)
;; 	     (:do (setq a (- (the (unsigned-byte 8) a) 48))))
;;        (:and (:type hex-upchar? a)
;; 	     (:do (setq a (+ 10 (- (the (unsigned-byte 8) a) 65)))))
;;        (:and (:type hex-lochar? a)
;; 	     (:do (setq a (+ 10 (- (the (unsigned-byte 8) a) 97))))))
;;   (:or (:and (:type digit? b)
;; 	     (:do (setq b (- (the (unsigned-byte 8) b) 48))))
;;        (:and (:type hex-upchar? b)
;; 	     (:do (setq b (+ 10 (- (the (unsigned-byte 8) b) 65)))))
;;        (:and (:type hex-lochar? b)
;; 	     (:do (setq b (+ 10 (- (the (unsigned-byte 8) b) 97))))))
;;   (:return (+ (* (the (unsigned-byte 8) a) 16) (the (unsigned-byte 8) b))))

;; (defparser escaped? (hex)  
;;   (:and #\% (:hex-value? hex) (:return hex)))

;; (defparser quoted? ((value (make-accumulator :byte)) c b)
;;   (:or
;;    (:and
;;     #\"
;;     (:zom (:or
;; 	   (:checkpoint
;; 	    (:and #\\ #\"
;; 		  (:do (push-atom #\" value))
;; 		  (:commit)))
;; 	   (:and #\" (:return (octets-to-string value :utf-8)))
;; 	   (:and (:or ;;		  (:and (:utf-escaped? b c) (:collect b value))
;; 		  (:escaped? c)
;; 		  (:type (or visible-char? space?) c))		 
;; 		 (:collect c value)))))
;;    (:and (:zom (:or ;;		(:and (:utf-escaped? b c) (:collect b value))
;; 		(:escaped? c)
;; 		(:type (or visible-char? space?) c))
;; 	       (:collect c value))
;; 	 (:return (octets-to-string value :utf-8)))))

;; (defvector->lisp rule-1 (a b c &aux def)
;;   (:zom (:or (:and (:seq "Username:")
;; 		   (:zom (:type visible-char? c) (:collect c username)))
;; 	     (:type octet?)))
;;   (:return username))

;; (deflisp->vector rule-2 (a b c &aux gee)
;;   (:char! a) (:byte! b) (:char! c))

;; (defvector->vector utf-8?! ()
;;   (:zom (:type octet? c)
;; 	(:if (> c 127)
;; 	     (:and
;; 	      (:checkpoint
;; 	       (read-until-utf8 acc))
;; 	      (:char! (octets-to-string acc :utf-8))))))
