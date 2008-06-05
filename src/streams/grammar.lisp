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
;; i) Every operator starts with ':', this is due to avoid from symbol conflicts
;; ii) walk-grammar walks DSL forms
;; ii) Every argument is walked
;; iii) To escape to lisp use (:do ..) operator
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun walk-grammar (lst)
    "Walker for Stream Grammar"
    (cond
      ((and (listp lst) (keywordp (car lst)))
       (if (find-class (intern (format nil "~A-FORM" (symbol-name (car lst)))
			       (find-package :core-server)) nil)
	   (apply (symbol-function (intern (format nil "~A-FORM" (symbol-name (car lst)))
					   (find-package :core-server)))
		  (mapcar #'walk-grammar (cdr lst)))
	   (apply #'bind-form (intern (symbol-name (car lst))) (mapcar #'walk-grammar (cdr lst)))))
      (t lst))))

(defmacro defgrammar-form (name supers lambda-list)
  "Define a stream operator"
  `(prog1 (defclass ,name (,@supers form)
	    ,(mapcar (lambda (slot)
		       (list slot :accessor slot :initarg (make-keyword slot)))
		     (flatten (extract-argument-names lambda-list))))
     (defun ,name ,lambda-list
       (make-instance ',name ,@(reduce (lambda (acc slot)
					 (cons (make-keyword slot)
					       (cons slot acc)))
				       (flatten (extract-argument-names lambda-list))
				       :initial-value nil)))))

;;-----------------------------------------------------------------------------
;; Classification of Stream Operators
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; Stream Operators
;;-----------------------------------------------------------------------------
;;   - Bind - Binds the result of the funciton to a variable (ie let)
;;   - Checkpoint - Checkpoints current stream while executing its body
;;   - Commit - Commits current stream and returns from checkpoint block
;;   - Rewind - Rewinds current stream and returns from checkpoint block
;;   - Rewind-Return - Rewinds current stream and return from checkpoint
;;     block with the 'value'.
;;-----------------------------------------------------------------------------
(defgrammar-form bind-form ()
  (func &rest args))

(defgrammar-form checkpoint-form ()
  (&rest args))

(defgrammar-form commit-form ()
  ())

(defgrammar-form rewind%-form ()
  ())

(defgrammar-form rewind-form ()
  ())

(defgrammar-form rewind-return-form ()
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
(defgrammar-form and-form ()
  (&rest args))

(defgrammar-form or-form ()
  (&rest args))

(defgrammar-form not-form ()
  (&rest args))

(defgrammar-form return-form ()
  (value))

(defgrammar-form zero-or-more-form ()
  (&rest children))

(defgrammar-form zom-form (zero-or-more-form)
  (&rest children))

(defgrammar-form one-or-more-form ()
  (&rest children))

(defgrammar-form oom-form (one-or-more-form)
  (&rest children))

;; Already defined in arnesi walker, didn't want to break
(defun if-form (consequent then &optional else)
  (make-instance 'if-form :consequent consequent :then then :else else))

(defgrammar-form cond-form ()
  (conditions))

(defgrammar-form do-form ()
  (&rest children))

;;-----------------------------------------------------------------------------
;; Debugging Operators
;;-----------------------------------------------------------------------------
;;   - Debug - Prints the current peeked element
;;   - Current - Describes stream object
;;-----------------------------------------------------------------------------
(defgrammar-form debug-form ()
  ())

(defgrammar-form current-form ()
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
(defgrammar-form type-form ()
  (type-name &optional target))

(defgrammar-form sequence-case-sensitive-form ()
  (value))

(defgrammar-form scs-form (sequence-case-sensitive-form)
  (value))

(defgrammar-form seq-form (sequence-case-sensitive-form)
  (value))

(defgrammar-form sequence-case-insensitive-form ()
  (value))

(defgrammar-form sci-form (sequence-case-insensitive-form)
  (value))

(defgrammar-form collect-form ()
  (source target))

(defgrammar-form sep-form ()
  (seperator children))

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

(defmacro defgrammar-expander (name &body body)
  "Define a stream operator to lisp2 compiler"
  `(defmethod expand-grammar ((form ,name) expander (stream symbol) &optional (continue nil) (checkpoint nil))
     (declare (ignorable continue checkpoint))
     ,@body))

(defgrammar-expander bind-form
  (if (< (length (args form)) 1)
      `(,(func form) ,stream)
      `(multiple-value-setq ,(args form) (,(func form) ,stream))))

;;-----------------------------------------------------------------------------
;; Stream Operator Compilers
;;-----------------------------------------------------------------------------
(defgrammar-expander checkpoint-form
  (with-unique-names (checkpoint)
    `(block ,checkpoint
       (checkpoint-stream ,stream)
       ,(funcall expander (apply #'and-form (args form)) expander stream continue checkpoint)
       (rewind-stream ,stream)
       ,continue)))

(defgrammar-expander commit-form
  `(progn
     (commit-stream ,stream)
     (return-from ,checkpoint ,continue)))

(defgrammar-expander rewind%-form
  `(rewind-stream ,stream))

(defgrammar-expander rewind-form
  `(progn
     ,(funcall expander (walk-grammar `(:rewind%)) expander stream continue checkpoint)
     (return-from ,checkpoint ,continue)))

(defgrammar-expander rewind-return-form
  (funcall expander (walk-grammar `(:and (:rewind%) (:return ,(value form)))) 
	   expander stream continue checkpoint))

;;-----------------------------------------------------------------------------
;; Control Operator Compilers
;;-----------------------------------------------------------------------------
(defgrammar-expander and-form
  `(and ,@(mapcar (rcurry expander expander stream t checkpoint) (args form))))

(defgrammar-expander or-form
  `(or ,@(mapcar (rcurry expander expander stream nil checkpoint) (args form))))

(defgrammar-expander not-form
  `(not ,@(mapcar (rcurry expander expander stream continue checkpoint) (args form))))

(defgrammar-expander return-form
  (with-unique-names (retval)
    `(let ((,retval (multiple-value-list ,(value form))))
       (cond
	 ((= (the fixnum (current-checkpoint ,stream)) (the fixnum cp)) nil)
	 ((< (the fixnum (current-checkpoint ,stream)) (the fixnum cp))
	  (error "This parser rule is not functional!"))
	 (t (do ((i (current-checkpoint ,stream)
		    (current-checkpoint ,stream)))
		((= (the fixnum i) (the fixnum cp)) nil)
	      (commit-stream ,stream))))
       (return-from rule-block (apply #'values ,retval)))))

(defgrammar-expander zero-or-more-form
  `(not (do ()
	    ((not ,(funcall expander (apply #'and-form (children form))
			    expander stream continue checkpoint))))))

(defgrammar-expander one-or-more-form
  (funcall expander (apply #'and-form (append (children form) (list (apply #'zom-form (children form)))))
	   expander stream continue checkpoint))

(defgrammar-expander if-form
  `(prog1 ,continue
     (if ,(consequent form)
	 ,(funcall expander (then form) expander stream continue checkpoint)
	 ,(if (arnesi::else form)
	      (funcall expander (arnesi::else form) expander stream continue checkpoint)))))

(defgrammar-expander cond-form
  `(prog1 ,continue
     (cond
       ,@(mapcar #'(lambda (atom)
		     (list (car atom)
			   (funcall expander (apply #'and-form (children form))
				    expander stream continue checkpoint)))
		 (conditions form)))))

(defgrammar-expander do-form
  `(prog1 ,continue ,@(children form)))

;;-----------------------------------------------------------------------------
;; Debug Operator Compilers
;;-----------------------------------------------------------------------------
(defgrammar-expander debug-form
  `(prog1 ,continue
     (format t "current:~A~%" (peek-stream ,stream))))

(defgrammar-expander current-form
  `(prog1 ,continue (describe ,stream)))

;;-----------------------------------------------------------------------------
;; Vector Operator Compilers
;;-----------------------------------------------------------------------------
(defgrammar-expander type-form
  `(if (typep (peek-stream ,stream) ',(type-name form))
       ,(if (target form)
	    `(setq ,(target form) (read-stream ,stream))
	    `(read-stream ,stream))))

(defgrammar-expander sequence-case-sensitive-form  
  (if (stringp (value form))
      (funcall expander
	       (walk-grammar `(:checkpoint
			       ,@(nreverse
				  (reduce (lambda (acc atom) (cons atom acc))
					  (value form) :initial-value nil))
			       (:commit)))
	       expander stream nil checkpoint)
      (funcall expander
	       (walk-grammar
		`(:checkpoint
		  (:do
		   (reduce #'(lambda (stream atom)
			       (when (not (eq (peek-stream stream) (char-code atom)))
				 (rewind-stream stream)
				 (return-from ,checkpoint ,(not continue)))
			       (read-stream stream)
			       stream)
			   ,(value form) :initial-value ,stream))
		  (:commit)))
	       expander stream nil checkpoint)))

(defgrammar-expander sequence-case-insensitive-form
  (if (stringp (value form))
      (funcall expander
	       (walk-grammar
		`(:checkpoint
		  ,@(nreverse
		     (reduce #'(lambda (acc atom)
				 (let ((upcase (char-upcase atom))
				       (downcase (char-downcase atom)))
				   (if (eq upcase downcase)
				       (cons atom acc)
				       (cons (list ':or upcase downcase)
					     acc))))
			     (value form) :initial-value nil))
		  (:commit)))
	       expander stream nil checkpoint)
      (funcall expander
	       (walk-grammar
		`(:checkpoint
		  (:do
		   (reduce #'(lambda (stream atom)
			       (when (not (or (eq (peek-stream stream) (char-code (char-upcase atom)))
					      (eq (peek-stream stream) (char-code (char-downcase atom)))))
				 (rewind-stream ,stream)
				 (return-from ,checkpoint ,(not continue)))
			       (read-stream stream)
			       stream)
			   ,(value form) :initial-value ,stream))
		  (:commit)))
	       expander stream nil checkpoint)))

(defgrammar-expander collect-form
  `(or (push-atom ,(source form) ,(target form)) t))

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
