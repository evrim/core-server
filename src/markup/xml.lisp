;; +------------------------------------------------------------------------
;; | XML Base
;; +------------------------------------------------------------------------
(in-package :core-server)

;; -------------------------------------------------------------------------
;; Protocol
;; -------------------------------------------------------------------------
(defgeneric xml.tag (xml)
  (:documentation "Returns tag"))

(defgeneric xml.namespace (xml)
  (:documentation "Returns namespace"))

(defgeneric xml.attributes (xml)
  (:documentation "Returns symbol names of attributes"))

(defgeneric xml.attribute (xml attribute)
  (:documentation "Returns the value of the 'attribute'"))

(defgeneric xml.children (xml)
  (:documentation "Returns children of xml node"))

(defgeneric xml.equal (a b)
  (:documentation "XML Equivalence Predicate")
  (:method ((a t) (b t)) (eq a b))
  (:method ((a string) (b string)) (equal a b)))

;; -------------------------------------------------------------------------
;; XML Metaclass
;; -------------------------------------------------------------------------
(defclass xml+ (class+)
  ((tag :initform nil :initarg :tag :reader xml+.tag)
   (namespace :initform nil :initarg :namespace :reader xml+.namespace)
   (schema :initform nil :initarg :schema :reader xml+.schema)
   (attributes :initarg :attributes :initform nil :reader xml+.attributes)))

(defmethod class+.ctor-name ((self xml+))
  (class+.name self))

(defmethod class+.ctor ((self xml+))
  (let ((name (class+.name self)))
    `(progn
       (fmakunbound ',(class+.ctor-name self))
       (defun ,(class+.ctor-name self) (&rest args)
	 (multiple-value-bind (attributes children) (tag-attributes args)
	   (apply #'make-instance ',name
		  (list* :children (flatten children) attributes)))))))

;; -------------------------------------------------------------------------
;; XML Base Class
;; -------------------------------------------------------------------------
(defclass xml ()
  ((xmlns :initarg :xmlns :accessor xml.xmlns)
   (children :initform nil :initarg :children :accessor xml.children)))

(defmethod xml.tag ((xml xml))
  (xml+.tag
   (or (any (lambda (a) (and (typep a 'xml+) a)) 
	    (cdr (class-superclasses (class-of xml))))
       (class-of xml))))

(defmethod xml.attributes ((xml xml))
  (xml+.attributes
   (or (any (lambda (a) (and (typep a 'xml+) a))
	    (cdr (class-superclasses (class-of xml))))
       (class-of xml))))

(defmethod xml.namespace ((xml xml))
  (xml+.namespace
   (or (any (lambda (a) (and (typep a 'xml+) a))
	    (cdr (class-superclasses (class-of xml))))
       (class-of xml))))

(defmethod xml.attribute ((xml xml) attribute)
  (slot-value xml attribute))

(defmethod xml.equal ((a xml) (b xml))
  (and
   (string= (xml.tag a) (xml.tag b))
   (string= (xml.namespace a) (xml.namespace b))
   (reduce (lambda (acc attr)
	     (and acc (xml.equal (xml.attribute a attr) (xml.attribute a attr))))
	   (xml.attributes a)
	   :initial-value t)
   (= (length (xml.children a)) (length (xml.children b)))
   (reduce (lambda (acc child)
	     (and acc (xml.equal (car child) (cdr child))))
	   (mapcar #'cons (xml.children a) (xml.children b))
	   :initial-value t)))

(defmethod xml.children ((self t)) nil)

(defun make-xml-type-matcher (type)
  (lambda (a) (typep a type)))

(defun xml-search (elements goal-p &optional (successor #'xml.children))
  (let ((result))
    (core-search (if (listp elements)
		     elements
		     (list elements))
		 (lambda (a)
		   (if (typep a 'xml)
		       (if (funcall goal-p a)
			   (pushnew a result)))
		   nil)
		 successor
		 #'append)
    result))

;; -------------------------------------------------------------------------
;; XML defining macro: defmxl
;; -------------------------------------------------------------------------
(defmacro defxml (name &rest attributes)
  `(defclass+ ,name (xml)
     (,@(mapcar (lambda (attr)
		  (list attr :print t))
		attributes))
     (:metaclass xml+)
     (:tag ,@(string-downcase (symbol-name name)))
     (:namespace ,@(string-downcase
		    (subseq (car (package-nicknames (symbol-package name))) 1)))
     (:attributes ,@attributes)))

;; -------------------------------------------------------------------------
;; XML Generic Class
;; -------------------------------------------------------------------------
(defclass generic-xml (xml)
  ((tag :initarg :tag :initform nil :accessor xml.tag)
   (namespace :initarg :namespace :initform nil :accessor xml.namespace)
   (attributes :initarg :attributes :initform nil)))

(defmethod xml.attributes ((xml generic-xml))
  (mapcar #'car (slot-value xml 'attributes)))

(defmethod xml.attribute ((xml generic-xml) attribute)
  (cdr (assoc attribute (slot-value xml 'attributes))))

;; -------------------------------------------------------------------------
;; Generic XML Constructor
;; -------------------------------------------------------------------------
(defun xml (tag namespace attributes &rest children)
  (make-instance 'generic-xml
		 :tag tag
		 :namespace namespace
		 :attributes attributes
		 :children (flatten children)))

(defprint-object (self generic-xml)
  (with-slots (tag namespace attributes children) self
    (if namespace
	(format *standard-output* "<~A:~A(~D)~{ ~A~}>"
		namespace tag (length children) attributes)
	(format *standard-output* "<~A(~D)~{ ~A~}>"
		tag (length children) attributes))))

;;---------------------------------------------------------------------------
;; XML Parser
;;---------------------------------------------------------------------------
(defrule xml-attribute-name? (c attribute namespace)
  (:or (:type alphanum? c)
       (:and #\- (:do (setq c #\-))))
  (:and (:do (setq attribute (make-accumulator)))
	(:collect c attribute))
  (:zom (:or (:type alphanum? c)
	     (:and #\- (:do (setq c #\-))))
	(:collect c attribute))  
  (:or (:and #\:
	     (:do (setq namespace attribute)
		  (setq attribute (make-accumulator)))
	     (:oom (:or (:type alphanum? c)
			(:and #\- (:do (setq c #\-))))
		   (:collect c attribute))
	     (:return (if namespace
			  (values attribute namespace)
			  ;;  (format nil "~A:~A" namespace attribute)
			  attribute)))
       (:return (if namespace
		    (values attribute namespace)
		    ;;  (format nil "~A:~A" namespace attribute)
		    attribute))))

(defrule xml-attribute-value? (c val)
  (:or (:and
	#\"
	(:do (setq val (make-accumulator :byte)))
	(:zom (:not #\")
	      (:checkpoint
	       #\> (:rewind-return (octets-to-string val :utf-8)))
	      ;; (:type (or visible-char? space?) c)
	      (:type octet? c)
	      (:collect c val))
	;; (:zom (:checkpoint #\" (:return val))
	;;       (:checkpoint #\> (:rewind-return val))
	;;       (:type (or visible-char? space?) c)
	;;       (:collect c val))
	)
       (:and
	#\'
	(:do (setq val (make-accumulator :byte)))
	(:zom (:not #\')
	      (:checkpoint
	       #\> (:rewind-return (octets-to-string val :utf-8)))
	      ;; (:type (or visible-char? space?) c)
	      (:type octet? c)
	      (:collect c val))
	;; (:zom (:checkpoint #\' (:return val))
	;;       (:checkpoint #\> (:rewind-return val))
	;;       (:type (or visible-char? space?) c)
	;;       (:collect c val))
	)
       (:zom ;; (:checkpoint (:or #\> #\/)
	     ;; 		  (:rewind-return val))
	     (:and (:not #\Space)
		   (:not #\>)
		   (:not #\/))
	     (:type octet? c)
	     (:collect c val)))
  (:return (octets-to-string val :utf-8)))

(defrule xml-attribute? (name value)
  (:xml-attribute-name? name)
  #\=
  (:xml-attribute-value? value)
  (:return (cons name value)))

(defrule xml-tag-name? (tag namespace)
  (:xml-attribute-name? tag namespace)
  (:return (values tag namespace)))

(defrule xml-lwsp? (c)
  (:oom (:or (:and (:type (or space? tab?)))
	     (:and (:type (or carriage-return? linefeed?))
		   (:do (setf c t))))
  (:if c
       (:return #\Newline)
       (:return #\Space))))

(defrule xml-text-node? (c acc)
  (:not #\/)
  ;; (:not #\<)
  ;; (:checkpoint #\< (:rewind-return (octets-to-string acc :utf-8)))
  (:or ;; (:and #\& (:or (:and (:seq "gt;") (:do (setf c #\<)))
       ;; 		      (:and (:seq "lt;") (:do (setf c #\>)))))
   (:and (:seq "&gt;") (:do (setf c #\<)))
   (:and (:seq "&lt;") (:do (setf c #\>)))
   (:xml-lwsp? c)
   (:type octet? c))
  (:do (setq acc (make-accumulator :byte)))
  (:collect c acc)
  (:zom (:not #\<)
	;; (:checkpoint #\< (:rewind-return (octets-to-string acc :utf-8)))
	(:or (:and (:seq "&gt;") (:do (setf c #\<)))
	     (:and (:seq "&lt;") (:do (setf c #\>)))
	     ;; (:and #\& (:or (:and (:seq "gt;") (:do (setf c #\<)))
	     ;; 	  (:and (:seq "lt;") (:do (setf c #\>)))))
	     (:xml-lwsp? c)
	     (:type octet? c))
	(:collect c acc))
  (:if (> (length acc) 0)
       (:return (octets-to-string acc :utf-8))))

(defrule xml-cdata? (c acc)
  ;; #\< #\!
  #\[ #\C #\D #\A #\T #\A #\[ ;; (:seq "CDATA[")
  (:do (setq acc (make-accumulator)))
  (:zom (:not (:seq "]]>"))
	(:type octet? c)
	(:collect c acc))
  (:return acc))

(defrule xml-comment? (c acc)
  ;; #\<
  ;; #\! 
  #\- #\- ;; (:seq "<!--")
  (:do (setq acc (make-accumulator)))
  (:collect #\< acc) (:collect #\! acc)
  (:collect #\- acc) (:collect #\- acc)
  (:zom (:not (:seq "-->"))
	(:type octet? c)
	(:collect c acc))
  (:collect #\- acc)
  (:collect #\- acc)
  (:collect #\> acc)
  (:return acc))

(defparser %xml-lexer? (tag namespace attr attrs child children
			    a b c d)
  (:xml-tag-name? tag namespace)
  (:zom (:lwsp?) (:xml-attribute? attr)
	(:do (push attr attrs)))
  (:or (:and (:lwsp?) (:seq "/>")
	     (:return (values tag namespace (nreverse attrs))))
       (:and #\>
	     (:zom (:lwsp?)
		   ;; (:debug)
		   (:or
		    (:and #\<
			  (:or (:and #\!
				     (:or (:xml-cdata? child)
					  (:xml-comment? child))
				     (:do (push child children)))
			       (:and (:%xml-lexer? a b c d)
				     (:do (push (list* a b c d) children)))))
		    (:and (:xml-text-node? child)
			  (:do (push child children))
			  #\/
			  (:if namespace
			       (:and (:sci namespace) #\: (:sci tag))
			       (:sci tag))
			  #\>
			  (:return (values tag namespace (nreverse attrs)
					   (nreverse children))))))
	     #\/
	     (:if namespace
		  (:and (:sci namespace) #\: (:sci tag))
		  (:sci tag))
	     #\>
	     (:return (values tag namespace (nreverse attrs)
			      (nreverse children))))))

(defparser xml-lexer? (tag namespace attrs children)
  (:lwsp?)
  (:checkpoint (:seq "<?xml")
	       (:zom (:not #\>) (:type octet?)) (:lwsp?) (:commit))
  (:checkpoint (:seq "<!DOCTYPE")
	       (:zom (:not #\>) (:type octet?)) (:lwsp?) (:commit))
  #\<
  (:%xml-lexer? tag namespace attrs children)
  (:return (list* tag namespace attrs children)))

(defvar +xml-namespace+ (find-package :<))
(defvar +xml-namespaces-table+
  (list (cons "http://www.w3.org/2005/Atom" (find-package :<atom))
	(cons "http://schemas.google.com/photos/2007"
	      (find-package :<gphoto))
	(cons "http://search.yahoo.com/mrss/" (find-package :<media))
	(cons "http://www.opensearch.org/Specifications/OpenSearch/1.1"
	      (find-package :<open-search))))

(declaim (inline xml->symbol))
(defun xml->symbol (name &optional package)
  (let ((a (reduce (lambda (acc a)
		     (cond
		       ((and (> (char-code a) 64)
			     (< (char-code a) 91))
			(push-atom #\- acc)
			(push-atom (code-char (+ (char-code a)
						 32))
				   acc))
		       (t (push-atom a acc)))
		     acc)
		   name :initial-value (make-accumulator))))
    (if package
	(intern (string-upcase a) package)
	(intern (string-upcase a) (find-package :<)))))

(defun parse-xml (xml)
  (labels ((property->keyword (property)
	     (if (position #\- property)
		 (make-keyword
		  (reduce (lambda (acc a)
			    (cond
			      ((eq #\- a)
			       (push-atom #\- acc)
			       (push-atom #\- acc))
			      (t (push-atom a acc)))
			    acc)
			  property :initial-value (make-accumulator)))
		 (make-keyword property)))
	   (make-generic-element (tag namespace attributes children)
	     (warn "<~A:~A> tag not found, using generic xml element."
		   namespace tag)
	     (apply #'xml tag namespace
		    (cons attributes (mapcar #'parse-xml children))))
	   (make-element (symbol attributes children)
	     (apply symbol 
		    (append
		     (reduce0 (lambda (acc attr)
				(cons (property->keyword (car attr))
				      (cons (cdr attr) acc)))
			      attributes)
		     (mapcar #'parse-xml children)))))
    (if (atom xml)
	xml
	(destructuring-bind (tag namespace attributes &rest children) xml
	  (let* ((+xml-namespace+
		  (acond
		   ((and namespace (find-package
				    (make-keyword
				     (format nil "<~A" 
					     (symbol-name
					      (xml->symbol namespace))))))
		     it)
		   ((cdr (assoc (cdr
				 (assoc "xmlns" attributes :test #'string=))
				+xml-namespaces-table+ :test #'string=))
		    it)
		   (t
		    +xml-namespace+)))
		 (symbol (xml->symbol tag +xml-namespace+)))
	    (if (and (fboundp symbol)
		     (not (eq (symbol-package symbol) #.(find-package :cl))))
		(let ((instance (make-element symbol attributes children)))
		  (if (slot-exists-p instance 'tag)
		      (setf (slot-value instance 'tag) tag))
		  (if (slot-exists-p instance 'namespace)
		      (setf (slot-value instance 'namespace) namespace))
		  instance)
		(make-generic-element tag namespace attributes children)))))))

;; +------------------------------------------------------------------------
;; | XML Stream
;; +------------------------------------------------------------------------
(defclass xml-stream (wrapping-stream)
  ())

(defun make-xml-stream (stream)
  (make-instance 'xml-stream :stream stream))

(defmethod read-stream ((stream xml-stream))
  (parse-xml (xml-lexer? (slot-value stream '%stream))))

(defmethod write-stream ((stream xml-stream) (string string))
  (prog1 stream
    (with-slots (%stream) stream
      (disable-indentation %stream)
      (string! %stream
	       (reduce (lambda (acc atom)
			 (cond
			   ((eq atom #\<)
			    (push-atom #\& acc)
			    (push-atom #\g acc)
			    (push-atom #\t acc)
			    (push-atom #\; acc))
			   ((eq atom #\>)
			    (push-atom #\& acc)
			    (push-atom #\l acc)
			    (push-atom #\t acc)
			    (push-atom #\; acc))
			   (t
			    (push-atom atom acc)))
			 acc)
		       string
		       :initial-value (make-accumulator)))
      (enable-indentation %stream))))

(defmethod intro! ((stream xml-stream) (object xml))
  (with-slots (%stream) stream
    (let ((tag (xml.tag object))
	  (namespace (xml.namespace object)))
      (char! %stream #\<)
      (if namespace
	  (progn
	    (string! %stream namespace)
	    (char! %stream #\:)
	    (string! %stream tag))
	  (string! %stream tag)))
    stream))

(defmethod attribute! ((stream xml-stream) attribute)
  (with-slots (%stream) stream
    (char! %stream #\Space)
    (if (symbolp (car attribute))
	(string! %stream (symbol-to-js (car attribute)))
	(string! %stream (car attribute)))
    (char! %stream #\=)
    (quoted! %stream (format nil "~A" (cdr attribute)))
    stream))

(defmethod child! ((stream xml-stream) object)
  (char! (slot-value stream '%stream) #\Newline)
  (write-stream stream object)
  stream)

(defmethod outro! ((stream xml-stream) (object xml))
  (with-slots (%stream) stream
    (let ((tag (xml.tag object))
	  (namespace (xml.namespace object)))
      (string! %stream "</")
      (if namespace
	  (progn
	    (string! %stream namespace)
	    (char! %stream #\:)
	    (string! %stream tag))
	  (string! %stream tag))
      (char! %stream #\>))
    stream))

(defmethod write-stream ((stream xml-stream) (object xml))
  (with-slots (%stream) stream
    (intro! stream object)
    (reduce #'attribute!
	    (reduce0 (lambda (acc slot)
		       (aif (slot-value object slot)
			    (cons (cons slot it) acc)
			    acc))
		     (xml.attributes object))
	    :initial-value stream)
    (cond
      ((null (xml.children object))
       (string! %stream "/>"))
      ((stringp (car (xml.children object)))
       (char! %stream #\>)
       (write-stream stream (car (xml.children object)))
       (outro! stream object))
      (t
       (char! %stream #\>)
       (increase-indent %stream)
       (reduce #'child!
	       (slot-value object 'children) 
	       :initial-value stream)
       (decrease-indent %stream)
       (char! %stream #\Newline)
       (outro! stream object)))
    stream))

(defmethod write-stream ((stream xml-stream) (object generic-xml))
  (with-slots (%stream) stream
    (intro! stream object)
    (reduce #'attribute!
	    (slot-value object 'attributes)
	    :initial-value stream)
    (cond
      ((null (xml.children object))
       (string! %stream "/>"))
      ((stringp (car (xml.children object)))
       (char! %stream #\>)
       (write-stream stream (car (xml.children object)))
       (outro! stream object))
      (t
       (char! %stream #\>)
       (increase-indent %stream)
       (reduce #'child!
	       (slot-value object 'children) 
	       :initial-value stream)
       (decrease-indent %stream)
       (char! %stream #\Newline)
       (outro! stream object)))
    stream))

(deftrace xml-render
    '(intro! outro! child! write-stream attribute!))

;;---------------------------------------------------------------------------
;; Relaxed XML Parser
;;---------------------------------------------------------------------------
;; This is a duplicate of the xml parser that is slow but allows
;; us to parse some of the broken xml's like HTML.
(defrule relaxed-xml-attribute-name? (c attribute namespace)
  (:or (:type alphanum? c)
	     (:and #\- (:do (setq c #\-))))
  (:and (:do (setq attribute (make-accumulator)))
	(:collect c attribute))
  (:zom (:or (:type alphanum? c)
	     (:and #\- (:do (setq c #\-))))
	(:collect c attribute))  
  (:optional
   #\:
   (:do (setq namespace attribute)
	(setq attribute (make-accumulator)))
   (:oom (:or (:type alphanum? c)
	      (:and #\- (:do (setq c #\-))))
	 (:collect c attribute)))
  (:return (if namespace
	       (values attribute namespace)
	       attribute)))

(defrule relaxed-xml-attribute-value? (c val)
  (:or (:and
	#\"
	(:do (setq val (make-accumulator :byte)))
	(:zom (:checkpoint
	       #\" (:return (octets-to-string val :utf-8)))
	      (:checkpoint
	       #\> (:rewind-return (octets-to-string val :utf-8)))
	      ;; (:type (or visible-char? space?) c)
	      (:type octet? c)
	      (:collect c val)))
       (:and
	#\'
	(:do (setq val (make-accumulator :byte)))
	(:zom (:checkpoint #\'
		(:return (octets-to-string val :utf-8)))
	      (:checkpoint #\>
		(:rewind-return (octets-to-string val :utf-8)))
	      ;; (:type (or visible-char? space?) c)
	      (:type octet? c)
	      (:collect c val)))
       (:zom (:checkpoint (:or #\> #\/)
			  (:rewind-return val))
	     (:type visible-char? c)
	     (:collect c val)))
  (:return (octets-to-string val :utf-8)))

(defrule relaxed-xml-attribute? (name value)
  (:relaxed-xml-attribute-name? name)
  #\=
  (:relaxed-xml-attribute-value? value)
  (:return (cons name value)))

(defrule relaxed-xml-tag-name? (tag namespace)
  (:relaxed-xml-attribute-name? tag namespace)
  (:return (values tag namespace)))

(defrule relaxed-xml-lwsp? (c)
  (:oom (:or (:and (:type (or space? tab?)))
	     (:and (:type (or carriage-return? linefeed?))
		   (:do (setf c t))))
  (:if c
       (:return #\Newline)
       (:return #\Space))))

(defrule relaxed-xml-text-node? (c acc)
  (:not #\<)
  (:checkpoint #\< (:rewind-return (octets-to-string acc :utf-8)))
  (:or (:and (:seq "&gt;") (:do (setf c #\<)))
       (:and (:seq "&lt;") (:do (setf c #\>)))
       (:relaxed-xml-lwsp? c) (:type octet? c))
  (:do (setq acc (make-accumulator :byte)))
  (:collect c acc)
  (:zom (:checkpoint #\< (:rewind-return (octets-to-string acc :utf-8)))
	(:or (:and (:seq "&gt;") (:do (setf c #\<)))
	     (:and (:seq "&lt;") (:do (setf c #\>)))
	     (:relaxed-xml-lwsp? c) (:type octet? c))
	(:collect c acc))
  (:if (> (length acc) 0)
       (:return (octets-to-string acc :utf-8))))

(defrule relaxed-xml-cdata? (c acc)
  (:seq "<![CDATA[")
  (:do (setq acc (make-accumulator)))
  (:zom (:not (:seq "]]>"))
	(:type octet? c)
	(:collect c acc))
  (:return acc))

(defrule relaxed-xml-comment? (c acc)
  (:seq "<!--")
  (:do (setq acc (make-accumulator)))
  (:collect #\< acc) (:collect #\! acc)
  (:collect #\- acc) (:collect #\- acc)
  (:zom (:not (:seq "-->")) (:type octet? c) (:collect c acc))
  (:collect #\- acc) (:collect #\- acc) (:collect #\> acc)
  (:return acc))

(defparser relaxed-xml-lexer? (tag namespace attr attrs child children)
  (:lwsp?)
  (:checkpoint (:seq "<?xml")
	       (:zom (:not #\>) (:type octet?)) (:lwsp?) (:commit))
  (:checkpoint (:seq "<!DOCTYPE")
	       (:zom (:not #\>) (:type octet?)) (:lwsp?) (:commit))
  #\<
  (:relaxed-xml-tag-name? tag namespace)
  (:zom (:lwsp?)
	(:relaxed-xml-attribute? attr)
	(:do (push attr attrs)))
  (:or (:and (:lwsp?)
	     (:seq "/>")
	     (:return (list tag namespace (nreverse attrs))))
       (:and #\>
	     (:zom (:lwsp?)
		   (:or
		    (:checkpoint
		     (:seq "</")
		     (:if namespace
			  (:not (:and (:sci namespace) #\: (:sci tag)))
			  (:not (:sci tag)))
		     (:rewind-return (list* tag namespace (nreverse attrs)
					    (nreverse children))))
		    (:relaxed-xml-lexer? child)
		    (:relaxed-xml-comment? child)
		    (:relaxed-xml-text-node? child)
		    (:relaxed-xml-cdata? child))
		   (:do (push child children)))
	     (:seq "</")
	     (:if namespace
		  (:and (:sci namespace) #\: (:sci tag))
		  (:sci tag))
	     #\>
	     (:return (list* tag namespace (nreverse attrs)
			     (nreverse children))))))

;; +------------------------------------------------------------------------
;; | Relaxed XML Stream
;; +------------------------------------------------------------------------
(defclass relaxed-xml-stream (xml-stream)
  ())

(defun make-relaxed-xml-stream (stream)
  (make-instance 'relaxed-xml-stream :stream stream))

(defmethod read-stream ((stream relaxed-xml-stream))
  (parse-xml (relaxed-xml-lexer? (slot-value stream '%stream))))

(deftrace xml-parsers
    '(xml-tag-name? xml-lexer? xml-comment? xml-text-node?
      xml-cdata? %xml-lexer? xml-lwsp? xml-attribute?
      xml-attribute-name? xml-attribute-value?
      relaxed-xml-tag-name? relaxed-xml-lexer? relaxed-xml-comment?
      relaxed-xml-text-node? relaxed-xml-cdata? relaxed-xml-lwsp?
      relaxed-xml-attribute? relaxed-xml-attribute-name?
      relaxed-xml-attribute-value?))
