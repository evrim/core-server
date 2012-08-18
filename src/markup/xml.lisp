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
  (any (lambda (a) (and (typep a 'xml+) (xml+.tag a))) 
       (class+.superclasses (class-of xml))))

(defmethod xml.attributes ((xml xml))
  (any (lambda (a) (and (typep a 'xml+) (xml+.attributes a)))
       (class-superclasses (class-of xml))))

(defmethod xml.namespace ((xml xml))  
  (any (lambda (a) (and (typep a 'xml+) (xml+.namespace a)))
       (class-superclasses (class-of xml))))

(defmethod xml.attribute ((xml xml) attribute)
  (slot-value xml attribute))

(defmethod xml.equal ((a xml) (b xml))
  (and
   (string= (xml.tag a) (xml.tag b))
   (string= (xml.namespace a) (xml.namespace b))
   (reduce (lambda (acc attr)
	     (and acc (xml.equal (xml.attribute a attr)
				 (xml.attribute a attr))))
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

(defun filter-xml-nodes (root goal-p)
  (cond
    ((stringp root) root)
    (t
     (let ((ctor (core-server::class+.ctor-name (class-of root))))
       (apply ctor
	      (append (reduce0
		       (lambda (acc attribute)
			 (aif (xml.attribute root attribute)
			      (cons (make-keyword attribute)
				    (cons it acc))
			      acc))
		       (xml.attributes root))
		      (nreverse
		       (reduce0 (lambda (acc child)
				  (if (and (not (stringp child))
					   (funcall goal-p child))
				      acc
				      (cons (filter-xml-nodes child goal-p)
					    acc)))
				(xml.children root)))))))))

;; -------------------------------------------------------------------------
;; XML defining macro: defmxl
;; -------------------------------------------------------------------------
(defmacro defxml (name &rest attributes)
  `(defclass+ ,name (xml)
     (,@(mapcar (lambda (attr) (list attr :print t))
		attributes))
     (:metaclass xml+)
     (:tag ,@(string-downcase (symbol-name name)))
     (:namespace ,@(string-downcase
		    (subseq (package-name (symbol-package name)) 1)))
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
(defatom xml-attribute-char? ()
  (or (alphanum? c) (= c #.(char-code #\-)) (= c #.(char-code #\_))))

(defrule xml-attribute-name? (c attribute namespace)
  (:type xml-attribute-char? c)
  (:do (setq attribute (make-accumulator)))
  (:collect c attribute)
  (:zom (:type xml-attribute-char? c) (:collect c attribute))  
  (:or (:and #\:
	     (:do (setq namespace attribute)
		  (setq attribute (make-accumulator)))
	     (:oom (:type xml-attribute-char? c)
		   (:collect c attribute))
	     (:return (if namespace
			  (values attribute namespace)
			  attribute)))
       (:return (if namespace
		    (values attribute namespace)
		    attribute))))

(defrule xml-attribute-value? (c val)
  (:or (:and (:quoted? val) (:return val))
       (:and (:oom (:not #\Space) (:not #\>) (:not #\/)
		   (:not #\") (:not #\')
		   (:do (setq val (make-accumulator :byte)))
		   (:type octet? c) (:collect c val))
	     (:return (if val (octets-to-string val :utf-8))))))

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
   (:and (:seq "&quot;") (:do (setf c #\")))
   (:xml-lwsp? c)
   (:type octet? c))
  (:do (setq acc (make-accumulator :byte)))
  (:collect c acc)
  (:zom (:not #\<)
	;; (:checkpoint #\< (:rewind-return (octets-to-string acc :utf-8)))
	(:or (:and (:seq "&gt;") (:do (setf c #\<)))
	     (:and (:seq "&lt;") (:do (setf c #\>)))
	     (:and (:seq "&quot;") (:do (setf c #\")))
	     ;; (:and #\& (:or (:and (:seq "gt;") (:do (setf c #\<)))
	     ;; 	  (:and (:seq "lt;") (:do (setf c #\>)))))
	     (:xml-lwsp? c)
	     (:type octet? c)) 
	(:collect c acc))
  (:if (> (length acc) 0)
       (:return (octets-to-string acc :utf-8))))

(defrule xml-cdata? (c acc)
  (:seq "CDATA[")
  (:do (setq acc (make-accumulator :byte)))
  (:zom (:not (:seq "]]>")) (:type octet? c) (:collect c acc))
  (:return (octets-to-string acc :utf-8)))

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
  (:or
   (:and (:lwsp?) (:seq "/>")
	 (:return (values tag namespace (nreverse attrs))))
   (:and #\>
	 (:checkpoint
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
		 (:and (:%xml-lexer? a b c d)
		       (:do (push (list* a b c d) children)))
		 (:and (:xml-text-node? child)
		       (:do (push child children))
		       ;; #\/
		       ;; (:if namespace
		       ;;      (:and (:sci namespace) #\: (:sci tag))
		       ;;      (:sci tag))
		       ;; #\>
		       ;; (:return (values tag namespace (nreverse attrs)
		       ;; 		   (nreverse children)))
		       )
		 ))
	  (:or (:and #\/
		     (:if namespace
			  (:and (:sci namespace) #\: (:sci tag))
			  (:sci tag))
		     #\>
		     (:return (values tag namespace (nreverse attrs)
				      (nreverse children))))
	       (:rewind-return (values tag namespace (nreverse attrs))))))))

(defparser xml-lexer? (tag namespace attrs children child)
  (:lwsp?)
  (:checkpoint (:seq "<?xml")
	       (:zom (:not #\>) (:type octet?)) (:lwsp?) (:commit))
  (:checkpoint (:seq "<!DOCTYPE")
	       (:zom (:not #\>) (:type octet?)) (:lwsp?) (:commit))
  #\<
  (:optional #\! (:or (:xml-cdata? child) (:xml-comment? child))
	     (:lwsp?) #\<)
  (:%xml-lexer? tag namespace attrs children)
  (:return (list* tag namespace attrs children)))

(defvar +xml-namespace+ (find-package :<))
(defparameter +xml-namespaces-table+
  (list (cons "http://labs.core.gen.tr/2011/DB" (find-package :<db))
	(cons "http://labs.core.gen.tr/2011/API" (find-package :<core-server))
	(cons "http://www.w3.org/2001/XMLSchema" (find-package :<xs))
	(cons "http://www.w3.org/2005/Atom" (find-package :<atom))
	(cons "http://schemas.google.com/photos/2007" (find-package :<gphoto))
	(cons "http://search.yahoo.com/mrss/" (find-package :<media))
	(cons "http://www.opensearch.org/Specifications/OpenSearch/1.1"
	      (find-package :<open-search))
	(cons "http://wordpress.org/export/1.0/" (find-package :<wordpress))
	(cons "http://purl.org/rss/1.0/modules/content/" (find-package :<content))
	(cons "http://purl.org/dc/elements/1.1/" (find-package :<dc))
	(cons "http://wordpress.org/export/1.0/excerpt/" (find-package :<excerpt))))

(defun register-xml-namespace (namespace package)
  (let ((package (find-package package)))
    (assert (not (null package)))
    (setf +xml-namespaces-table+
	  (cons (cons namespace package)
		(remove namespace +xml-namespaces-table+ :key #'car :test #'equal)))))

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

(defun parse-xml (xml &optional default-namespace)
  (labels ((make-generic-element (tag namespace attributes children)
	     (warn "<~A:~A> tag not found, using generic xml element."
		   namespace tag)
	     (apply #'xml tag namespace
		    (cons attributes (mapcar #'parse-xml children))))
	   (make-element (symbol attributes children)
	     (apply symbol 
		    (append
		     (reduce0 (lambda (acc attr)
				(cons (js->keyword (car attr))
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
		   ((and default-namespace
			 (find-package
			  (make-keyword
			   (format nil "<~A" 
				   (symbol-name
				    (xml->symbol default-namespace))))))
		    it)
		   (t +xml-namespace+)))
		 (symbol (let ((symbol1 (xml->symbol tag +xml-namespace+))
			       (symbol2 (intern tag +xml-namespace+)))
			   (if (fboundp symbol1)
			       symbol1
			       (if (fboundp symbol2)
				   symbol2)))))
	    (if (and symbol
		     (not (eq (symbol-package symbol) #.(find-package :cl)))
		     ;; (not (eq (symbol-package symbol) #.(find-package :arnesi)))
		     )
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
  ((namespace :initform nil :initarg :namespace :reader namespace)))

(defun make-xml-stream (stream &optional namespace)
  (make-instance 'xml-stream :stream stream
		 :namespace namespace))

(defmethod read-stream ((stream xml-stream))
  (parse-xml (xml-lexer? (slot-value stream '%stream))
	     (namespace stream)))

(defmethod write-stream ((stream xml-stream) (list list))
  (reduce #'write-stream list :initial-value stream))

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
      (if (and namespace (not (equal namespace (namespace stream))))
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
      (if (and namespace (not (equal (namespace stream) namespace)))
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
      ((eq 1 (length (xml.children object)))
       (stringp (car (xml.children object)))
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
;;http://msdn.microsoft.com/en-us/library/ms537495.aspx
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defparameter +xml-entities+
    '(("gt" . #\<)
      ("lt" . #\>)
      ("quot" . #\")
      ("nbsp" . #\Space)
      ("amp" . #\&)
      ("Uuml" . #\Ü)
      ("Ouml" . #\Ö)
      ("Ccedil" . #\Ç)
      ("uuml" . #\ü)
      ("ouml" . #\ö)
      ("ccedil" . #\ç))))

(defmacro defxml-entity-parser (entities)
  `(defparser xml-entity? ()
     (:or ,@(mapcar (lambda (e)
		      `(:and (:seq ,(format nil "~A;" (car e)))
			     (:return ,(cdr e))))
		    (symbol-value entities)))))

(defxml-entity-parser +xml-entities+)

(defrule relaxed-xml-text-node? (c (acc (make-accumulator :byte)))
  (:debug)
  (:not #\<)
  ;; (:checkpoint #\< (:rewind-return (octets-to-string acc :utf-8)))
  (:oom (:checkpoint #\< (:rewind-return (octets-to-string acc :utf-8)))
	(:or (:and #\& (:xml-entity? c)
		   (:do (reduce (lambda (acc a)
				  (push-atom a acc)
				  acc)
				(string-to-octets (format nil "~A" c) :utf-8)
				:initial-value acc))) 
	     (:and (:or (:xml-lwsp? c)
			(:type octet? c)) (:collect c acc))))
  (:if (> (length acc) 0)
       (:return (octets-to-string acc :utf-8))))

(defrule relaxed-xml-comment? (c acc)
  (:seq "<!--")
  (:do (setq acc (make-accumulator :byte)))
  (:collect #\< acc) (:collect #\! acc)
  (:collect #\- acc) (:collect #\- acc)
  (:zom (:not (:seq "-->")) (:type octet? c) (:collect c acc))
  (:collect #\- acc) (:collect #\- acc) (:collect #\> acc)
  (:return (octets-to-string acc :utf-8)))

(defparser %relaxed-xml-lexer? (tag namespace attr attrs child children
				    immediate)
  #\<
  (:xml-tag-name? tag namespace)
  (:do (describe (list tag namespace)))
  (:debug)
  (:zom (:lwsp?) (:xml-attribute? attr)
	(:do (push attr attrs)))
  (:or (:and (:lwsp?)
	     (:seq "/>")
	     (:return (list tag namespace (nreverse attrs))))
       (:and #\>
	     (:zom (:lwsp?)
		   (:or
		    (:debug)
		    (:checkpoint
		     (:seq "</")
		     (:if namespace
			  (:not (:and (:sci namespace) #\: (:sci tag)))
			  (:not (:sci tag)))
		     (:debug)
		     (:do (describe (list 'foo tag namespace children) ))
		     (:rewind-return nil ;; (values
				     ;;  (list* tag namespace (nreverse attrs)
				     ;; 	     (nreverse children))
				     ;;  t)
				     ))
		    (:oom (:%relaxed-xml-lexer? child)
			  (:do (push child children)))
		    (:relaxed-xml-comment? child)
		    (:xml-text-node? child)
		    (:xml-cdata? child))
		   (:do (push child children)))
	     (:or (:and (:seq "</")
			(:if namespace
			     (:and (:sci namespace) #\: (:sci tag))
			     (:sci tag))
			#\>
			(:return (list* tag namespace (nreverse attrs)
					(nreverse children))))
		  (:return (list* tag namespace (nreverse attrs)
				  (nreverse children)))))))

(defparser relaxed-xml-lexer? (tag namespace attr attrs child children)
  (:lwsp?)
  (:checkpoint (:seq "<?xml")
	       (:zom (:not #\>) (:type octet?)) (:lwsp?) (:commit))
  (:checkpoint (:sci "<!DOCTYPE")
	       (:zom (:not #\>) (:type octet?)) (:lwsp?) (:commit))
  (:zom (:lwsp?) (:relaxed-xml-comment? child) (:lwsp?))
  (:%relaxed-xml-lexer? tag)
  (:return tag))

;; +------------------------------------------------------------------------
;; | Relaxed XML Stream
;; +------------------------------------------------------------------------
(defclass relaxed-xml-stream (xml-stream)
  ())

(defun make-relaxed-xml-stream (stream &optional namespace)
  (make-instance 'relaxed-xml-stream :stream stream
		 :namespace namespace))

(defmethod read-stream ((stream relaxed-xml-stream))
  (parse-xml (xml-lexer? (slot-value stream '%stream))
	     (namespace stream)))

(defmethod write-stream ((stream relaxed-xml-stream) (object xml))
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
      ;; ((null (xml.children object))
      ;;  (string! %stream "/>"))
      ((eq 1 (length (xml.children object)))
       (stringp (car (xml.children object)))
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

(defmethod write-stream ((stream relaxed-xml-stream) (object generic-xml))
  (with-slots (%stream) stream
    (intro! stream object)
    (reduce #'attribute!
	    (slot-value object 'attributes)
	    :initial-value stream)
    (cond
      ;; ((null (xml.children object))
      ;;  (string! %stream "/>"))
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

;; -------------------------------------------------------------------------
;; Trace Definition
;; -------------------------------------------------------------------------
(deftrace xml-parsers
    '(xml-tag-name? xml-lexer? xml-comment? xml-text-node?
      xml-cdata? %xml-lexer? xml-lwsp? xml-attribute?
      xml-attribute-name? xml-attribute-value?
      xml-tag-name? relaxed-xml-lexer? relaxed-xml-comment?
      relaxed-xml-text-node? xml-cdata? xml-lwsp?
      xml-attribute? xml-attribute-name?
      xml-attribute-value? parse-xml))
