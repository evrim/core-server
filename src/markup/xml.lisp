;; +----------------------------------------------------------------------------
;; | XML Base
;; +----------------------------------------------------------------------------
(in-package :core-server)

;; ----------------------------------------------------------------------------
;; Protocol
;; ----------------------------------------------------------------------------
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

;; ----------------------------------------------------------------------------
;; XML Metaclass
;; ----------------------------------------------------------------------------
(defclass xml+ (class+)
  ((tag :initform nil :initarg :tag)
   (namespace :initform nil :initarg :namespace)
   (attributes :initarg :attributes :initform nil)))

(defmethod xml+.attributes ((self xml+))
  (car (slot-value self 'attributes)))

(defmethod xml+.tag ((self xml+))
  (car (slot-value self 'tag)))

(defmethod xml+.namespace ((self xml+))
  (car (slot-value self 'namespace)))

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

;; ----------------------------------------------------------------------------
;; XML Base Class
;; ----------------------------------------------------------------------------
(defclass xml ()
  ((children :initform nil :initarg :children :accessor xml.children)))

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

;; ----------------------------------------------------------------------------
;; XML defining macro: defmxl
;; ----------------------------------------------------------------------------
(defmacro defxml (name &rest attributes)
  `(defclass+ ,name (xml)
     (,@(mapcar (lambda (attr)
		  (list attr :print t))
		attributes))
     (:metaclass xml+)
     (:tag ,(string-downcase (symbol-name name)))
     (:namespace ,(string-downcase (subseq (car (package-nicknames (symbol-package name))) 1)))
     (:attributes ,attributes)))

;; ----------------------------------------------------------------------------
;; XML Generic Class
;; ----------------------------------------------------------------------------
(defclass generic-xml (xml)
  ((tag :initarg :tag :initform nil :accessor xml.tag)
   (namespace :initarg :namespace :initform nil :accessor xml.namespace)
   (attributes :initarg :attributes :initform nil)))

(defmethod xml.attributes ((xml generic-xml))
  (mapcar #'car (slot-value xml 'attributes)))

(defmethod xml.attribute ((xml generic-xml) attribute)
  (cdr (assoc attribute (slot-value xml 'attributes))))

;; ----------------------------------------------------------------------------
;; Generic XML Constructor
;; ----------------------------------------------------------------------------
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

;;-----------------------------------------------------------------------------
;; XML Parser
;;-----------------------------------------------------------------------------
(defrule xml-attribute-name? (c (attribute (make-accumulator)) namespace)
  (:oom (:or (:type alphanum? c)
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
	       (values attribute namespace);;  (format nil "~A:~A" namespace attribute)
	       attribute)))

(defrule xml-attribute-value? (c (val (make-accumulator)))
  (:or (:and
	#\"
	(:zom (:checkpoint #\" (:return val))
	      (:checkpoint #\> (:rewind-return val))
	      (:type (or visible-char? space?) c)
	      (:collect c val)))
       (:and
	#\'
	(:zom (:checkpoint #\' (:return val))
	(:checkpoint #\> (:rewind-return val))
	(:type (or visible-char? space?) c)
	(:collect c val)))
       (:zom (:checkpoint (:or #\> #\/)
			  (:rewind-return val))
	     (:type visible-char? c)
	     (:collect c val)))
  (:return val))

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

(defrule xml-text-node? (c (acc (make-accumulator :byte)))
  (:not #\<)
  (:oom (:checkpoint #\< (:rewind-return (octets-to-string acc :utf-8)))
	(:or (:and (:seq "&gt;") (:do (setf c #\<)))
	     (:and (:seq "&lt;") (:do (setf c #\>)))
	     (:xml-lwsp? c) (:type octet? c))
	(:collect c acc))
  (:if (> (length acc) 0)
       (:return (octets-to-string acc :utf-8))))

(defrule xml-cdata? (c (acc (make-accumulator)))
  (:seq "<![CDATA[")
  (:zom (:not (:seq "]]>"))
	(:type octet? c)
	(:collect c acc))
  (:return acc))

(defrule xml-comment? (c (acc (make-accumulator)))
  (:seq "<!--")
  (:collect #\< acc) (:collect #\! acc)
  (:collect #\- acc) (:collect #\- acc)
  (:zom (:not (:seq "-->")) (:type octet? c) (:collect c acc))
  (:collect #\- acc) (:collect #\- acc) (:collect #\> acc)
  (:return acc))

(defparser xml-lexer? (tag namespace attr attrs child children)
  (:lwsp?)
  (:checkpoint (:seq "<?xml")
	       (:zom (:not #\>) (:type octet?)) (:lwsp?) (:commit))
  (:checkpoint (:seq "<!DOCTYPE")
	       (:zom (:not #\>) (:type octet?)) (:lwsp?) (:commit))  
  #\<
  (:xml-tag-name? tag namespace)
  (:zom (:lwsp?)
	(:xml-attribute? attr)
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
		    (:xml-lexer? child)
		    (:xml-comment? child)
		    (:xml-text-node? child)
		    (:xml-cdata? child))
		   (:do (push child children)))
	     (:seq "</")
	     (:if namespace
		  (:and (:sci namespace) #\: (:sci tag))
		  (:sci tag))
	     #\>
	     (:return (list* tag namespace (nreverse attrs) (nreverse children))))))


(deftrace xml-parsers
    '(xml-tag-name? xml-lexer? xml-comment? xml-text-node?
      xml-cdata?))

;; FIXME: clean this mess up -evrim.
(defun parse-xml (xml)
  (if (atom xml)
      xml
      (destructuring-bind (tag namespace attributes &rest children) xml
	(cond
	  (namespace
	   (let ((package (find-package (make-keyword (format nil "<~A" namespace)))))
	     (if package
		 (let ((symbol (intern (string-upcase tag) package)))
		   (if (fboundp symbol)
		       (let ((instance (apply symbol 
					      (append
					       (reduce0 (lambda (acc attr)
							  (cons (make-keyword (car attr))
								(cons (cdr attr) acc)))
							attributes)
					       (mapcar #'parse-xml children)))))
			 (if (slot-exists-p instance 'tag)
			     (setf (slot-value instance 'tag) tag))
			 (if (slot-exists-p instance 'namespace)
			     (setf (slot-value instance 'namespace) namespace))
			 instance)
		       
		       (apply #'xml tag namespace
			      (cons attributes (mapcar #'parse-xml children)))))
		 (apply #'xml tag namespace
			(cons attributes (mapcar #'parse-xml children))))))
	  (t
	   (let ((symbol (intern (string-upcase tag) (find-package :<))))
	     (if (fboundp symbol)
		 (let ((instance (apply symbol 
					(append
					 (reduce0 (lambda (acc attr)
						    (cons (make-keyword (car attr))
							  (cons (cdr attr) acc)))
						  attributes)
					 (mapcar #'parse-xml children)))))
		   (if (slot-exists-p instance 'tag)
		       (setf (slot-value instance 'tag) tag))
		   (if (slot-exists-p instance 'namespace)
		       (setf (slot-value instance 'namespace) namespace))
		   instance)
		 (apply #'xml tag namespace
			(cons attributes (mapcar #'parse-xml children))))))))))

;; +----------------------------------------------------------------------------
;; | XML Stream
;; +----------------------------------------------------------------------------
(defclass xml-stream (wrapping-stream)
  ())

(defun make-xml-stream (stream)
  (make-instance 'xml-stream :stream stream))

(defmethod read-stream ((stream xml-stream))
  (parse-xml (xml-lexer? (slot-value stream '%stream))))

(defmethod write-stream ((stream xml-stream) (string string))
  (string! (slot-value stream '%stream)
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
  		   :initial-value (make-accumulator))))

(defmethod write-stream ((xml-stream xml-stream) (object xml))
  (flet ((intro! (stream)
	   (let ((tag (xml.tag object))
		 (namespace (xml.namespace object)))
	     (if namespace
		 (progn
		   (string! stream namespace)
		   (char! stream #\:)
		   (string! stream tag))
		 (string! stream tag))))
	 (attribute! (stream attribute)
	   (char! stream #\Space)
	   (if (symbolp (car attribute))
	       (string! stream (symbol-to-js (car attribute)))
	       (string! stream (car attribute)))
	   (char! stream #\=)
	   (quoted! stream (format nil "~A" (cdr attribute)))
	   stream)
	 (child! (stream child)
	   (increase-indent stream)
	   (char! stream #\Newline)	   
	   (write-stream xml-stream child)
	   (decrease-indent stream)
	   stream)
	 (outro! (stream)
	   (char! stream #\Newline)
	   (let ((tag (xml.tag object))
		 (namespace (xml.namespace object)))
	     (string! stream "</")
	     (if namespace
		 (progn
		   (string! stream namespace)
		   (char! stream #\:)
		   (string! stream tag))
		 (string! stream tag))
	     (char! stream #\>))))
    (outro!
     (reduce #'child! (slot-value object 'children)
	     :initial-value
	     (char!
	      (reduce
	       #'attribute!
	       (reduce0 (lambda (acc slot)
			  (aif (slot-value object slot)
			       (cons (cons slot it) acc)
			       acc))
			(xml.attributes object))
	       :initial-value (intro!
			       (char!
				(slot-value xml-stream '%stream)
				#\<)))
	      #\>)))
    xml-stream))

(defmethod write-stream ((xml-stream xml-stream) (object generic-xml))
  (flet ((intro! (stream)
	   (let ((tag (slot-value object 'tag))
		 (namespace (slot-value object 'namespace)))
	     (if namespace
		 (progn
		   (string! stream namespace)
		   (char! stream #\:)
		   (string! stream tag))
		 (string! stream tag))))
	 (attribute! (stream attribute)
	   (char! stream #\Space)
	   (if (symbolp (car attribute))
	       (symbol! stream (car attribute))
	       (string! stream (car attribute)))
	   (char! stream #\=)
	   (quoted! stream (cdr attribute))
	   stream)
	 (child! (stream child)
	   (if (stringp child)
	       (string! stream child)
	       (write-stream xml-stream child))
	   stream)
	 (outro! (stream)
	   (let ((tag (slot-value object 'tag))
		 (namespace (slot-value object 'namespace)))
	     (string! stream "</")
	     (if namespace
		 (progn
		   (string! stream namespace)
		   (char! stream #\:)
		   (string! stream tag))
		 (string! stream tag))
	     (char! stream #\>))))
    (outro!
     (reduce #'child! (slot-value object 'children)
	     :initial-value
	     (char!
	      (reduce #'attribute! (slot-value object 'attributes)
		      :initial-value (intro!
				      (char!
				       (slot-value xml-stream '%stream)
				       #\<)))
	      #\>)))
    xml-stream))


;; (defxml <db:moo a b c)

;; (defmethod serialize ((self abstract-database) (object null))
;;   (<db:null))

;; (defmethod %deserialize ((self abstract-database) (tag <db:null))
;;   nil)

;; (defmethod deserialize ((self abstract-database))
;;   (let ((tag (xml? (database.stream self))))
;;     (%deserialize self tag)))

;; (defclass core-pseudo-stream (core-stream)
;;   ((%stream :initarg :stream :reader %stream)))

;; (defmethod return-stream ((stream core-pseudo-stream))
;;   (return-stream (%stream stream)))

;; (defmethod checkpoint-stream ((stream core-pseudo-stream))
;;   (checkpoint-stream (%stream stream)))

;; (defmethod commit-stream ((stream core-pseudo-stream))
;;   (commit-stream (%stream stream)))

;; (defmethod rewind-stream ((stream core-pseudo-stream))
;;   (rewind-stream (%stream stream)))

;; (defmethod close-stream ((stream core-pseudo-stream))
;;   (close-stream (%stream stream)))

;; (defmethod transacitonalp ((stream core-pseudo-stream))
;;   (transactionalp (%stream stream)))

;; (defclass core-xml-stream (core-pseudo-stream)
;;   ())

;; (defun make-xml-stream (stream)
;;   (make-instance 'core-xml-stream :stream stream))

;; (defmethod write-stream ((stream core-xml-stream) (object t))
;;   (write-stream stream (to-markup object)))

;; (defmethod write-stream ((stream core-xml-stream) (markup markup))
;;   (prog1 stream
;;     (flet ((write-tag ()
;; 	     (write-stream (%stream stream) (slot-value markup 'namespace))
;; 	     (write-stream (%stream stream) #\:)
;; 	     (write-stream (%stream stream) (slot-value markup 'tag))))
;;       (write-stream (%stream stream) #\<)
;;       (write-tag)

;;       (mapc (lambda (attribute)
;; 	      (write-stream (%stream stream) #\Space)
;; 	      (write-stream (%stream stream) (car attribute))
;; 	      (write-stream (%stream stream) "=\"")
;; 	      (write-stream (%stream stream) (cdr attribute))
;; 	      (write-stream (%stream stream) #\"))
;; 	    (slot-value markup 'attributes))

;;       (let ((children (slot-value markup 'children)))
;; 	(cond
;; 	  ((null children)
;; 	   (write-stream (%stream stream) "/>"))
;; 	  ((= 1 (length children))
;; 	   (write-stream stream (car children))
;; 	   (write-stream (%stream stream) "</")
;; 	   (write-tag)
;; 	   (write-stream (%stream stream) #\>))
;; 	  (t
;; 	   (mapcar (lambda (child)
;; 		     (write-stream stream child))
;; 		   children)
;; 	   (write-stream (%stream stream) "</")
;; 	   (write-tag)
;; 	   (write-stream (%stream stream) #\>)))))))

;; ;;-----------------------------------------------------------------------------
;; ;; XML Parser
;; ;;-----------------------------------------------------------------------------
;; (defrule xml-attribute-name? (c (attribute (make-accumulator)) namespace)
;;   (:oom (:or (:type alphanum? c)
;; 	     (:and #\- (:do (setq c #\-))))
;; 	(:collect c attribute))  
;;   (:optional
;;    #\:
;;    (:do (setq namespace attribute)
;; 	(setq attribute (make-accumulator)))
;;    (:oom (:or (:type alphanum? c)
;; 	      (:and #\- (:do (setq c #\-))))
;; 	 (:collect c attribute)))
;;   (:return (if namespace
;; 	       (format nil "~A:~A" namespace attribute)
;; 	       attribute)))

;; (defrule xml-attribute-value? (c (val (make-accumulator)))
;;   (:or (:and
;; 	#\"
;; 	(:zom (:checkpoint #\" (:return val))
;; 	(:checkpoint #\> (:rewind-return val))
;; 	(:type (or visible-char? space?) c)
;; 	(:collect c val)))
;;        (:and
;; 	#\'
;; 	(:zom (:checkpoint #\' (:return val))
;; 	(:checkpoint #\> (:rewind-return val))
;; 	(:type (or visible-char? space?) c)
;; 	(:collect c val))))
;;   (:return val))

;; (defrule xml-attribute? (name value)
;;   (:dom-attribute-name? name)
;;   #\=
;;   (:dom-attribute-value? value)
;;   (:return (cons name value)))

;; (defrule xml-tag-name? (name)
;;   (:dom-attribute-name? name)
;;   (:return name))

;; (defrule xml-lwsp? (c)
;;   (:oom (:or (:and (:type (or space? tab?)))
;; 	     (:and (:type (or carriage-return? linefeed?))
;; 		   (:do (setf c t))))
;;   (:if c
;;        (:return #\Newline)
;;        (:return #\Space))))

;; (defrule xml-text-node? (c (acc (make-accumulator)))
;;   (:not #\<)
;;   (:oom (:checkpoint #\< (:rewind-return acc))
;; 	(:or (:dom-lwsp? c)
;; 	     (:type octet? c))
;; 	(:collect c acc))
;;   (:if (> (length acc) 0)
;;        (:return acc)))

;; (defrule xml-cdata? (c (acc (make-accumulator)))
;;   (:seq "<![CDATA[")
;;   (:zom (:not (:seq "]]>"))
;; 	(:type octet? c)
;; 	(:collect c acc))
;;   (:return acc))

;; (defrule xml-comment? (c (acc (make-accumulator)))
;;   (:seq "<!--")
;;   (:collect #\< acc) (:collect #\! acc)
;;   (:collect #\- acc) (:collect #\- acc)
;;   (:zom (:not (:seq "-->"))
;; 	(:type octet? c)
;; 	(:collect c acc))
;;   (:collect #\- acc) (:collect #\- acc) (:collect #\> acc)
;;   (:return acc))

;; ;;-----------------------------------------------------------------------------
;; ;; Generic Document Parser
;; ;;-----------------------------------------------------------------------------
;; (defparser xml-element? (tag namespace attr attrs child children)
;;   (:lwsp?)
;;   (:checkpoint (:seq "<?xml")
;; 	       (:zom (:not #\>) (:type octet?)) (:lwsp?) (:commit))
;;   (:checkpoint (:seq "<!DOCTYPE")
;; 	       (:zom (:not #\>) (:type octet?)) (:lwsp?) (:commit))  
;;   #\<
;;   (:xml-tag-name? tag namespace)
;;   (:zom (:lwsp?)
;; 	(:xml-attribute? attr)
;; 	(:do (push attr attrs)))
;;   (:or (:and (:lwsp?)
;; 	     (:seq "/>")
;; 	     (:return (make-markup namespace tag (nreverse attrs))))
;;        (:and #\>
;; 	     (:zom (:lwsp?)
;; 		   (:or (:xml-element? child)
;; 			(:xml-comment? child)
;; 			(:xml-text-node? child)
;; 			(:xml-cdata? child))
;; 		   (:do (push child children)))
;; 	     (:seq "</")
;; 	     (:if namespace
;; 		  (:and (:sci namespace) #\: (:sci tag))
;; 		  (:sci tag))
;; 	     #\>
;; 	     (:return (make-markup namespace tag (nreverse attrs) (nreverse children))))))

;; (defmethod read-stream ((stream core-xml-stream))
;;   (let ((markup (xml-element? (%stream stream))))
;;     (if markup
;; 	(from-markup markup))))

;; ;; ----------------------------------------------------------------------------
;; ;; Default Write Method for XML Stream
;; ;; ----------------------------------------------------------------------------
;; (defmethod write-stream ((stream xml-stream-mixin) (object dom-element))
;;   (dom-element! stream object))

;; (defmethod write-stream ((stream xml-stream-mixin) (object null))
;;   (write-stream stream (make-dom-element "null" "cl" (list))))

;; (defmethod write-stream ((stream xml-stream-mixin) (object (eql 't)))
;;   (call-next-method stream "<true/>"))

;; (defmethod write-stream ((stream xml-stream-mixin) (object symbol))
;;   (call-next-method stream "<symbol>")
;;   (symbol-with-package! stream object)
;;   (call-next-method stream "</symbol>"))

;; (defmethod write-stream ((stream xml-stream-mixin) (object character))
;;   (call-next-method stream "<char>")
;;   (char! stream object)
;;   (call-next-method stream "</char>"))

;; (defparser xml? (c (acc (make-accumulator)))
;;   (:or (:and (:seq "<null/>") (:return 'null))
;;        (:and (:seq "<true/>") (:return t))
;;        (:and (:seq "<symbol>")
;; 	     (:oom (:not (:seq "</symbol>"))
;; 		   (:type octet? c)
;; 		   (:collect c acc))
;; 	     (:return (read-from-string acc)))
;;        (:and (:seq "<char>")
;; 	     (:oom (:not (:seq "</char>"))
;; 		   (:type octet? c))
;; 	     (:return (code-char c)))))

;; (defmethod read-object ((stream xml-stream-mixin) (tag t) object)
;;   'default)

;; (defmethod read-object ((stream xml-stream-mixin) (tag (eql 'null)) object)
;;   'moo)

;; (defvar +in-parser+ nil)
;; (defmethod read-stream ((stream xml-stream-mixin))
;;   (if +in-parser+
;;       (call-next-method stream)
;;       (let* ((+in-parser+ t)
;; 	     (element (dom-element? stream)))
;; 	(describe element)
;; 	(read-object stream (intern (string-upcase (dom.tag element))) element))))

;; (defclass+ xml-element ()
;;   ((tag :accessor xml.tag :initarg :tag :initform nil)
;;    (namespace :accessor xml.namespace :initarg :namespace :initform nil)
;;    (attributes :accessor xml.attributes :initarg :attributes :initform nil)
;;    (children :accessor xml.children :initarg :children :initform nil)))

;; (defmethod print-object ((self xml-element) stream)
;;   (print-unreadable-object (self stream :type t :identity t)
;;     (if (xml.namespace self)
;; 	(format stream "~A:~A(~D)"
;; 		(xml.tag self) (xml.namespace self) (length (xml.children self)))
;; 	(format stream "~A(~D)"
;; 		(xml.tag self) (length (xml.children self))))))

;; (defmethod get-attribute ((element xml-element) name)
;;   (cdr (assoc name (xml.attributes element) :test #'string=)))

;; (defmethod (setf get-attribute) (value (element xml-element) name)
;;   (if (assoc name (xml.attributes element))
;;       (setf (cdr (assoc name (xml.attributes element) :test #'string=)) value)
;;       (setf (xml.attributes element)
;; 	    (cons (cons name value) (xml.attributes element)))))

;; (defmethod set-attribute ((element xml-element) name value)
;;   (setf (get-attribute element name) value))

;; (defmacro defxml (name &rest attributes)
;;   `(defclass+ ,name ()
;;      (,attributes)))

;; (defxml )