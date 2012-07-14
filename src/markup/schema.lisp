(in-package :core-server)

;; -------------------------------------------------------------------------
;; XML Schema
;; -------------------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass xml-schema+ (xml+)
    ()
    (:default-initargs
     :namespace "xs"
      :schema "http://www.w3.org/2001/XMLSchema")))

(defclass+ xml-schema-element (xml)
  ())

(defmacro defxs-tag (name &rest attributes)
  `(defclass+ ,name (xml-schema-element)
     (,@(mapcar (lambda (attr) (list attr :host 'remote :print t)) attributes))
     (:metaclass xml-schema+)
     (:tag ,@(string-downcase (symbol-name name)))
     (:namespace ,@(string-downcase
		    (subseq (package-name (symbol-package name)) 1)))
     (:attributes ,@attributes)))

(defxs-tag <xs:schema target-namespace element-form-default final-default block-default
	   attribute-form-default)
(defxs-tag <xs:import namespace schema-location)
(defxs-tag <xs:element name type ref)
(defxs-tag <xs:complex-type name mixed abstract)
(defxs-tag <xs:sequence)
(defxs-tag <xs:any max-occurs min-occurs namespace process-contents)
(defxs-tag <xs:any-attribute namespace process-contents)
(defxs-tag <xs:annotation)
(defxs-tag <xs:documentation)
(defxs-tag <xs:complex-content)
(defxs-tag <xs:simple-content)
(defxs-tag <xs:extension base)
(defxs-tag <xs:unique name)
(defxs-tag <xs:selector xpath)
(defxs-tag <xs:field xpath)
(defxs-tag <xs:choice min-occurs max-occurs)
(defxs-tag <xs:attribute name type use)
(defxs-tag <xs:simple-type)
(defxs-tag <xs:list item-type)
(defxs-tag <xs:union member-types)
(defxs-tag <xs:restriction base)
(defxs-tag <xs:enumeration value)
(defxs-tag <xs:attribute-group ref name)
(defxs-tag <xs:pattern value)

;; -------------------------------------------------------------------------
;; Schema -> Markup Generation
;; -------------------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun generate-tags-from-xml (xml package-name &optional (no-package nil))
    (let ((elements (xml-search xml (lambda (a) (typep a '<xs:element))))
	  (tags (list)))
      (mapcar (lambda (element)
		(let* ((type (slot-value element 'type))
		       (type (aif (split ":" type)
				  (cadr it)
				  type))
		       (type-def (xml-search xml
					     (lambda (a)
					       (and (typep a '<xs:complex-type) 
						    (equal (slot-value a 'name) type))))))
		  (cond
		    (type-def
		     (let ((attributes (xml-search type-def (lambda (a) (typep a '<xs:attribute)))))
		       (pushnew (list element type-def attributes) tags
				:key #'car :test (lambda (a b)
						   (equal (slot-value a 'name)
							  (slot-value b 'name))))))
		    (t
		     ;; (warn "type-def not found ~A" (slot-value element 'name))
		     nil
		     ))))
	      elements)
      (let* ((upper-case (string-upcase (symbol-name package-name)))
	     (long-name (make-keyword (format nil "TR.GEN.CORE.SERVER.~A" upper-case)))
	     (short-name (make-keyword (format nil "<~A" upper-case)))
	     (third-name (make-keyword (format nil "CORE-SERVER.~A" upper-case)))
	     (metaclass (intern (format nil "~A+" upper-case)))
	     (class (intern (format nil "~A-ELEMENT" upper-case)))
	     (macro-name (intern (format nil "DEF~A-TAG" upper-case)))
	     (namespace (slot-value xml 'target-namespace)))
	(assert (not (null namespace)) nil "Target-Namespace cannot be null")
	`(progn
	   ,(when (not no-package)
	     `(defpackage ,long-name
		(:nicknames ,short-name ,third-name)
		(:export ,@(mapcar (lambda (tag)
				     (make-symbol (string-upcase (slot-value (car tag) 'name))))
				   tags))))
	   (eval-when (:compile-toplevel :load-toplevel :execute)
	     (defclass ,metaclass (xml+)
	       ()
	       (:default-initargs :namespace ,(symbol-to-js package-name) :schema ,namespace)))

	   (defclass+ ,class (xml)
	     ())

	   (defmacro ,macro-name (name &rest attributes)
	     (let ((class-name (intern (symbol-name name) ,short-name) ))
	       `(progn
		  (defclass+ ,class-name (,',class)
		    (,@(mapcar (lambda (attr) (list attr :print nil :host 'remote))
			       attributes))
		    (:metaclass ,',metaclass)
		    (:tag ,@(string-downcase (symbol-name name)))
		    (:attributes ,@attributes))
		  (find-class+ ',class-name))))

	   ,@(mapcar (lambda (tag)
		       (destructuring-bind (element type-def attributes) tag
			 (declare (ignore type-def))
			 `(,macro-name ,(intern (string-upcase (slot-value element 'name)))
				       ,@(mapcar (lambda (attribute)
						   (with-slots (name) attribute
						     (intern
						      (symbol-name (js->keyword name)))))
						 attributes))))
		     tags)
	   (register-xml-namespace ,namespace ,short-name))))))

(defmacro defxml-namespace (namespace xml-schema-pathname &optional (no-package 'nil))
  (let* ((xml-schema-pathname (if (pathnamep xml-schema-pathname)
				  xml-schema-pathname
				  (eval xml-schema-pathname)))
	 (stream (make-xml-stream (make-core-stream xml-schema-pathname) namespace))
	 (xml (read-stream stream))
	 (no-package (eval no-package)))
    (assert (not (null xml)) nil "Cannot read xml from ~A" xml-schema-pathname)
    (generate-tags-from-xml xml namespace no-package)))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun make-xml-schema-pathname (name)
    (pathname (format nil "~A/src/markup/~A" (bootstrap:home) name))))

;; (defxml-namespace wsdl (make-xml-schema-pathname "wsdl20.xsd"))
