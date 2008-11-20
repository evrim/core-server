(in-package :core-server)

(defclass markup ()
  ((tag :initarg :tag)
   (namespace :initarg :namespace)
   (attributes :initarg :attributes)
   (children :initarg :children)))

(defun make-markup (namespace tag attributes children)
  (make-instance 'markup
		 :namespace namespace
		 :tag tag
		 :attributes attributes
		 :children children))

(defun make-markup2 (symbol attributes children)
  (make-instance 'markup
		 :namespace (package-name (symbol-package symbol))
		 :tag (symbol-name symbol)
		 :attributes attributes
		 :children children))

(defmacro defto-markup (((var specializer)) &body body)
  (with-unique-names (k)
    `(defmethod to-markup ((,var ,specializer) &optional (,k #'to-markup))
       (declare (ignorable ,k))
       ,@body)))

(defto-markup ((object t))
  (error "Could not convert ~A to markup." object))

(defto-markup ((object (eql 't)))
  (make-markup2 t nil nil))

(defto-markup ((object null))
  (make-markup2 'nil nil nil))

(defmethod from-markup ((markup markup) &optional (k #'from-markup))
  (from-markup2 markup (intern (slot-value markup 'tag)
			       (find-package (slot-value markup 'namespace)))
		k))

(defmacro deffrom-markup ((object specializer) &body body)
  (with-unique-names (k tag)
    `(defmethod from-markup2 ((,object markup) (,tag ,specializer) &optional (,k #'from-markup))
       (declare (ignorable ,k))
       ,@body)))

(deffrom-markup (object (eql 't))
  t)

(deffrom-markup (object null)
  nil)

(defmacro defmarkup (name supers slots)
  `(defclass+ ,name ,supers
     ,(mapcar (lambda (slot)
		`(,slot :host attribute))
	      slots)))

(defmarkup html-markup (markup)
  ())

(defmarkup html-core-attributes ()
  (class id style title))

(defmarkup html-event-attributes ()
  (onclick ondblclick onmousedown onmouseup onmouseover onmousemove
	   onmouseout onkeypress onkeydown onkeyup))

(defmarkup html-i18n-attributes ()
  (dir lang))

(defmacro defhtml-tag1 (name supers slots)
  `(progn
     (defmarkup ,name (,@supers html-markup)
       ,slots)
     (defun ,name (&rest args)
       (multiple-value-bind (attributes children) (tag-attributes args)
	 (apply #'make-instance ',name
		(append attributes
			(list :children (flatten children))))))))

;; ----------------------------------------------------------------------------
;; HTML Markup
;; ----------------------------------------------------------------------------
(defhtml-tag1 <:foo (html-core-attributes html-event-attributes html-i18n-attributes)
  (accesskey charset coords href hreflang name onblur onfocus rel rev shape
	     tabindex target type))

(defhtml-tag1 <:abbr (html-core-attributes html-event-attributes html-i18n-attributes)
  ())

