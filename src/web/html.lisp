(in-package :core-server)

(defclass dom-element ()
  ((id :accessor id)
   (tag :accessor tag :initarg :tag)
   (attributes :accessor attributes :initarg :attributes)
   (children :accessor children :initarg :children)))

(defmethod print-object ((self dom-element) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "tag:~A" (tag self))))

(defun make-dom-element (tag attributes &rest children)
  (make-instance 'dom-element
		 :tag tag
		 :attributes attributes
		 :children children))

(defrule attribute-name? (c (acc (make-accumulator)))
  (:oom (:or (:type alphanum? c)
	     (:and #\- (:do (setq c #\-))))
	(:collect c acc))
  (:return acc))

(defrule attribute-value? (c (val (make-accumulator)))
  (:checkpoint #\" (:commit))
  (:zom (:checkpoint #\" (:return val))
	(:checkpoint #\> (:rewind-return val))
	(:type (or visible-char? space?) c)
	(:collect c val))
  (:checkpoint #\" (:commit)))

(defrule attribute? (name value)
  (:attribute-name? name)
  #\=
  (:attribute-value? value)
  (:return (cons name value)))

(defrule tag-name? (name)
  (:attribute-name? name) (:return name))

(defrule text-node? (c (acc (make-accumulator)))
  (:not #\<)
  (:oom (:checkpoint #\< (:rewind-return acc))
	(:type octet? c)
	(:collect c acc))
  (:if (> (length acc) 0)
       (:return acc)))

(defrule dom-element? (tag attr attrs child children)
  (:checkpoint (:seq "<!DOCTYPE")
	       (:zom (:not #\>) (:type octet?)) (:lwsp?) (:commit))
  #\<
  (:tag-name? tag)
  (:zom (:lwsp?)
	(:attribute? attr)
	(:do (push attr attrs)))
  (:or (:and (:lwsp?) (:seq "/>") (:return (make-dom-element tag (nreverse attrs))))
       (:and #\>
	     (:zom (:or (:dom-element? child) (:text-node? child))
		   (:do (push child children)))
	     (:seq "</")
	     (:seq tag)
	     #\>
	     (:return (apply #'make-dom-element tag (nreverse attrs) (nreverse children))))))

(defparameter *dom-parsers* '(dom-element? tag-name? attribute? attribute-value? attribute-name? text-node?
			      make-dom-element dom-element!))

(defun trace-dom-parsers ()
  (mapcar (lambda (fun)
	    (eval `(trace ,fun)))
	  *dom-parsers*))

(defun dom-element! (stream element)
  (prog1 stream
    (char! stream #\<)
    (string! stream (tag element))
    (mapcar (lambda (attr)
	      (char! stream #\Space)
	      (string! stream (car attr))
	      (char! stream #\=)
	      (string! stream (cdr attr)))
	    (attributes element))
    (cond
      ((null (children element))
       (string! stream "/>"))
      (t
       (char! stream #\>)
       (mapcar (lambda (child)
		 (if (stringp child)
		     (string! stream child)
		     (progn
		       (char! stream #\Newline)
		       (dom-element! stream child))))
	       (children element))
       (string! stream "</")
       (string! stream (tag element))
       (char! stream #\>)))))

(defmacro def-domelement (name args &body body)
  )