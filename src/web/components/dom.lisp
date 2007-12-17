(in-package :core-server)

(defcomponent dom-element ()
  ((tag :reader tag :initarg :tag :host remote :initform (error "Please specify :tag for dom element."))
   (id :reader id :initarg :id :host remote :initform nil)
   (class :accessor css-class :initarg :class :host remote :initform nil)))

(defmethod/cc send/ctor ((self dom-element) remote-slots local-methods remote-methods)
  (<:js
   `(defun ,(class-name (class-of self)) ()
	(let ((o (create ,@remote-slots ,@local-methods ,@remote-methods))
	      (p (document.create-element o.tag)))
	  (doeach (property o)
	    (setf (slot-value p property) (slot-value o property)))
	  (setf this.prototype p)
	  (return p)))))

