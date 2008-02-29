(in-package :core-server)

(defcomponent hilighter ()
  ((menu-query :host remote :accessor menu-query :initarg :menu-query
	       :initform ".menu a")
   (active-class :host remote :initarg :active-class :initform "active")
   (passive-class :host remote :initarg :passive-class :initform "")))

(defmethod/remote hilight ((self hilighter) anchor)
  (dolist (item (dojo.query this.menu-query))
    (if (= anchor (item.hash.substr 1))
	(setf item.parent-node.class-name this.active-class
	      item.class-name this.active-class)
	(setf item.parent-node.class-name this.passive-class
	      item.class-name this.passive-class))))