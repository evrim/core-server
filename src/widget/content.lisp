(in-package :core-server)

(defcomponent <widget:simple-content (<:div <widget:simple)
  ((content :host remote :initform nil :initarg :children)))

(defmethod/remote init ((self <widget:simple-content))
  (call-next-method self) 
  (mapcar-cc (lambda (a)
	       (if (typep a 'function)
		   (append self (make-component a))
		   (append self a)))
	     (content self)))