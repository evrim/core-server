(in-package :core-server)

(defcomponent dojo-widget (html-element)
  ((dojo-type :host remote :initarg :dojo-type :initform (error "Please specify :dojo-type"))))

(defmethod/local render ((self dojo-widget))
  (with-yaclml-output-to-string
    (<:div (<:ul (<:li "Item 1")
		 (<:li "Item 2")
		 (<:li "Item 3")))))

(defmethod/cc send/ctor ((self dojo-widget) remote-slots local-methods remote-methods)
  (<:js
   `(defun ,(class-name (class-of self)) ()
	(let ((o (create ,@remote-slots ,@local-methods ,@remote-methods))
	      (p (document.create-element o.tag)))
	  (doeach (property o)
	    (setf (slot-value p property) (slot-value o property)))
	  
	  (if (= "function" (typeof o.render))		
	      (setf p.inner-h-t-m-l (o.render)))
	  
	  (dojo.require "dojo.xml.Parse")
	  (dojo.require ,(format nil "dojo.widget.~A" (string-capitalize (slot-value self 'dojo-type))))
;;; 	  (let ((parser (new (dojo.xml.*parse)))
;;; 		(frag (parser.parse-element p nil t)))
;;; 	    (.create-components (dojo.widget.get-parser) frag))
	  (setf this.prototype p)
	  (return p)))))

