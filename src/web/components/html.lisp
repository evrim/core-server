(in-package :core-server)

(defcomponent html-element (dom-element)
  ())

(defmethod/local render ((self html-element))
  nil)

;;; (with-yaclml-output-to-string
;;;     (<:div :id "56" :style "background-color:#FFF; color:#000"
;;; 	   (<:p "This is render of html-element")
;;; 	   (<:p "Aytek maraba!")))

(defmethod/cc send/ctor ((self html-element) remote-slots local-methods remote-methods)
  (<:js
   `(defun ,(class-name (class-of self)) ()
	(let ((o (create ,@remote-slots ,@local-methods ,@remote-methods))
	      (p (document.create-element o.tag)))
	  (doeach (property o)
	    (setf (slot-value p property) (slot-value o property)))
	  
	  (if (= "function" (typeof o.render))		
	      (setf p.inner-h-t-m-l (o.render)))

	  (setf this.prototype p)
	  (return p)))))

(defcomponent div-element (html-element)
  ()
  (:default-initargs :tag "div"))

(defmethod/local render ((self div-element))
  (with-html-output (http-response.stream (response +context+))
    (<:div "hobaaa")))