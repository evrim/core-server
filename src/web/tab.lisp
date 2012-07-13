(in-package :core-server)

(defcomponent <core:tab (<:div)
  ((tabs :host remote)
   (hilight-class :host remote :initform "hilight")
   (tab-css :host remote :initform +tab-css+)
   (default-tab :host remote :initform nil)
   (_content :host remote)
   (_nav :host remote)))

(defmethod/remote hilight-tab ((self <core:tab) tab)
  (mapcar (lambda (li)
	    (let ((a (car (slot-value li 'child-nodes))))
	      (cond
		((eq tab (slot-value a 'inner-text))
		 (add-class a (hilight-class self))
		 (add-class li (hilight-class self)))
		(t
		 (remove-class a (hilight-class self))
		 (remove-class li (hilight-class self))))))
	  (slot-value (_nav self) 'child-nodes)))

(defmethod/remote make-tab ((self <core:tab) tab)
  (reduce0 (lambda (acc a)
	     (if (and (listp a) (eq tab (car a)))
		 (car (cdr a))
		 acc))
	   (tabs self)))

(defmethod/remote show-tab ((self <core:tab) tab)
  (hilight-tab self tab)
  (aif (make-tab self tab)
       (setf (_content self) (replace-node (_content self) it))))

(defmethod/remote navigation ((self <core:tab))
  (<:ul :class "core-tab-navigation block left width-100p"
   (mapcar-cc (lambda (tab)
		(<:li (<:a :onclick (lifte (show-tab self tab)) tab)))
	      (mapcar (lambda (a) (if (atom a) a (car a)))
		      (tabs self)))))

(defmethod/remote destroy ((self <core:tab))
  (remove-class self "core-tab")
  (remove-class self "core")
  (call-next-method self))

(defmethod/remote init ((self <core:tab))
  (call-next-method self)
  (load-css (tab-css self))
  (add-class self "core-tab")
  (add-class self "core")
  (append self (setf (_nav self) (navigation self)))
  (append self (<:div :class "clear"))
  (append self (setf (_content self)
		     (<:p :class "pad10"
			  "Please select an option from above."))))
