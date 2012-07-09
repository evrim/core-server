(in-package :core-server)

;; -------------------------------------------------------------------------
;; Tab Widget
;; -------------------------------------------------------------------------
(defcomponent <widget:tab (<widget:simple-widget)
  ((tabs :host remote)
   (hilight-class :host remote :initform "hilight")
   (widget-css :host remote :initform +tab-css+)
   (default-tab :host remote :initform nil)
   (_content :host remote)
   (_nav :host remote)))

(defmethod/remote hilight-tab ((self <widget:tab) tab)
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

(defmethod/remote show-tab ((self <widget:tab) tab)
  (hilight-tab self tab))

(defmethod/remote navigation ((self <widget:tab))
  (<:ul :class "core-tab-navigation block left width-100p"
   (mapcar-cc (lambda (tab)
		(<:li (<:a :onclick (lifte (show-tab self tab)) tab)))
	      (tabs self))))

(defmethod/remote destroy ((self <widget:tab))
  (remove-class self "core-tab")
  (remove-class self "core")
  (call-next-method self))

(defmethod/remote init ((self <widget:tab))
  (call-next-method self)
  (load-css (widget-css self))
  (add-class self "core-tab")
  (add-class self "core")
  (append self (setf (_nav self) (navigation self)))
  (append self (<:div :class "clear"))
  (append self (setf (_content self)
		     (<:p :class "pad10"
			  "Please select an option from above."))))