(in-package :core-server)

;; -------------------------------------------------------------------------
;; Simple Menu Widget
;; -------------------------------------------------------------------------
(defcomponent <widget:simple-menu (<:ul <widget:simple)
  ((items :host remote :initarg :children)
   (hilight-class :host remote :initform "hilight")))

(defmethod/remote on-page-load ((self <widget:simple-menu) name)
  (mapcar (lambda (a)
	    (if (eq name (get-parameter "page" (slot-value a 'href)))
		(add-class a (hilight-class self))
		(remove-class a (hilight-class self))))
	  (node-search (lambda (a) (eq (slot-value a 'tag-name) "A")) self)))

(defmethod/remote init ((self <widget:simple-menu))
  (mapcar-cc (lambda (a) (append self a))
	     (mapcar-cc (lambda (menu)
			  (with-slots (name title) menu
			    (<:li (<:a :href (+ "#page:" name) title))))
			(items self))))
