(in-package :core-server)

;; -------------------------------------------------------------------------
;; Simple Menu Widget
;; -------------------------------------------------------------------------
(defcomponent <widget:simple-menu-widget (<:ul simple-widget)
  ((items :host remote :initform (error "Provide :items"))
   (hilight-class :host remote :initform "hilight")))

(defmethod/remote on-page-load ((self <widget:simple-menu-widget) name)
  (mapcar (lambda (a)
	    (if (eq name (get-parameter "page" (slot-value a 'href)))
		(add-class a (hilight-class self))
		(remove-class a (hilight-class self))))
	  (node-search (lambda (a) (eq (slot-value a 'tag-name) "A")) self)))

(defmethod/remote init ((self <widget:simple-menu-widget))
  (mapcar-cc (lambda (a) (append self a))
	     (mapcar-cc (lambda (a)
			  (<:li (<:a :href (+ "#page:" (car (cdr a))) (car a))))
			(items self))))
