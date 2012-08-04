(in-package :core-server)

;; -------------------------------------------------------------------------
;; Simple Widget
;; -------------------------------------------------------------------------
(defcomponent <widget:simple ()
  ((widget-map :host both :type abstract-widget-map)
   (_child-nodes :host remote))
  (:ctor make-simple-widget))

(defmethod/remote destroy ((self <widget:simple))
  (mapcar (lambda (a) (.remove-child self a))
	  (reverse (slot-value self 'child-nodes)))
  (mapcar (lambda (a) (.append-child self a)) (_child-nodes self))
  (call-next-method self))

(defmethod/remote init ((self <widget:simple))
  (mapcar (lambda (a) (.remove-child self a))
	  (setf (_child-nodes self) (reverse (slot-value self 'child-nodes))))  
  (call-next-method self))
