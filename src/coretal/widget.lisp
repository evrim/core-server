(in-package :core-server)

;; -------------------------------------------------------------------------
;; Simple Widget
;; -------------------------------------------------------------------------
(defcomponent simple-widget ()
  ((widget-map :host both :type abstract-widget-map))
  (:ctor make-simple-widget))

(defmethod/remote init ((self simple-widget))
  (mapcar (lambda (a) (.remove-child self a))
	  (reverse (slot-value self 'child-nodes))))

(defmethod/remote destroy ((self simple-widget))
  (mapcar (lambda (a) (.remove-child self a))
	  (reverse (slot-value self 'child-nodes)))
  (call-next-method self))