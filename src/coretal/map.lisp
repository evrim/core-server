(in-package :core-server)

;; -------------------------------------------------------------------------
;; Abstract Widget Map
;; -------------------------------------------------------------------------
(defcomponent abstract-widget-map ()
  ((selector :host both :type string)
   (widget :host both :type abstract-widget)
   (enabled :host remote :initform nil))
  (:ctor %make-abstract-widget-map))

;; -------------------------------------------------------------------------
;; Simple Widget Map
;; -------------------------------------------------------------------------
(defcomponent simple-widget-map (abstract-widget-map)
  ()
  (:ctor make-simple-widget-map))

(defmethod/remote init ((self simple-widget-map))
  (awhen (document.get-element-by-id (selector self))
    (setf (enabled self) t
	  (widget self) (call/cc (widget self) it))))