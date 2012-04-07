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
(defcomponent simple-widget-map (secure-object abstract-widget-map)
  ()
  (:default-initargs
      :levels '(simple-widget-map/anonymous)
    :permissions '((owner . 0) (group . 0) (other . 0) (anonymous . 0)
		   (unauthorized . 0))
    :owner (make-simple-user :name "admin")
    :group (make-simple-group :name "admin"))
  (:ctor make-simple-widget-map))

;; -------------------------------------------------------------------------
;; Simple Widget Map / Anonymous
;; -------------------------------------------------------------------------
(defcomponent simple-widget-map/anonymous (simple-widget-map
					   secure-object/authorized)
  ((secure-object :host lift :type simple-widget-map)
   (selector :host remote :lift t)
   (widget :host remote :lift t :authorize t)
   (enabled :host remote :lift t)))

(defmethod/remote init ((self simple-widget-map/anonymous))
  (awhen (document.get-element-by-id (selector self))
    (setf (enabled self) t
	  (widget self) (call/cc (widget self) it))))
