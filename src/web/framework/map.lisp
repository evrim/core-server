(in-package :core-server)

;; -------------------------------------------------------------------------
;; Abstract Widget Map
;; -------------------------------------------------------------------------
(defcomponent abstract-widget-map (<:div)
  ((selector :host both :type string :print t)
   (widget :host both :type abstract-widget :print t)
   (enabled :host remote :initform nil))  
  (:ctor %make-abstract-widget-map))

;; -------------------------------------------------------------------------
;; Simple Widget Map
;; -------------------------------------------------------------------------
(defcomponent <core:simple-widget-map (secure-object abstract-widget-map)
  ()
  (:default-initargs
   :levels '(<core:simple-widget-map/anonymous)
   :permissions '((owner . 0) (group . 0) (other . 0) (anonymous . 0)
		  (unauthorized . 0))
   :owner (make-simple-user :name "admin")
   :group (make-simple-group :name "admin")))

;; -------------------------------------------------------------------------
;; Simple Widget Map / Anonymous
;; -------------------------------------------------------------------------
(defcomponent <core:simple-widget-map/anonymous (<core:simple-widget-map
						 secure-object/authorized)
  ((secure-object :host lift :type <core:simple-widget-map)
   (selector :host remote :lift t)
   (widget :host remote :lift t :authorize t)
   (enabled :host remote :lift t)
   (controller :host remote)))

(defmethod/remote on-page-load ((self <core:simple-widget-map/anonymous) name)
  (if (and (enabled self)
	   (typep (slot-value (widget self) 'on-page-load) 'function))
      (on-page-load (widget self) name)))

(defmethod/remote destroy ((self <core:simple-widget-map/anonymous))
  (when (enabled self)
    (destroy (widget self))))

(defmethod/remote init ((self <core:simple-widget-map/anonymous))
  (awhen (document.get-element-by-id (selector self))
    (setf (enabled self) t
	  (widget self)
	  (call/cc (widget self) (extend (jobject :widget-map self) it)))))
