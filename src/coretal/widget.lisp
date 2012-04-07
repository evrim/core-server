(in-package :core-server)

;; -------------------------------------------------------------------------
;; Simple Widget
;; -------------------------------------------------------------------------
(defcomponent simple-widget ()
  ((widget-map :host both :type abstract-widget-map))
  (:ctor make-simple-widget))
