(in-package :core-server)

;; -------------------------------------------------------------------------
;; Abstract Widget
;; -------------------------------------------------------------------------
(defcomponent abstract-widget (secure-object)
  ((widget-map :host both :type abstract-widget-map))
  (:ctor %make-abstract-widget))

;; -------------------------------------------------------------------------
;; Simple Widget
;; -------------------------------------------------------------------------
(defcomponent simple-widget (abstract-widget)
  ()
  (:ctor make-simple-widget))

(defmacro defwidget (name supers slots args)
  `(defcomponent ,name ,supers ,slots ,args))