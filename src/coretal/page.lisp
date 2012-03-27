(in-package :core-server)

;; -------------------------------------------------------------------------
;; Abstract Page
;; -------------------------------------------------------------------------
(defcomponent abstract-page ()
  ((name :host both :index t :print t :type string)
   (widgets :host remote :type abstract-widget-map*))
  (:ctor %make-abstract-page))

;; -------------------------------------------------------------------------
;; Simple Page
;; -------------------------------------------------------------------------
(defcomponent simple-page (abstract-page)
  ()
  (:ctor make-simple-page))

(defmethod/remote destroy ((self simple-page))
  (mapcar-cc (lambda (a) (if a (destroy a))) (widgets self))
  (delete-slots self 'widgets)
  (call-next-method self))

(defmethod/remote init ((self simple-page))
  (setf (widgets self) (mapcar-cc (lambda (m) (make-component m)) (widgets self))))



