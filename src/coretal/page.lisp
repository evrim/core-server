(in-package :core-server)

;; -------------------------------------------------------------------------
;; Simple Page
;; -------------------------------------------------------------------------
(defcomponent <core:simple-page (secure-object <:div)
  ((name :host both :index t :print t :type string)
   (widgets :host remote :type abstract-widget-map* :initarg :children))
  (:default-initargs
    :levels '(<core:simple-page/unauthorized <core:simple-page/anonymous
	      <core:simple-page/registered)
    :permissions '((owner . 2) (group . 2) (other . 2) (anonymous . 1)
		   (unauthorized . 0))
    :owner (make-simple-user :name "admin")
    :group (make-simple-group :name "admin")))

;; -------------------------------------------------------------------------
;; Unauthorized Controller
;; -------------------------------------------------------------------------
(defcomponent <core:simple-page/unauthorized (<core:simple-page
					      secure-object/unauthorized)
  ())

(defmethod/remote init ((self <core:simple-page/unauthorized))
  (alert "Sorry, you are unauthorized to view this page."))

;; -------------------------------------------------------------------------
;; Simple Page / Anonymous
;; -------------------------------------------------------------------------
(defcomponent <core:simple-page/anonymous (<core:simple-page
					   secure-object/authorized)
  ((secure-object :host lift :type <core:simple-page)
   (widgets :host remote :lift t :authorize t)
   (name :host remote :lift t)
   (controller :host remote)))

(defmethod/remote destroy ((self <core:simple-page/anonymous))
  (mapcar-cc (lambda (a) (if a (destroy a))) (widgets self))
  (delete-slots self 'widgets)
  (call-next-method self))

(defmethod/remote init ((self <core:simple-page/anonymous))
  (setf (widgets self)
	(mapcar-cc (lambda (m) (make-component m :controller (controller self)))
		   (widgets self))))

;; -------------------------------------------------------------------------
;; Simple Page / Registered
;; -------------------------------------------------------------------------
(defcomponent <core:simple-page/registered (<core:simple-page/anonymous)
  ())
