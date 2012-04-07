(in-package :core-server)

;; -------------------------------------------------------------------------
;; Simple Page
;; -------------------------------------------------------------------------
(defcomponent simple-page (secure-object)
  ((name :host both :index t :print t :type string)
   (widgets :host remote :type abstract-widget-map*))
  (:default-initargs
    :levels '(simple-page/unauthorized simple-page/anonymous
	      simple-page/registered)
    :permissions '((owner . 2) (group . 2) (other . 2) (anonymous . 1)
		   (unauthorized . 0))
    :owner (make-simple-user :name "admin")
    :group (make-simple-group :name "admin"))
  (:ctor make-simple-page))

;; -------------------------------------------------------------------------
;; Unauthorized Controller
;; -------------------------------------------------------------------------
(defcomponent simple-page/unauthorized (simple-page secure-object/unauthorized)
  ())

(defmethod/remote init ((self simple-page/unauthorized))
  (alert "Sorry, you are unauthorized to view this page."))

;; -------------------------------------------------------------------------
;; Simple Page / Anonymous
;; -------------------------------------------------------------------------
(defcomponent simple-page/anonymous (simple-page secure-object/authorized)
  ((secure-object :host lift :type simple-page)
   (widgets :host remote :lift t :authorize t)
   (name :host remote :lift t)))

(defmethod/remote destroy ((self simple-page/anonymous))
  (mapcar-cc (lambda (a) (if a (destroy a))) (widgets self))
  (delete-slots self 'widgets)
  (call-next-method self))

(defmethod/remote init ((self simple-page/anonymous))
  (setf (widgets self) (mapcar-cc (lambda (m) (make-component m)) (widgets self))))

;; -------------------------------------------------------------------------
;; Simple Page / Registered
;; -------------------------------------------------------------------------
(defcomponent simple-page/registered (simple-page/anonymous)
  ())
