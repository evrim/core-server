(in-package :core-server)

;; -------------------------------------------------------------------------
;; Simple Page Controller
;; -------------------------------------------------------------------------
(defcomponent simple-controller (secure-object)
  ((pages :host local :export nil :type abstract-page*
	  :documentation "List of pages")   
   (default-page :host both :type string
		 :documentation "Name of the default page")
   (constants :host remote :type widget-map*
	      :documentation "Constant widgets associated with this
	      controller"))
  (:default-initargs
    :levels '(simple-controller/unauthorized simple-controller/anonymous
	      simple-controller/authorized)
    :permissions '((owner . 2) (group . 2) (other . 2) (anonymous . 1)
		   (unauthorized . 0))
    :owner (make-simple-user :name "admin")
    :group (make-simple-group :name "admin"))
  (:ctor make-simple-controller))

;; -------------------------------------------------------------------------
;; Unauthorized Controller
;; -------------------------------------------------------------------------
(defcomponent simple-controller/unauthorized (simple-controller
					      secure-object/unauthorized)
  ())

(defmethod/remote init ((self simple-controller/unauthorized))
  (alert "Sorry, you are unauthorized.")
  (setf window.location.href "index.html"))

;; -------------------------------------------------------------------------
;; Anonymous Controller
;; -------------------------------------------------------------------------
(defcomponent simple-controller/anonymous (simple-controller
					   secure-object/authorized)
  ((secure-object :host lift :type simple-controller)
   (pages :host local :lift t :export nil :authorize t :type abstract-page*)
   (default-page :host remote :lift t)
   (constants :host remote :lift t :authorize t)
   (_page :host remote)))

(defmethod/remote destroy ((self simple-controller/anonymous))
  (mapcar-cc (lambda (a) (destroy a)) (constants self))
  (aif (_page self) (destroy it))
  (delete-slots self 'constants)
  (call-next-method self))

(defmethod/local get-page ((self simple-controller/anonymous) name)
  (aif (find name (pages self) :key #'name :test #'string=)
       (authorize (current-application self) (user self) it)))

(defmethod/remote load-page ((self simple-controller/anonymous) name)
  (let ((ctor (get-page self name)))
    (cond
      (ctor (setf (_page self) (make-component ctor)))
      (t (_debug (list "page not found" name))
	 nil))))

(defmethod/remote init ((self simple-controller/anonymous))
  (setf (constants self)
  	(mapcar-cc (lambda (a) (make-component a)) (constants self)))
  (load-page self (or (get-parameter "page") (default-page self)))
  (_debug "loaded."))

;; -------------------------------------------------------------------------
;; Authorized Controller
;; -------------------------------------------------------------------------
(defcomponent simple-controller/authorized (simple-controller/anonymous)
  ())