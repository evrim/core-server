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
(defcomponent simple-controller/anonymous (history-mixin
					   simple-controller
					   secure-object/authorized)
  ((secure-object :host lift :type simple-controller)
   (pages :host local :lift t :export nil :authorize t :type abstract-page*)
   (default-page :host remote :lift t)
   (constants :host remote :lift t :authorize t)
   (core-css :host remote :initform "style/core.css")
   (_page :host remote)))

(defmethod/remote destroy ((self simple-controller/anonymous))
  (mapcar-cc (lambda (a) (destroy a)) (constants self))
  (aif (_page self) (destroy it) nil)
  (delete-slots self 'constants)
  (call-next-method self))

(defmethod/local get-page ((self simple-controller/anonymous) name)
  (aif (find name (pages self) :key #'name :test #'string=)
       (authorize (secure.application self) (secure.user self) it)))

(defmethod/remote load-page ((self simple-controller/anonymous) name)
  (let ((ctor (get-page self name)))
    (cond
      (ctor
       (if (_page self) (destroy (_page self)))
       (setf (_page self) (make-component ctor))
       (mapcar-cc (lambda (a)
		    (make-web-thread
		     (lambda () (on-page-load a name))))
		  (constants self)))
      (t (_debug (list "page not found" name))
	 nil))))

(defmethod/remote get-page-in-the-url ((self simple-controller/anonymous))
  (get-parameter "page"))

(defmethod/remote set-page-in-the-url ((self simple-controller/anonymous) _name)
  (unless (eq _name (get-page-in-the-url self))
    (set-parameter "page" _name)))

(defmethod/remote on-history-change ((self simple-controller/anonymous))
  (_debug (list "on-history-change" self))
  (let ((page (_page self))
	(anchor (or (get-page-in-the-url self) (default-page self))))
    (if page
	(let ((name (name page)))
	  (if (and (not (eq name anchor))
		   (not (eq name window.location.pathname)))
	      (load-page self anchor)
	      (call-next-method self)))
	(load-page self anchor))))

(defmethod/remote init ((self simple-controller/anonymous))
  (load-css (core-css self))
  (setf (constants self)
  	(mapcar-cc (lambda (a) (make-component a)) (constants self)))
  (load-page self (or (get-parameter "page") (default-page self)))
  (start-history-timeout self)
  (_debug "loaded."))

;; -------------------------------------------------------------------------
;; Authorized Controller
;; -------------------------------------------------------------------------
(defcomponent simple-controller/authorized (simple-controller/anonymous)
  ())