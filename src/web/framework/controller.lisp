;; -------------------------------------------------------------------------
;; Coretal Controller
;; -------------------------------------------------------------------------
(in-package :core-server)

;; -------------------------------------------------------------------------
;; Generic Unauthorized Controller
;; -------------------------------------------------------------------------
(defcomponent <core:controller/unauthorized (secure-object/unauthorized)
  ((redirect-location :host both :initform "index.html")))

(defmethod/remote init ((self <core:controller/unauthorized))
  (alert "Sorry, you are unauthorized.")
  (setf (slot-value (slot-value window 'location) 'href)
	(redirect-location self)))

;; -------------------------------------------------------------------------
;; Simple Page Controller
;; -------------------------------------------------------------------------
(defcomponent <core:simple-controller (secure-object <:div)
  ((pages :host local :export nil :type <core:simple-page
	  :documentation "List of pages" :initarg :children)   
   (default-page :host both :type string
		 :documentation "Name of the default page")
   (constants :host remote :type abstract-widget-map* :authorize t
	      :documentation "Constant widgets associated with this
	      controller")
   (plugins :host remote :type plugin* :authorize t
	    :documentation "Plugins that extends functions of this
	    controller"))
  (:default-initargs
   :levels '(<core:controller/unauthorized <core:simple-controller/anonymous
	     <core:simple-controller/authorized)
   :permissions '((owner . 2) (group . 2) (other . 2) (anonymous . 1)
		  (unauthorized . 0))
   :owner (make-simple-user :name "admin")
   :group (make-simple-group :name "admin")))

;; -------------------------------------------------------------------------
;; Anonymous Controller
;; -------------------------------------------------------------------------
(defcomponent <core:simple-controller/anonymous (history-mixin
						 <core:simple-controller
						 secure-object/authorized)
  ((secure-object :host lift :type <core:simple-controller)
   (pages :host local :lift t :export nil :authorize t :type <core::simple-page*)
   (plugins :host remote :lift t :authorize t :type plugin*)
   (default-page :host remote :lift t)
   (constants :host remote :lift t :authorize t)
   (core-css :host remote :initform +core.css+)
   (_page :host remote)))

(defmethod/remote destroy ((self <core:simple-controller/anonymous))
  (mapcar-cc (lambda (a) (destroy a)) (constants self))
  (aif (_page self) (destroy it) nil)
  (delete-slots self 'constants)
  (call-next-method self))

(defmethod/local get-page ((self <core:simple-controller/anonymous) name)
  (aif (find name (pages self) :key #'name :test #'string=)
       (authorize (secure.application self) (secure.user self) it)))

(defmethod/remote load-page ((self <core:simple-controller/anonymous) name)
  (let ((ctor (get-page self name)))
    (cond
      (ctor
       (if (_page self) (destroy (_page self)))
       (setf (_page self) (make-component ctor :controller self))
       (mapcar-cc (lambda (a)
		    (make-web-thread
		     (lambda () (on-page-load a name))))
		  (constants self)))
      (t (_debug (list "page not found" name))
	 nil))))

(defmethod/remote get-page-in-the-url ((self <core:simple-controller/anonymous))
  (get-parameter "page"))

(defmethod/remote set-page-in-the-url ((self <core:simple-controller/anonymous)
				       _name)
  (unless (eq _name (get-page-in-the-url self))
    (set-parameter "page" _name)))

(defmethod/remote on-history-change ((self <core:simple-controller/anonymous))
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

(defmethod/remote init ((self <core:simple-controller/anonymous))
  (load-css (core-css self))
  (setf (plugins self)
	(mapcar-cc (lambda (plugin) (call/cc plugin self))
		   (plugins self)))
  (setf (constants self)
  	(mapcar-cc (lambda (a) (make-component a :controller self))
		   (constants self)))
  (load-page self (or (get-parameter "page") (default-page self)))
  (start-history-timeout self)
  (_debug "loaded."))

;; -------------------------------------------------------------------------
;; Authorized Controller
;; -------------------------------------------------------------------------
(defcomponent <core:simple-controller/authorized (<core:simple-controller/anonymous)
  ())
