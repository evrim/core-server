(in-package :manager)

(defcomponent basic-widget ()
  ())

(defmethod/remote destroy ((self basic-widget))
  (mapcar (lambda (a) (.remove-child self a))
	  (reverse (slot-value self 'child-nodes))))

;; -------------------------------------------------------------------------
;; Info Component
;; -------------------------------------------------------------------------
(defcomponent info-component (basic-widget)
  ((_hostname :host remote :initform (hostname))
   (_memory :host remote
	    :initform (format nil "~10:D" (sb-kernel::dynamic-usage)))
   (_date :host remote :initform (get-universal-time))))

(defmethod/remote init ((self info-component))
  (append self (<:p "Date:" (date-to-string
			     (lisp-date-to-javascript (_date self)))))
  (append self (<:p "Hostname:" (_hostname self)))
  (append self (<:p "Memory: " (_memory self) " bytes")))

;; -------------------------------------------------------------------------
;; Settings Component
;; -------------------------------------------------------------------------
(defcomponent settings-component (basic-widget)
  ())


;; -------------------------------------------------------------------------
;; Sites Table
;; -------------------------------------------------------------------------
(deftable sites-table ()
  ((fqdn :label "FQDN")))

;; -------------------------------------------------------------------------
;; Sites Component 
;; -------------------------------------------------------------------------
(defcomponent sites-component (basic-widget)
  ((_table-ctor :host remote :initform (sites-table))
   (_table :host remote)
   (_fqdn-input :host remote :initform (<core:default-value-input))))

(defmethod/remote destroy ((self sites-component))
  (delete-slots self '_table)
  (call-next-method self))

(defmethod/local get-sites ((self sites-component))
  (site.list application))

(defmethod/remote make-table ((self sites-component))
  (make-component (_table-ctor self) :instances (get-sites self)))

(defmethod/local add-site ((self sites-component) fqdn)
  (site.add application :fqdn fqdn))

(defmethod/remote do-add-site ((self sites-component) fqdn)
  (add-site self fqdn)
  (setf (_table self) (replace-node (_table self) (make-table self))))

(defmethod/remote make-form ((self sites-component))
  (let ((fqdn (make-component (_fqdn-input self)
			      :default-value "enter site name")))
    (<:form :onsubmit (lifte (do-add-site self (get-input-value fqdn)))
	    (with-field "Fqdn of the site:" fqdn)
	    (with-field "" (<:input :type "submit" :disabled t
				    :value "Add")))))

(defmethod/remote init ((self sites-component))
  (append self (make-table self))
  (append self (setf (_table self) (make-form self))))

;; -------------------------------------------------------------------------
;; Manager Component
;; -------------------------------------------------------------------------
(defcomponent manager-controller (simple-controller)
  ((_menu :host remote)
   (_content :host remote))
  (:default-initargs :default-page "info")
  (:ctor make-manager-controller))

(defvar +pages+
  (list (list "info" "Info" "Information regarding to the current instance"
	      (info-component))
	(list "aanda" "A & A" "Authentication & Authorization"
	      (sites-component))
	(list "settings" "Settings" "Settings related to the current instance"
	      (settings-component))))

(defun make-controller (application user)
  (labels ((make-heading (title subtitle)
	     (make-simple-widget-map :selector "title"
				     :widget (<widget:simple-content-widget
					      :content (list (<:h1 title)
							     (<:h2 subtitle)))))
	   (make-page (name title subtitle widget)
	     (let ((map (make-simple-widget-map :selector "content"
						:widget widget)))
	       (make-simple-page :name name
				 :widgets
				 (list (make-heading title subtitle)
				       map))))
	   (make-map (name widget)
	     (make-simple-widget-map :selector name :widget widget))
	   (make-menu (pages)
	     (<widget:simple-menu-widget
	      :items (mapcar (lambda (pages)
			       (cons (cadr pages) (car pages)))
			     pages))))
    (let ((pages (mapcar (curry #'apply #'make-page) +pages+))
	  (constants (list
		      (make-map "clock" (<core:simple-clock))
		      (make-map "menu" (make-menu +pages+)))))
      (authorize application user
		 (make-manager-controller :pages pages :constants constants)))))

;; (defmethod/local get-component ((self manager-component) name)
;;   (cond
;;     ((equal name "info") (info-component))
;;     ((equal name "sites") (sites-component))
;;     ((equal name "settings") (settings-component))
;;     (t nil)))

;; (defmethod/remote load-component ((self manager-component) name)
;;   (let ((ctor (get-component self name)))
;;     (if ctor
;; 	(progn
;; 	  (when (slot-value (_content self) 'destroy)
;; 	    (destroy (_content self)))
;; 	  (call/cc ctor (_content self))))))

;; (defmethod/remote menu-template ((self manager-component))
;;   (<:ul
;;    (<:li (<:a :class "hilight" :onclick (lifte (load-component self "info")) "INFO"))
;;    (<:li (<:a :onclick (lifte (load-component self "sites")) "SITES"))
;;    (<:li (<:a :onclick (lifte (load-component self "settings")) "SETTINGS"))))

;; (defmethod/remote init ((self manager-component))
;;   (_debug "manager ready!")
;;   (setf (_clock self) (call/cc (_clock self) (document.get-element-by-id "clock")))
;;   (let ((menu (setf (_menu self) (document.get-element-by-id "menu"))))
;;     (append menu (menu-template self))
;;     (setf (_content self) (document.get-element-by-id "content"))
;;     (load-component self "info")))
