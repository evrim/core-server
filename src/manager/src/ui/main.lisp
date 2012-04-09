(in-package :manager)

;; -------------------------------------------------------------------------
;; Info Component
;; -------------------------------------------------------------------------
(defcomponent info-component (<widget:simple-widget)
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
(defcomponent settings-component (<widget:simple-widget)
  ())

;; -------------------------------------------------------------------------
;; Manager Component
;; -------------------------------------------------------------------------
(defcomponent manager-controller (simple-controller)
  ((_menu :host remote)
   (_content :host remote))
  (:default-initargs :default-page "info")
  (:ctor make-manager-controller))

(defparameter +pages+
  (list (list "info" "Info" "Information regarding to the current instance"
	      (info-component))
	(list "aanda" "A & A" "Authentication & Authorization"
	      (sites-component))
	(list "users" "Manager Users" "Accounts that manage this server instance"
	      (manager-users-component))
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
