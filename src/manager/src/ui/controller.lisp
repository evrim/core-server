(in-package :manager)

;; -------------------------------------------------------------------------
;; Page & Widget Definitions
;; -------------------------------------------------------------------------
(defparameter +pages+
  (list (list "info" "Info"
	      "Information regarding to the current instance"
	      (cons "content" (<manager:server-info)))
	(list "server" "Server" "Manage current core server instance"
	      (cons "content" (<manager:server)))
	(list "apps" "Applications"
	      "Applications currently deployed on this server"
	      (cons "content" (<manager:applications)))
	(list "aanda" "A & A" "Authentication & Authorization"
	      (cons "content" (sites-component)))
	(list "users" "Administrators"
	      "Administrative accounts that manage this server instance"
	      (cons "content" (administrators-component)))
	(list "settings" "Settings"
	      "Settings related to the current server instance"
	      (cons "content" (<manager:settings))))
  "Simple Page & Widget Definitions")

;; -------------------------------------------------------------------------
;; Manager Component
;; -------------------------------------------------------------------------
(defcomponent manager-controller (simple-controller)
  ((_menu :host remote)
   (_content :host remote))
  (:default-initargs :default-page "info")
  (:ctor make-manager-controller))

;; -------------------------------------------------------------------------
;; Make Page
;; -------------------------------------------------------------------------
(defun make-page (name title description &rest widgets)
  (<core:simple-page :name name
   (cons
    (<core:simple-widget-map :selector "title"
			     :widget (<widget:simple-content
				      (<:h1 title)
				      (<:h2 description)))
    (mapcar (lambda (widget)
	      (destructuring-bind (selector . widget) widget
		(<core:simple-widget-map :selector selector
					 :widget widget)))
	    widgets))))

(defun make-pages (&optional (pages +pages+))
  (mapcar (curry #'apply #'make-page) pages))

;; -------------------------------------------------------------------------
;; Make Menu
;; -------------------------------------------------------------------------
(defun make-menu (&optional (pages +pages+))
  (<core:simple-widget-map :selector "menu"
			   :widget
			   (<widget:simple-menu
			    (mapcar (lambda (page)
				      (jobject :name (car page)
					       :title (cadr page)))
				    pages))))

;; -------------------------------------------------------------------------
;; Make Clock
;; -------------------------------------------------------------------------
(defun make-clock ()
  (<core:simple-widget-map :selector "clock"
			   :widget (<core:simple-clock)))

;; -------------------------------------------------------------------------
;; Make Controller
;; -------------------------------------------------------------------------
(defun make-controller (application user)
  (authorize application user
	     (make-manager-controller :pages (make-pages +pages+)
				      :constants (list (make-menu +pages+)
						       (make-clock)))))
