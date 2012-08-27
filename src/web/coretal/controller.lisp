(in-package :core-server)

;; +-------------------------------------------------------------------------
;; | Portal Controller
;; +-------------------------------------------------------------------------
(defcomponent <core:portal-controller (<core:taskbar)
  ((core-css :host remote :initform +core.css+)
   (default-page :host both :initform "index")
   (plugins :host both :initform nil :type plugin*)
   (v2-compat-mode :host both :initform nil)
   (greeting :host both :initform "[Core-serveR] ready.")
   (language :host both :initform (name +default-language-pack+))
   (session-id :host both :initform nil)
   (loaded-p :host remote :initform nil)
   (language-pack :host remote :initform +default-language-pack+)
   (session-variable :host remote :initform "core-session")))

(defmethod shared-initialize :after ((self <core:portal-controller) slots &rest rest)
  (declare (ignore rest))
  (setf (language-pack self) (or (find-language-pack (language self))
				 +default-language-pack+)))

(defmethod/remote destroy ((self <core:portal-controller))
  (remove-css (core-css self))
  (call-next-method self))

(defmethod/remote load-plugins ((self <core:portal-controller))
  (mapcar-cc (lambda (plugin)
	       (_debug (list "Loading plugin" plugin))
	       (call/cc plugin self))
	     (plugins self)))

(defmethod/remote redirect-page ((self <core:portal-controller) redirect-to)
  (setf window.location redirect-to)
  (suspend))

(defmethod/remote make-about-menu ((self <core:portal-controller))
  (<:a :href "http://labs.core.gen.tr/" :target "_blank"
       (_ "About [Core-serveR]")
       (<:span :class "subtitle" (_ "Learn more about our server"))))

(defmethod/remote init ((self <core:portal-controller))
  (when (session-id self)
    (set-cookie (session-variable self) (session-id self)))
  
  (load-css (core-css self))
  (setf +default-language+ (language self)
	(language-pack self) (make-component (language-pack self)))

  (call-next-method self)
  (load-plugins self)
  (setf (loaded-p self) t)
  
  (add-menu self (make-about-menu self))  
  (add-menu self (<:a :onclick (lifte (hide-taskbar self)) (_ "Close")
		      (<:span :class "subtitle" (_ "Hide this taskbar"))))

  (when (greeting self)
    (toast self (greeting self))))

;; -------------------------------------------------------------------------
;; Authorize
;; -------------------------------------------------------------------------
(defmethod authorize ((application application) (user t) (self <core:portal-controller))
  (<core:portal-controller :plugins (authorize application user (plugins self))
			   :default-page (default-page self)
			   :v2-compat-mode (v2-compat-mode self)
			   :language (language self)
			   :greeting (greeting self)
			   :session-id (session-id self)
			   :session-variable (session-variable self)))