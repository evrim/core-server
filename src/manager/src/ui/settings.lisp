(in-package :manager)

(defclass+ facebook-credentials ()
  ((app-id :host both)
   (app-secret :host both))
  (:ctor make-facebook-credentials))

(defwebcrud facebook-app-crud ()
  ((app-id :label "Application ID")
   (app-secret :label "Application Secret"))
  (:default-initargs :title "Facebook Application Configuration"))

(defclass+ google-credentials ()
  ((client-id :host both)
   (client-secret :host both))
  (:ctor make-google-credentials))

(defwebcrud google-app-crud ()
  ((client-id :label "Client ID")
   (client-secret :label "Client Secret"))
  (:default-initargs :title "Google API Configuration"))

;; -------------------------------------------------------------------------
;; Settings Component
;; -------------------------------------------------------------------------
(defcomponent <manager:settings (<widget:tab)
  ((_facebook-crud :host remote :initform (facebook-app-crud))
   (_google-crud :host remote :initform (google-app-crud)))
  (:default-initargs :tabs '("Facebook API" "Google API")))

(defmethod/local get-config ((self <manager:settings) tab)
  (cond
    ((equal tab "Facebook API")
     (or (database.get application 'facebook) (jobject)))
    ((equal tab "Google API")
     (or (database.get application 'google) (jobject)))))

(defmethod/local save-config ((self <manager:settings) tab args)
  (cond
    ((equal tab "Facebook API")
     (setf (database.get application 'facebook)
	   (apply #'make-facebook-credentials (jobject.attributes args))))
    ((equal tab "Google API")
     (setf (database.get application 'google)
	   (apply #'make-google-credentials (jobject.attributes args))))))

(defmethod/local delete-config ((self <manager:settings) tab)
  (cond
    ((equal tab "Facebook API")
     (setf (database.get application 'facebook) nil))
    ((equal tab "Google API")
     (setf (database.get application 'google) nil))))

(defmethod/remote make-tab ((self <manager:settings) tab)
  (let* ((_config (get-config self tab))
	 (_crud
	  (cond
	    ((eq tab "Facebook API")
	     (make-component (_facebook-crud self) :instance _config))
	    ((eq tab "Google API")
	     (make-component (_google-crud self) :instance _config)))))
    (make-web-thread
     (lambda ()
       (destructuring-bind (action args) (call-component _crud)
	 (cond
	   ((eq action "update")
	    (save-config self tab args))
	   ((eq action "delete")
	    (delete-config self tab)))
	 (show-tab self tab))))
    _crud))

(defmethod/remote show-tab ((self <manager:settings) tab)
  (let ((_tab (make-tab self tab)))
    (with-slots (_content) self
      (replace-node _content (setf (_content self) _tab))
      (call-next-method self tab))))

(defmethod/remote init ((self <manager:settings))
  (call-next-method self))

