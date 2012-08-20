(in-package :manager)

;; -------------------------------------------------------------------------
;; Crud's
;; -------------------------------------------------------------------------
(defwebcrud facebook-app-crud ()
  ((app-id :label "Application ID")
   (app-secret :label "Application Secret"))
  (:default-initargs :title "Facebook Application Configuration"))

(defwebcrud google-app-crud ()
  ((client-id :label "Client ID")
   (client-secret :label "Client Secret"))
  (:default-initargs :title "Google API Configuration"))

(defwebcrud twitter-app-crud ()
  ((consumer-key :label "Consumer Key")
   (consumer-secret :label "Consumer Secret"))
  (:default-initargs :title "Twitter API Configuration"))

(defwebcrud yahoo-app-crud ()
  ((consumer-key :label "Consumer Key")
   (consumer-secret :label "Consumer Secret"))
  (:default-initargs :title "Yahoo API Configuration"))

;; -------------------------------------------------------------------------
;; Settings Component
;; -------------------------------------------------------------------------
(defcomponent <manager:settings (<widget:tab)
  ((_facebook-crud :host remote :initform (facebook-app-crud))
   (_google-crud :host remote :initform (google-app-crud))
   (_twitter-crud :host remote :initform (twitter-app-crud))
   (_yahoo-crud :host remote :initform (yahoo-app-crud)))
  (:default-initargs :tab-title "Server Settings"
		     :tabs '("Facebook API" "Google API" "Twitter API" "Yahoo API")))

(defmethod/local get-config ((self <manager:settings) tab)
  (or (cond
	((equal tab "Facebook API") (database.get application :facebook))
	((equal tab "Google API") (database.get application :google))
	((equal tab "Twitter API") (database.get application :twitter))
	((equal tab "Yahoo API") (database.get application :yahoo)))
      (jobject)))

(defmethod/local save-config ((self <manager:settings) tab args)
  (cond
    ((equal tab "Facebook API")
     (setf (database.get application :facebook)
	   (apply #'make-facebook-credentials (jobject.attributes args))))
    ((equal tab "Google API")
     (setf (database.get application :google)
	   (apply #'make-google-credentials (jobject.attributes args))))
    ((equal tab "Twitter API")
     (setf (database.get application :twitter)
	   (apply #'make-twitter-credentials (jobject.attributes args))))
    ((equal tab "Yahoo API")
     (setf (database.get application :yahoo)
	   (apply #'make-yahoo-credentials (jobject.attributes args))))))

(defmethod/local delete-config ((self <manager:settings) tab)
  (cond
    ((equal tab "Facebook API")
     (setf (database.get application :facebook) nil))
    ((equal tab "Google API")
     (setf (database.get application :google) nil))
    ((equal tab "Twitter API")
     (setf (database.get application :twitter) nil))
    ((equal tab "Yahoo API")
     (setf (database.get application :yahoo) nil))))

(defmethod/remote core-server::make-tab ((self <manager:settings) tab)
  (let* ((_config (get-config self tab))
	 (_crud
	  (cond
	    ((eq tab "Facebook API")
	     (make-component (_facebook-crud self) :instance _config))
	    ((eq tab "Google API")
	     (make-component (_google-crud self) :instance _config))
	    ((eq tab "Twitter API")
	     (make-component (_twitter-crud self) :instance _config))
	    ((eq tab "Yahoo API")
	     (make-component (_yahoo-crud self) :instance _config)))))
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
  (call-next-method self tab)
  ;; (let ((_tab (make-tab self tab)))
  ;;   (with-slots (_content) self
  ;;     (replace-node _content (setf (_content self) _tab))
  ;;     (call-next-method self tab)))
  )

(defmethod/remote init ((self <manager:settings))
  (call-next-method self))

