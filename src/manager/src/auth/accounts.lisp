(in-package :manager)

(defcomponent account-crud-mixin ()
  ())

(defmethod/remote view-buttons ((self account-crud-mixin))
  (list
   (<:input :type "button" :value "Use this account"
	    :title "Use this account"
	    :onclick (lifte
		      (answer-component
		       self (list "use" (slot-value self 'instance)))))))

;; -------------------------------------------------------------------------
;; Facebook Account CRUD
;; -------------------------------------------------------------------------
(defwebcrud facebook-account/crud (account-crud-mixin)
  ((first-name :label "First Name")
   (last-name :label "Last Name")
   (email :label "E-mail"))
  (:default-initargs :title nil))

(defcomponent facebook-account/view (remote-reference)
  ((account :host lift :type facebook-account)
   (first-name :host remote :lift t)
   (last-name :host remote :lift t)
   (email :host remote :lift t)))

;; -------------------------------------------------------------------------
;; Twitter Account CRUD
;; -------------------------------------------------------------------------
(defwebcrud twitter-account/crud (account-crud-mixin)
  ((name :label "Name")
   (screen-name :label "Screen Name")
   (description :label "Description"))
  (:default-initargs :title nil))

(defcomponent twitter-account/view (remote-reference)
  ((account :host lift :type twitter-account)
   (name :host remote :lift t)
   (screen-name :host remote :lift t)
   (description :host remote :lift t)))

;; -------------------------------------------------------------------------
;; Google Account CRUD
;; -------------------------------------------------------------------------
(defwebcrud google-account/crud (account-crud-mixin)
  ((first-name :label "First Name")
   (last-name :label "Last Name")
   (email :label "E-mail"))
  (:default-initargs :title nil))

(defcomponent google-account/view (remote-reference)
  ((account :host lift :type google-account)
   (first-name :host remote :lift t)
   (last-name :host remote :lift t)
   (email :host remote :lift t)))

;; -------------------------------------------------------------------------
;; Account Widget
;; -------------------------------------------------------------------------
(defcomponent <manager:accounts (secure-object <widget:simple)
  ((default-account :host local))
  (:default-initargs
   :levels '(<manager:accounts/registered)
   :permissions '((owner . 0) (group . 0) (other . 0) (anonymous . -1)
		  (unauthorized . -1))
   :owner (make-user :name "root")
   :group (make-simple-group :name "users")))

(defmethod authorize ((self manager-application) (user user)
		      (accounts <manager:accounts))
  (apply (core-server::%secure-constructor accounts user)
	 (list :secure-object accounts
	       :user user
	       :current-application self
	       :owner (secure.owner accounts)
	       :group (secure.group accounts)
	       :default-tab (aif (default-account accounts)
				 (format nil "~@(~A~)" (account.provider it))
				 "Coretal"))))

(defparameter +account-cruds+
  (list (cons "Facebook" (facebook-account/crud))
	(cons "Twitter" (twitter-account/crud))
	(cons "Google" (google-account/crud))))

(defcomponent <manager:accounts/registered (secure-object/authorized
					    <widget:tab)
  ((secure-object :host lift :type <manager:accounts)
   (accounts :host local :export nil)
   (default-account :host remote :lift t)
   (account-cruds :host remote :initform +account-cruds+))
  (:default-initargs :tabs (list "Coretal" "Facebook" "Google" "Twitter")
		     :tab-title "Accounts"))

(defmethod/local get-account ((self <manager:accounts/registered) name)
  (let ((accounts (user.accounts (secure.user self))))
    (acond
     ((and (equal "Coretal" name)
	   (any (make-type-matcher 'local-account) accounts))
      (coretal-account/crud :instance it))
     ((and (equal "Facebook" name)
	   (any (make-type-matcher 'facebook-account) accounts))
      (facebook-account/view :account it))
     ((and (equal "Google" name)
	   (any (make-type-matcher 'google-account) accounts))
      (google-account/view :account it))
     ((and (equal "Twitter" name)
	   (any (make-type-matcher 'twitter-account) accounts))
      (twitter-account/view :account it)))))

(defmethod/local do-use1 ((self <manager:accounts/registered) account)
  (answer-component self (list self :use (slot-value account 'account))))

(defmethod/local external-login-url ((self <manager:accounts/registered) provider)
  (let ((provider (find (string-upcase provider) '(facebook yahoo google twitter)
			:test #'string=)))
    (answer-component self (list self :login provider))))

(defmethod/remote do-external-login ((self <manager:accounts/registered) provider)
  (setf window.location (external-login-url self provider)))

(defmethod/remote core-server::make-tab ((self <manager:accounts/registered) tab)
  (let ((account (get-account self tab)))
    (cond
      (account
       (let* ((_crud (find-cc (lambda (a) (equal (car a) tab)) (account-cruds self)))
	      (_crud (make-component (car (cdr _crud)) :instance account)))
    
	 (make-web-thread
	  (lambda ()
	    (destructuring-bind (action account) (call-component _crud)
	      (cond
		((equal action "use") (do-use1 self account))
		(t (throw (new (*error (+ "unhandled action:" action ",args:" args)))))))))
	 _crud))
      ((equal tab "Facebook")
       (<:div :class "auth-buttons center text-center"
	(<:p
	 (<:a :class "btn-auth btn-facebook"
	      :onclick (lifte (do-external-login self "facebook"))
	  (_ "Add My") (<:b " Facebook ")  (_ "Account")))))
      ((equal tab "Google")
       (<:div :class "auth-buttons center text-center"
	(<:p
	 (<:a :class "btn-auth btn-google"
	      :onclick (lifte (do-external-login self "google"))
	  (_ "Add My") (<:b " Google ")  (_ "Account")))))
      ((equal tab "Twitter")
       (<:div :class "auth-buttons center text-center"
	(<:p
	 (<:a :class "btn-auth btn-twitter"
	      :onclick (lifte (do-external-login self "twitter"))
	  (_ "Add My") (<:b " Twitter ")  (_ "Account")))))
      ((equal tab "Yahoo")
       (<:div :class "auth-buttons center text-center"
	(<:p
	 (<:a :class "btn-auth btn-yahoo"
	      :onclick (lifte (do-external-login self "yahoo"))
	  (_ "Add My") (<:b " Yahoo ")  (_ "Account"))))))))


