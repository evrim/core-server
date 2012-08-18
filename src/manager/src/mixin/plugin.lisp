;; -------------------------------------------------------------------------
;; Manager Plugin
;; -------------------------------------------------------------------------
(in-package :manager)

(defcomponent <manager:authentication-plugin (secure-object)
  ()
  (:default-initargs
   :levels '(<manager:authentication-plugin/anonymous
	     <manager:authentication-plugin/user)
   :permissions '((OWNER . 1) (GROUP . 1) (OTHER . 1) (ANONYMOUS . 0) (UNAUTHORIZED . -1))
   :group (make-simple-group :name "admin")
   :owner (make-simple-user :name "root")))

;; -------------------------------------------------------------------------
;; Anonymous Plugin
;; -------------------------------------------------------------------------
(defplugin <manager:authentication-plugin/anonymous (secure-object/authorized)
  ())

(defmethod/local login-with-token ((self <manager:authentication-plugin/anonymous) token)
  (with-slots (oauth-key oauth-secret) application
    (describe (list 'trying-to-get-user token))
    (let ((user (<core-server:login :token token
				    :debug-p t
				    ;; :cache-p nil
				    :url (web-application.api-uri application "login")
				    :username oauth-key
				    :password oauth-secret)))
      (describe (list 'just-got-user token))
      (setf (query-session :token) token)
      (answer-component self (list self :login user)))))

(defmethod/remote do-login ((self <manager:authentication-plugin/anonymous) token)
  (let ((ctor (login-with-token self token)))
    (destroy self) ;; Destroy current controller
    (call/cc ctor self))) ;; Make new controller instance

;; -------------------------------------------------------------------------
;; User Plugin
;; -------------------------------------------------------------------------
(defplugin <manager:authentication-plugin/user (secure-object/authorized)
  ())

(defmethod/local logout ((self <manager:authentication-plugin/user))
  (with-slots (oauth-key oauth-secret) application
    (let* ((token (query-session :token))
	   (response (<core-server:logout :token token
					  :url (web-application.api-uri application "logout")
					  :username oauth-key
					  :password oauth-secret)))
      (answer-component self (list self :logout response)))))

(defmethod/remote do-logout ((self <manager:authentication-plugin/user))
  (let ((ctor (logout self)))
    (destroy self) ;; Destroy current controller
    (call/cc ctor self))) ;; Make new controller instance
