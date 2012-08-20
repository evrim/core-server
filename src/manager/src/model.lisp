;; +-------------------------------------------------------------------------
;; | Manager Application Model
;; +-------------------------------------------------------------------------
(in-package :manager)

;; -------------------------------------------------------------------------
;; Server OAuth Credentials
;; -------------------------------------------------------------------------
(defclass+ facebook-credentials ()
  ((app-id :host both)
   (app-secret :host both))
  (:ctor make-facebook-credentials))

(defclass+ google-credentials ()
  ((client-id :host both)
   (client-secret :host both))
  (:ctor make-google-credentials))

(defclass+ twitter-credentials ()
  ((consumer-key :host both)
   (consumer-secret :host both))
  (:ctor make-twitter-credentials))

(defclass+ yahoo-credentials ()
  ((consumer-key :host both)
   (consumer-secret :host both))
  (:ctor make-yahoo-credentials))

;; +-------------------------------------------------------------------------
;; | Administrator Definition (Manager Application Users)
;; +-------------------------------------------------------------------------
(defclass+ admin (secure-object simple-user)
  ((username :host local :print t :index t :documentation "Admin Username")
   (password :host local :documentation "Admin Password")
   (creation-timestamp :host local :type integer
		       :initform (get-universal-time)))
  (:ctor make-admin)
  (:default-initargs
   :permissions '((owner . 0) (group . 0) (other . 0) (unauthorized . -1))
   :levels '(admin/authorized)))

;; -------------------------------------------------------------------------
;; Account Definition
;; -------------------------------------------------------------------------
(defclass+ account (object-with-id)
  ((user :host local :type user :relation accounts
	 :documentation "Associated User")
   (provider :initform (error "Provide :provider"))
   (last-update :host local :documentation "Last update time of this account")
   (name :host local :documentation "Name of this person"))
  (:ctor %make-account))

;; -------------------------------------------------------------------------
;; Local Account Definition
;; -------------------------------------------------------------------------
(defclass+ local-account (account)
  ((email :host local :print t :index t :documentation "Email address")
   (password :host local :documentation "Password"))
  (:ctor make-local-account)
  (:default-initargs :provider 'coretal))

;; -------------------------------------------------------------------------
;; External Account Definition
;; -------------------------------------------------------------------------
(defclass+ external-account (account)
  ((account-id :host local :print t :index t :documentation "Account ID"
	       :accessor account.account-id)
   (token :host local :documentation "Token associated with this account"))
  (:ctor %make-external-account))

;; -------------------------------------------------------------------------
;; Facebook Account Definition
;; -------------------------------------------------------------------------
;; #<JOBJECT {100D4774D3}> [standard-object]

;; Slots with :INSTANCE allocation:
;;   ATTRIBUTES  = (:METADATA #<JOBJECT {100D4760F3}> :UPDATED_TIME
;;  "2012-06-25T18:36:31+0000" :VERIFIED TRUE :LOCALE "en_US" :TIMEZONE 3
;;  :EMAIL "evrimulu@gmail.com" :GENDER "male" :FAVORITE_ATHLETES
;;  (#<JOBJECT {100CFD0A13}>) :LOCATION #<JOBJECT {100CFC86E3}> :USERNAME
;;  "eevrimulu" :LINK "http://www.facebook.com/eevrimulu" :LAST_NAME "Ulu"
;;  :FIRST_NAME "Evrim" :NAME "Evrim Ulu" :ID "700518347")

(defclass+ facebook-account (external-account)
  ((username :host local)
   (first-name :host local)
   (last-name :host local)
   (email :host local)
   (verified :host local)
   (timezone :host local)
   (locale :host local)
   (location :host local)
   (gender :host local)
   (link :host local))
  (:ctor make-facebook-account)
  (:default-initargs :provider 'facebook))

;; -------------------------------------------------------------------------
;; Google Account
;; -------------------------------------------------------------------------
(defclass+ google-account (external-account)
  ((locale :host local)
   (gender :host local)
   (picture :host local)
   (link :host local)
   (last-name :host local)
   (first-name :host local)
   (verified :host local)
   (email :host local)
   (last-update :host local))
  (:ctor make-google-account)
  (:default-initargs :provider 'google))

;; -------------------------------------------------------------------------
;; Twitter Account
;; -------------------------------------------------------------------------
(defclass+ twitter-account (external-account)
  ((lang :host local)
   (verified :host local)
   (geo-enabled :host local)
   (time-zone :host local)
   (utc-offset :host local)
   (created-at :host local)
   (protected :host local)
   (description :host local)
   (url :host local)
   (location :host local)
   (screen-name :host local))
  (:ctor make-twitter-account)
  (:default-initargs :provider 'twitter))

;; -------------------------------------------------------------------------
;; Yahoo Account
;; -------------------------------------------------------------------------
(defclass+ yahoo-account (external-account)
  ((nickname :host local)
   (first-name :host local)
   (last-name :host local))
  (:ctor make-yahoo-account)
  (:default-initargs :provider 'yahoo))

;; -------------------------------------------------------------------------
;; User Definition
;; -------------------------------------------------------------------------
(defclass+ user (object-with-id simple-user)
  ((accounts :host local :type account* :relation user
	     :documentation "Associated accounts" :leaf t)
   (default-account :host local :type account
		    :documentation "Default Account of this user")
   (associations :host local :type account-associations :relation user))
  (:ctor make-user))

(defmethod user.default-account ((self user))
  (with-slots (accounts default-account) self
    (or default-account (car accounts))))

;; -------------------------------------------------------------------------
;; Account Association
;; -------------------------------------------------------------------------
(defclass+ account-association (object-with-id)
  ((account :host local :type account)
   (realm :host local :type realm :relation associations)))

;; +-------------------------------------------------------------------------
;; | Realm Definition
;; +-------------------------------------------------------------------------
(defclass+ realm (secure-object object-with-id)
  ((fqdn :host local :index t :print t :documentation "FQDN" :type string)
   (creation-timestamp :host both :type integer :documentation "Creation timestamp"
		       :initform (get-universal-time))
   (associations :host local :type account-association* :relation realm))
  (:ctor make-realm)
  (:default-initargs
    :permissions '((owner . 0) (group . 0) (other . 0) (unauthorized . -1))
    :levels '(realm/authorized)))

;; -------------------------------------------------------------------------
;; Crud Definitions
;; -------------------------------------------------------------------------
(defcrud admin)
(defcrud user)
(defcrud account)
(defcrud local-account)
(defcrud external-account)
(defcrud facebook-account)
(defcrud google-account)
(defcrud twitter-account)
(defcrud account-association)
(defcrud realm)

;; -------------------------------------------------------------------------
;; Access Token
;; -------------------------------------------------------------------------
(defvar +token-lifetime+ (* 12 3600))
(defclass+ access-token ()
  ((timestamp :host local :initform (get-universal-time)
	      :accessor access-token.timestamp)
   (token :host local :initform (random-string 8) :accessor access-token.token)
   (association :host local :initform (error "Provide :association")
		:accessor access-token.association)
   (session-id :host local :initform (error "Provide :session-id")
	       :accessor access-token.session-id))
  (:ctor make-access-token))

(defmethod access-token.expired-p ((self access-token))
  (with-slots (timestamp) self
    (if (> (get-universal-time) (+ timestamp +token-lifetime+))
	t)))


;; (deftransaction user.update-from-facebook ((self database) (user user)
;; 					   (data jobject))
;;   (with-attributes (id) data
;;     (let ((account (any (lambda (account)
;; 			  (and (typep account 'facebook-account)
;; 			       (equal (account.account-id account) id)
;; 			       account))
;; 			(user.accounts user))))
      
;;       (if (null account)
;; 	  (error "No Facebook account found for ~A, Data: ~A" user data))

;;       (prog1 user (facebook-account.update-from-jobject self account data)))))

;; ;; -------------------------------------------------------------------------
;; ;; Manager Users
;; ;; -------------------------------------------------------------------------
;; (defcomponent manager-user (simple-user remote-reference)
;;   ((username :host both :print t :index t :accessor manager-user.username
;; 	     :documentation "Manager Username")
;;    (password :host local :export nil :accessor manager-user.password
;; 	     :documentation "Manager Password")
;;    (creation-timestamp :host both :type integer :initform (get-universal-time)
;; 		       :documentation "Creation timestamp"))
;;   (:ctor make-manager-user))

;; (defcrud manager-user)

;; ;; -------------------------------------------------------------------------
;; ;; Site Definition
;; ;; -------------------------------------------------------------------------
;; (defcomponent site (object-with-id)
;;   ((fqdn :host both :print t :documentation "FQDN of remote site" :index t
;; 	 :type string)
;;    (owner :host both :type manager-user :initform (error "Provide :owner")
;; 	  :documentation "Owner of this site")
;;    (creation-timestamp :host both :type integer :initform (get-universal-time)
;; 		       :documentation "Creation timestamp of this site"))
;;   (:ctor make-site))

;; (defmethod/remote init ((self site))
;;   (setf (owner self) (make-component (owner self))))


;; ;; -------------------------------------------------------------------------
;; ;; Account Definition
;; ;; -------------------------------------------------------------------------
;; (defcomponent account (object-with-id)
;;   ((user :host local :type user :relation accounts :export nil
;; 	 :documentation "Associated coretal4-user to this account"))
;;   (:ctor make-account))

;; ;; -------------------------------------------------------------------------
;; ;; Local Account Definition
;; ;; -------------------------------------------------------------------------
;; (defcomponent local-account (account)
;;   ((email :host local :print t :index t
;; 	  :documentation "Email address of the local account")
;;    (password :host local :print t
;; 	     :documentation "Password of the local account"))
;;   (:ctor make-local-account))

;; ;; -------------------------------------------------------------------------
;; ;; External Account Definition
;; ;; -------------------------------------------------------------------------
;; (defcomponent external-account (account)
;;   ((username :host local :print t :index t
;; 	     :documentation "Username associated to account")
;;    (token :host local :export nil
;; 	  :documentation "Token associated to this account"))
;;   (:ctor make-external-account))

;; ;; -------------------------------------------------------------------------
;; ;; Facebook Account Definition
;; ;; -------------------------------------------------------------------------
;; (defcomponent fb-account (external-account)
;;   ()
;;   (:ctor make-fb-account))

;; ;; -------------------------------------------------------------------------
;; ;; Google Account
;; ;; -------------------------------------------------------------------------
;; (defcomponent google-account (external-account)
;;   ()
;;   (:ctor make-google-account))

;; ;; -------------------------------------------------------------------------
;; ;; Twitter Account
;; ;; -------------------------------------------------------------------------
;; (defcomponent twitter-account (external-account)
;;   ()
;;   (:ctor make-twitter-account))

;; ;; -------------------------------------------------------------------------
;; ;; User Definition
;; ;; -------------------------------------------------------------------------
;; (defcomponent user (object-with-id)
;;   ((accounts :host both :type account* :relation user
;; 	     :documentation "Associated accounts to this user")
;;    (profiles :host both :type profile* :relation account
;; 	     :documentation "Associated profiles to this account")
;;    (default-profile :host both :type profile
;; 		    :documentation "Default profile of this account")
;;    (associations :host both :type profile-association* :relation user
;; 		 :documentation "Profile associations"))
;;   (:ctor make-user))

;; ;; -------------------------------------------------------------------------
;; ;; Profile Definition
;; ;; -------------------------------------------------------------------------
;; (defcomponent profile (object-with-id)
;;   ((email :host both :print t :index t :documentation "User Profile Email")
;;    (name :host both :print t :documentation "Name of this profile (ie Google)")
;;    (visible-name :host both :documentation "Visible name")
;;    (avatar :host both :documentation "Avatar URL of this profile")
;;    (account :host both :type account :relation profiles
;; 	    :documentation "Associated account to this profile"))
;;   (:ctor make-profile))

;; ;; -------------------------------------------------------------------------
;; ;; Profile Association Definition
;; ;; -------------------------------------------------------------------------
;; (defcomponent profile-association (object-with-id)
;;   ((site :host both :print t :type site
;; 	 :documentation "Site that this profile is associated with")
;;    (profile :host both :print t :type profile
;; 	    :documentation "Profile that this association points to")
;;    (user :host both :print t :type user :relation associations
;; 	 :documentation "Owner account of this association"))
;;   (:ctor make-profile-association))


;; ;; -------------------------------------------------------------------------
;; ;; CRUD Definitions
;; ;; -------------------------------------------------------------------------
;; (defcrud site)
;; (defcrud account)
;; (defcrud fb-account)
;; (defcrud google-account)
;; (defcrud twitter-account)
;; (defcrud profile)
;; (defcrud profile-association)
;; (defcrud user)
