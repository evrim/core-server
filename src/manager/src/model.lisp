(in-package :manager)

;; -------------------------------------------------------------------------
;; Manager Users
;; -------------------------------------------------------------------------
(defclass+ manager-user (simple-user)
  ((username :host both :print t :index t)
   (password :host both)))

(defcrud manager-user)

;; -------------------------------------------------------------------------
;; Site Definition
;; -------------------------------------------------------------------------
(defclass+ site (object-with-id)
  ((fqdn :host both :print t :documentation "FQDN of remote site")
   (api-key :host local :export nil :print t
	    :documentation "Api key of this site")
   (api-password :host local :export nil
		 :documentation "Api pass of this site"))
  (:ctor make-site))

;; -------------------------------------------------------------------------
;; Account Definition
;; -------------------------------------------------------------------------
(defclass+ account (object-with-id)
  ((user :host local :type user :relation accounts :export nil
	 :documentation "Associated coretal4-user to this account"))
  (:ctor make-account))

;; -------------------------------------------------------------------------
;; Local Account Definition
;; -------------------------------------------------------------------------
(defclass+ local-account (account)
  ((email :host local :print t :index t
	  :documentation "Email address of the local account")
   (password :host local :print t
	     :documentation "Password of the local account"))
  (:ctor make-local-account))

;; -------------------------------------------------------------------------
;; External Account Definition
;; -------------------------------------------------------------------------
(defclass+ external-account (account)
  ((username :host local :print t :index t
	     :documentation "Username association with this account")
   (token :host local :export nil
	  :documentation "Token associated to this account"))
  (:ctor make-external-account))

;; -------------------------------------------------------------------------
;; Facebook Account Definition
;; -------------------------------------------------------------------------
(defclass+ fb-account (external-account)
  ()
  (:ctor make-fb-account))

;; -------------------------------------------------------------------------
;; Google Account
;; -------------------------------------------------------------------------
(defclass+ google-account (external-account)
  ()
  (:ctor make-google-account))

;; -------------------------------------------------------------------------
;; Twitter Account
;; -------------------------------------------------------------------------
(defclass+ twitter-account (external-account)
  ()
  (:ctor make-twitter-account))

;; -------------------------------------------------------------------------
;; Profile Definition
;; -------------------------------------------------------------------------
(defcomponent profile (object-with-id)
  ((email :host both :print t :index t :documentation "User Profile Email")
   (name :host both :print t
	 :documentation "Name of this profile (ie Google)")
   (username :host both :documentation "Visible name")
   (avatar :host both :documentation "Avatar URL of this profile")
   (account :host both :type account :relation profiles
	    :documentation "Associated account to this profile"))
  (:ctor make-profile))

;; -------------------------------------------------------------------------
;; Profile Association Definition
;; -------------------------------------------------------------------------
(defcomponent profile-association (object-with-id)
  ((site :host both :print t :type site
	 :documentation "Site that this profile is associated with")
   (profile :host both :print t :type profile
	    :documentation "Profile that this association points to")
   (user :host both :print t :type user :relation associations
	 :documentation "Owner account of this association"))
  (:ctor make-profile-association))

;; -------------------------------------------------------------------------
;; User Definition
;; -------------------------------------------------------------------------
(defclass+ user (object-with-id)
  ((accounts :host both :type account* :relation user
	     :documentation "Associated accounts to this user")
   (profiles :host both :type profile* :relation account
	     :documentation "Associated profiles to this account")
   (default-profile :host both :type profile
		    :documentation "Default profile of this account")
   (associations :host both :type profile-association* :relation user
		 :documentation "Profile associations"))
  (:ctor make-user))

;; -------------------------------------------------------------------------
;; CRUD Definitions
;; -------------------------------------------------------------------------
(defcrud site)
(defcrud account)
(defcrud fb-account)
(defcrud google-account)
(defcrud twitter-account)
(defcrud profile)
(defcrud profile-association)
(defcrud user)
