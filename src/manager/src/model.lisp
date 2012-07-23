(in-package :manager)

;; +-------------------------------------------------------------------------
;; | Administrator Definition
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

;; +-------------------------------------------------------------------------
;; | Site Definition
;; +-------------------------------------------------------------------------
(defclass+ site (secure-object object-with-id)
  ((fqdn :host local :index t :print t :documentation "FQDN" :type string)
   (creation-timestamp :host both :type integer
		       :documentation "Creation timestamp"
		       :initform (get-universal-time)))
  (:ctor make-site)
  (:default-initargs
    :permissions '((owner . 0) (group . 0) (other . 0) (unauthorized . -1))
    :levels '(site/authorized)))

;; -------------------------------------------------------------------------
;; User Definition
;; -------------------------------------------------------------------------
(defclass+ user (object-with-id simple-user)
  ((accounts :host local :type abstract-account :relation user
	     :documentation "Associated accounts")))

;; -------------------------------------------------------------------------
;; Account Definition
;; -------------------------------------------------------------------------
(defclass+ abstract-account (object-with-id)
  ((user :host local :type user :relation accounts
	 :documentation "Associated User"))
  (:ctor %make-abstract-account))

;; -------------------------------------------------------------------------
;; Local Account Definition
;; -------------------------------------------------------------------------
(defclass+ local-account (abstract-account)
  ((email :host local :print t :index t :documentation "Email address")
   (password :host local :print t :documentation "Password"))
  (:ctor make-local-account))

;; -------------------------------------------------------------------------
;; External Account Definition
;; -------------------------------------------------------------------------
(defclass+ abstract-external-account (abstract-account)
  ((username :host local :print t :index t
	     :documentation "Username associated")
   (token :host local :documentation "Token associated")
   (token-timestamp :host local :documentation "Token timestamp"))
  (:ctor %make-abstract-external-account))

;; -------------------------------------------------------------------------
;; Facebook Account Definition
;; -------------------------------------------------------------------------
(defclass+ fb-account (abstract-external-account)
  ()
  (:ctor make-fb-account))

;; -------------------------------------------------------------------------
;; Google Account
;; -------------------------------------------------------------------------
(defclass+ google-account (abstract-external-account)
  ()
  (:ctor make-google-account))

;; -------------------------------------------------------------------------
;; Twitter Account
;; -------------------------------------------------------------------------
(defclass+ twitter-account (abstract-external-account)
  ()
  (:ctor make-twitter-account))

(defcrud admin)
(defcrud site)
(defcrud user)
(defcrud local-account)
(defcrud fb-account)
(defcrud google-account)
(defcrud twitter-account)

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
