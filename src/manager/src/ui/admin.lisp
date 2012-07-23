(in-package :manager)

;; -------------------------------------------------------------------------
;; Admin/Authorized
;; -------------------------------------------------------------------------
(defcomponent admin/authorized (secure-object/authorized
				admin remote-reference)
  ((secure-object :host lift :type admin)
   (username :lift t :host remote)
   (password :lift t :host remote)
   (name :lift t :host remote)
   (creation-timestamp :lift t :host remote)))

(defcrud/lift admin/authorized admin)

;; -------------------------------------------------------------------------
;; Admin Table
;; -------------------------------------------------------------------------
(deftable <manager:admin/table ()
  ((name :label "Name")   
   (username :label "Username")
   (creation-timestamp :label "Creation Timestamp" :remote-type timestamp)))

;; -------------------------------------------------------------------------
;; Manager Users Crud
;; -------------------------------------------------------------------------
(defwebcrud <manager:admin/crud ()
  ((name :label "Name")
   (username :label "Username" :read-only t)
   (password :label "Password" :remote-type password)
   (creation-timestamp :label "Creation Timestamp" :remote-type date
		       :read-only t))
  (:default-initargs :title "Administrative Account"
    :editable-p t :deletable-p t))

;; -------------------------------------------------------------------------
;; Sites Component 
;; -------------------------------------------------------------------------
(defcomponent <manager:administrators (<core:table-with-crud <widget:simple)
  ()
  (:default-initargs :table-title "Administrators"
    :table (<manager:admin/table)
    :crud (<manager:admin/crud)
    :input-element (<core:required-value-input :default-value
					       "Enter username (ie root)")))

(defmethod/local get-instances ((self <manager:administrators))
  (admin.list application))

(defmethod/local add-instance ((self <manager:administrators) username)
  (admin.add application :username username))

(defmethod/local delete-instance ((self <manager:administrators)
				  (user admin))
  (prog1 t (admin.delete application user)))

(defmethod/local update-instance ((self <manager:administrators)
				  (user admin) updates)
  (prog1 updates
    (apply #'admin.update application
	   (cons user (jobject.attributes updates)))))

