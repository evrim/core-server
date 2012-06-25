(in-package :core-server.test)

;; ;; -------------------------------------------------------------------------
;; ;; Manager Users Table
;; ;; -------------------------------------------------------------------------
;; (deftable manager-users-table ()
;;   ((name :label "Name")   
;;    (username :label "Username")
;;    (creation-timestamp :label "Creation Timestamp" :remote-type timestamp)))

;; ;; -------------------------------------------------------------------------
;; ;; Manager Users Crud
;; ;; -------------------------------------------------------------------------
;; (defwebcrud manager-user-crud ()
;;   ((name :label "Name")
;;    (username :label "Username" :read-only t)
;;    (password :label "Password" :remote-type password)
;;    (creation-timestamp :label "Creation Timestamp" :remote-type date ;; timestamp
;; 		       )
;;    (select-text :label "Select Text" :options '("Text1" "Text2")
;; 		:remote-type select)
;;    (multiselect-text :label "MultiSelect Text" :options '("Text1" "Text2")
;; 		     :remote-type multiple-select)
;;    (multicheckbox-text :label "MultiCheckbox" :options '("Text1" "Text2")
;; 		       :remote-type multiple-checkbox)
;;    (number-value :label "A Number" :remote-type number)
;;    (email-value :label "Email" :remote-type email)
;;    (html-value :label "HTML Value" :remote-type html))
;;   (:default-initargs :title "Administrative Account" :editable-p t :deletable-p t))

;; ;; -------------------------------------------------------------------------
;; ;; Sites Component 
;; ;; -------------------------------------------------------------------------
;; (defcomponent manager-users-component (<core:table-with-crud <widget:simple-widget)
;;   ()
;;   (:default-initargs :table (manager-users-table) :crud (manager-user-crud)
;; 		     :input-element (<core:required-value-input
;; 				     :default-value "Enter username (ie root)")
;; 		     :table-title "Users"))

;; (defmethod/local get-instances ((self manager-users-component))
;;   (manager-user.list application))

;; (defmethod/local add-instance ((self manager-users-component) username)
;;   (manager-user.add application :username username))

;; (defmethod/local delete-instance ((self manager-users-component) username)
;;   (aif (manager-user.find application :username username)
;;        (prog1 t (manager-user.delete application it))))

;; (defmethod/local update-instance ((self manager-users-component)
;; 				  (user manager-user) updates)
;;   (prog1 updates
;;     (apply #'manager-user.update application (cons user (jobject.attributes updates)))))
