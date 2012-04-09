(in-package :manager)

;; -------------------------------------------------------------------------
;; Manager Users Table
;; -------------------------------------------------------------------------
(deftable manager-users-table ()
  ((name :label "Name")   
   (username :label "Username")
   (timestamp :label "Timestamp"
	      :reader
	      (with-call/cc
		(lambda (a)
		  (take 10
			(date-to-string
			 (lisp-date-to-javascript (slot-value a 'timestamp)))))))))

;; -------------------------------------------------------------------------
;; Site Crud
;; -------------------------------------------------------------------------
(defwebcrud manager-user-crud ()
  ((name :label "Name")
   (username :label "Username")
   (password :label "Password")
   (timestamp :label "Creation Timestamp"
	      :reader
	      (with-call/cc
		(lambda (a)
		  (take 10
			(date-to-string
			 (lisp-date-to-javascript (slot-value a 'timestamp))))))))
  (:default-initargs :title "Manager User" :editable-p t :deletable-p t))

;; -------------------------------------------------------------------------
;; Sites Component 
;; -------------------------------------------------------------------------
(defcomponent manager-users-component (<core:table-with-crud <widget:simple-widget)
  ()
  (:default-initargs :table (manager-users-table) :crud (manager-user-crud)
		     :input-element (<core:default-value-input
					:default-value "Enter username (ie root)")
		     :table-title "Users"))

(defmethod/local get-instances ((self manager-users-component))
  (manager-user.list application))

(defmethod/local add-instance ((self manager-users-component) username)
  (manager-user.add application :username username))

(defmethod/local delete-instance ((self manager-users-component) username)
  (aif (manager-user.find application :username username)
       (prog1 t (manager-user.delete application it))))
