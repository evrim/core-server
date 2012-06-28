(in-package :core-server)

;; -------------------------------------------------------------------------
;; Authentication
;; -------------------------------------------------------------------------
;; Author: Evrim Ulu <evrim@core.gen.tr>
;; Date: 11/03/2012

;; -------------------------------------------------------------------------
;; Abstract Group
;; -------------------------------------------------------------------------
(defclass+ abstract-group ()
  ()
  (:ctor %make-abstract-group))

;; -------------------------------------------------------------------------
;; Simple Group
;; -------------------------------------------------------------------------
(defclass+ simple-group ()
  ((name :accessor group.name :initform (error "Provide :name")
	 :initarg :name :host local :index t :export t :print t)
   (users :host local :export nil :accessor group.users :relation groups
	  :type simple-user*))
  (:ctor make-simple-group))

;; -------------------------------------------------------------------------
;; Abstract User
;; -------------------------------------------------------------------------
(defclass+ abstract-user ()
  ()
  (:ctor %make-abtract-user))

;; -------------------------------------------------------------------------
;; Simple User
;; -------------------------------------------------------------------------
(defclass+ simple-user (abstract-user)
  ((name :accessor user.name :initform nil
	 :initarg :name :host both :index t :print t)
   (groups :accessor user.groups :host local :type simple-group*
	   :relation users :export nil))
  (:ctor make-simple-user))

(defmethod has-group ((user simple-user) (group string))
  (find group (user.groups user) :key #'group.name))

(defmethod has-group ((user simple-user) (group abstract-group))
  (has-group user (group.name group)))

(defcrud simple-group)
(defcrud simple-user)

;; -------------------------------------------------------------------------
;; Anonymous User
;; -------------------------------------------------------------------------
(defclass+ anonymous-user (abstract-user)
  ()
  (:ctor make-anonymous-user))


(deftransaction init-authentication ((self database))
  (let* ((admins (simple-group.add self :name "admin"))
	 (editors (simple-group.add self :name "editor"))
	 (admin (simple-user.add self :name "admin"
				 :groups (list admins editors))))
    (list admin admins editors)))