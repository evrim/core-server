(in-package :core-server)

;; -------------------------------------------------------------------------
;; Abstract Security Classes
;; -------------------------------------------------------------------------

;; -------------------------------------------------------------------------
;; Abstract Group
;; -------------------------------------------------------------------------
(defclass+ abstract-group ()
  ((name :accessor group.name :initform (error "Provide :name")
	 :initarg :name :host local :index t :export t :print t)
   (users :host local :export nil :accessor group.users :relation groups
	  :type abstract-user*))
  (:ctor %make-abstract-group))

;; -------------------------------------------------------------------------
;; Abstract User
;; -------------------------------------------------------------------------
(defclass+ abstract-user ()
  ((name :accessor user.name :initform nil :initarg :name :host both
	 :index t :print t)
   (groups :accessor user.groups :host local :type abstract-group*
	   :relation users :export nil))
  (:ctor %make-abtract-user))

(defmethod user.has-group ((user abstract-user) (group string))
  (find group (user.groups user) :key #'group.name))

(defmethod user.has-group ((user abstract-user) (group abstract-group))
  (user.has-group user (group.name group)))

;; -------------------------------------------------------------------------
;; Anonymous User
;; -------------------------------------------------------------------------
(defclass+ anonymous-user (abstract-user)
  ()
  (:ctor make-anonymous-user))
