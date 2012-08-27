(in-package :core-server)

;; -------------------------------------------------------------------------
;; Authentication
;; -------------------------------------------------------------------------
;; Author: Evrim Ulu <evrim@core.gen.tr>
;; Date: 11/03/2012

;; -------------------------------------------------------------------------
;; Simple Group
;; -------------------------------------------------------------------------
(defclass+ simple-group (abstract-group)
  ()
  (:ctor make-simple-group))

;; -------------------------------------------------------------------------
;; Simple User
;; -------------------------------------------------------------------------
(defclass+ simple-user (abstract-user)
  ()
  (:ctor make-simple-user))

(defcrud simple-group)
(defcrud simple-user)

(deftransaction init-authentication ((self database))
  (let* ((admins (simple-group.add self :name "admin"))
	 (editors (simple-group.add self :name "editor"))
	 (admin (simple-user.add self :name "admin"
				 :groups (list admins editors))))
    (list admin admins editors)))

(defvar +system-group+ (make-simple-group :name "admin"))
(defvar +system+ (make-simple-user :name "admin" :group +system-group+))