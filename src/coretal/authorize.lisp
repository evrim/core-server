;; -------------------------------------------------------------------------
;; Authorization
;; -------------------------------------------------------------------------
;; Author: Evrim Ulu <evrim@core.gen.tr>
;; Date: 11/3/2012

(in-package :core-server)
;; -------------------------------------------------------------------------
;; Secure Object
;; -------------------------------------------------------------------------
(defparameter +umask+
  '((owner . 1) (group . 0) (other . 0) (anonymous . 0) (unauthorized . -1)))

(defcomponent secure-object ()
  ((owner :host local :type abstract-user :export nil :reader owner
	  :initarg :owner :initform (error "Provide :owner"))
   (group :host local :type abstract-group :export nil :reader group
	  :initarg :group :initform (error "Provide :group"))
   (levels :host local :export nil
	   :initform '(secure-object/authorized secure-object/unauthorized)
	   ;; (error "Provide :levels")
	   :reader levels)
   (permissions :host local :export nil :reader permissions
		:initform +umask+))
  (:ctor make-secure-object))

(defcrud secure-object)
(defcomponent secure-object/authorized ()
  ((secure-object :host lift :type secure-object)
   (owner :lift t :host local :export nil :type abstract-user)
   (group :lift t :host local :export nil :type abstract-group)
   (user :host local :export nil :initform (error "Provide :user")
	 :type abstract-user)
   (current-application :host local :export nil
			:initform (error "Provide :current-application")
			:type application)))

(defmethod component.application ((self secure-object/authorized))
  (current-application self))

(defcomponent secure-object/unauthorized ()
  ((secure-object :host lift :type secure-object)
   (current-application :host local :export nil
			:initform (error "Provide :current-application")
			:type application)))

(defmethod component.application ((self secure-object/unauthorized))
  (current-application self))

;; -------------------------------------------------------------------------
;; Authorization Helpers
;; -------------------------------------------------------------------------
(defmethod level->constructor ((self secure-object) (level integer))
  (let ((levels (levels self)))
    (cond
      ((>= level (length levels))
       (car (reverse levels)))
      ((< level 0)
       (car levels))
      (t (nth level levels)))))

(defmethod %secure-constructor ((self secure-object) (user anonymous-user)
				&optional levels)
  (let ((levels (or levels (levels self)))
	(level (cdr (assoc 'anonymous (permissions self)))))
    (level->constructor self level)))

(defmethod %secure-constructor ((self secure-object) (user simple-user)
				&optional levels)
  (let* ((levels (or levels (levels self)))
	 (level (cond
		  ;; Grant Maximum Permission
		  ((or (has-group user "editor") (has-group user "admin"))
		   (- (length (permissions self)) 1))
		  ;; Owner Match
		  ((eq user (owner self))
		   (cdr (assoc 'owner (permissions self))))
		  ;; Group Match
		  ((find (group.name (group self)) (user.groups user)
			 :key #'group.name :test #'equal)
		   (cdr (assoc 'group (permissions self))))
		  ;; Default Permission
		  (t
		   (cdr (assoc 'other (permissions self)))))))
    (level->constructor self level)))

;; -------------------------------------------------------------------------
;; Authorization Protocol
;; -------------------------------------------------------------------------
(defmethod copy-lifted-slot ((self secure-object/authorized)
			     (slot standard-slot-definition) value)
  (if (slot-definition-authorize slot)
      (authorize (current-application self) (user self) value)
      value))

(defmethod copy-lifted-slot ((self secure-object/unauthorized)
			     (slot standard-slot-definition) value)
  (if (slot-definition-authorize slot)
      (authorize (current-application self) (make-anonymous-user) value)
      value))

(defmethod authorize ((application application) (user abstract-user)
		      (object t))
  object)

(defmethod authorize ((application application) (user abstract-user)
		      (object list))
  (mapcar (curry #'authorize application user) object))

(defmethod authorize ((application application)
		      (user abstract-user) (object secure-object))
  (apply (%secure-constructor object user)
	 (list :secure-object object :user user :current-application application)))
