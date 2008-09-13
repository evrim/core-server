(in-package :core-server)

;; ----------------------------------------------------------------------------
;; User Add
;; ----------------------------------------------------------------------------
(defcommand useradd (shell)
  ((username :host local :initform (error "Username must be provided."))
   (group :host local :initform nil)
   (extra-groups :host local :initform '())
   (home-directory :host local :initform nil)
   (user-group :host local :initform nil)
   (create-home :host local :initform nil)
   (comment :host local :initform nil))
  (:default-initargs :cmd #P"/usr/sbin/useradd"))

(defmethod render-arguments ((self useradd))
  (cons (s-v 'username)
	(reduce
	 #'(lambda (acc item)
	     (if (s-v (car item))
		 (cons (cadr item) acc)
		 acc))	  
	 '((user-group "-n") (create-home "-m"))
	 :initial-value (reduce
			 #'(lambda (acc item)
			     (if (s-v (car item))
				 (cons (cadr item)
				       (if (listp (s-v (car item)))
					   (cons (reduce
						  #'(lambda (acc i)
						      (format nil "~A,~A" acc i))
						  (s-v (car item))) acc)
					   (cons (s-v (car item)) acc)))
				 acc))
			 '((group "-g") (extra-groups "-G") (home-directory "-d")
			   (comment "-c")) :initial-value nil))))

;; ----------------------------------------------------------------------------
;; Group Add
;; ----------------------------------------------------------------------------
(defcommand groupadd (shell)
  ((groupname :host local :initform (error "Group name must be provided.")))
  (:default-initargs :cmd #P"/usr/bin/groupadd" :errorp nil))

(defmethod render-arguments ((self groupadd))
  (list (s-v 'groupname)))
