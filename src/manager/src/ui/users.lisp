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
(defcomponent manager-users-component (<widget:simple-widget)
  ((_table-ctor :host remote :initform (sites-table))
   (_crud-ctor :host remote :initform (site-crud))
   (_table :host remote)
   (_crud :host remote)
   (_fqdn-input :host remote :initform (<core:default-value-input))))

(defmethod/remote destroy ((self manager-users-component))
  (delete-slots self '_table)
  (call-next-method self))

(defmethod/local get-users ((self manager-users-component))
  (manager-user.list application))

(defmethod/remote make-table ((self manager-users-component))
  (make-component (_table-ctor self)
		  :instances (mapcar-cc (lambda (a) (make-component a))
					(get-sites self))))

(defmethod/local add-site ((self sites-component) fqdn)
  (site.add application :fqdn fqdn))

(defmethod/local delete-site ((self sites-component) fqdn)
  (aif (site.find application :fqdn fqdn)
       (prog1 t (site.delete application it))))

(defmethod/remote do-add-site ((self sites-component) fqdn)
  (add-instance (_table self) (make-component (add-site self fqdn))))

(defmethod/remote make-form ((self sites-component))
  (let ((fqdn (make-component (_fqdn-input self)
			      :default-value "Enter site name (ie www.core.gen.tr)")))
    (add-class fqdn "width-250px")
    (add-class fqdn "pad5")
    (<:form :onsubmit (lifte (do-add-site self (get-input-value fqdn))
			     (setf (slot-value fqdn 'value) ""))
	    fqdn (<:input :type "submit" :disabled t :value "Add"))))

(defmethod/remote init ((self sites-component))
  (let ((form (make-form self))
	(table (setf (_table self) (make-table self)))
	(crud (setf (_crud self) (<:div))))
    (append self (<:div :class "heading"
			(<:h2 :class "left" "Sites")
			(<:div :class "right" form)))
    (append self table)
    (append self crud)
    (make-web-thread
     (lambda ()
       (let* ((result (call-component table))
	      (new-crud (make-component (_crud-ctor self) :instance result)))
	 (setf (_crud self) (replace-node (_crud self) new-crud))
	 (make-web-thread
	  (lambda ()
	    (destructuring-bind (action &rest args) (call-component new-crud)
	      (cond
		((eq "delete" action)
		 (delete-site self (slot-value result 'fqdn))
		 (setf (_crud self) (replace-node (_crud self) (<:div)))
		 (remove-instance (_table self) result))
		(t nil))))))))))
