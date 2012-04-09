(in-package :manager)

;; -------------------------------------------------------------------------
;; Sites Table
;; -------------------------------------------------------------------------
(deftable sites-table ()
  ((fqdn :label "FQDN")   
   (owner :label "Owner"
	  :reader (with-call/cc
		    (lambda (a) 
		      (slot-value (slot-value a 'owner) 'name))))
   (timestamp :label "Timestamp"
	      :reader (with-call/cc
			(lambda (a)
			  (take 10
				(date-to-string
				 (lisp-date-to-javascript
				  (slot-value a 'timestamp)))))))))

;; -------------------------------------------------------------------------
;; Site Crud
;; -------------------------------------------------------------------------
(defwebcrud site-crud ()
  ((fqdn :label "FQDN")
   (api-key :label "API Key")
   (api-password :label "API Password")
   (owner :label "Owner"
	  :reader (with-call/cc
		    (lambda (a)
		      (let ((owner (slot-value a 'owner)))
			(with-slots (name username) owner
			  (+ name " (" username ")"))))))
   (timestamp :label "Creation Timestamp"
	      :reader (with-call/cc
			(lambda (a)
			  (take 10
				(date-to-string
				 (lisp-date-to-javascript
				  (slot-value a 'timestamp))))))))
  (:default-initargs :title "Site" :editable-p nil :deletable-p t))

;; -------------------------------------------------------------------------
;; Sites Component 
;; -------------------------------------------------------------------------
(defcomponent sites-component (<core:table-with-crud <widget:simple-widget)
  ()
  (:default-initargs :table-title "Sites" :table (sites-table) :crud (site-crud)
    :input-element (<core:default-value-input
		       :default-value "Enter site name (ie www.core.gen.tr)")))

(defmethod/local get-instances ((self sites-component))
  (site.list application))

(defmethod/local add-instance ((self sites-component) fqdn)
  (site.add application :fqdn fqdn))

(defmethod/local delete-instance ((self sites-component) fqdn)
  (aif (site.find application :fqdn fqdn)
       (prog1 t (site.delete application it))))
