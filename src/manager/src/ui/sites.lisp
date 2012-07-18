(in-package :manager)

(defcomponent site/authorized (secure-object/authorized
			       site remote-reference)
  ((secure-object :host lift :type site)
   (fqdn :host remote :lift t)
   (owner :host remote :lift t)
   (creation-timestamp :host remote :lift t)))

(defcrud/lift site/authorized site)

;; -------------------------------------------------------------------------
;; Sites Table
;; -------------------------------------------------------------------------
(deftable sites-table ()
  ((fqdn :label "FQDN")   
   (owner :label "Owner"
	  :reader (with-call/cc
		    (lambda (a) (slot-value (slot-value a 'owner) 'name))))
   (timestamp :label "Timestamp" :remote-type timestamp)))

;; -------------------------------------------------------------------------
;; Site Crud
;; -------------------------------------------------------------------------
(defwebcrud site-crud ()
  ((fqdn :label "FQDN")
   (api-key :label "API Key")
   (api-password :label "API Password")
   (owner :label "Owner"
	  :reader (walk-form
		    `(lambda (a)
		       (let ((owner (slot-value a 'owner)))
			 (with-slots (name username) owner
			   (+ name " (" username ")"))))))
   (timestamp :label "Creation Timestamp" :remote-type timestamp))
  (:default-initargs :title "Site" :editable-p nil :deletable-p t))

;; -------------------------------------------------------------------------
;; Sites Component 
;; -------------------------------------------------------------------------
(defcomponent sites-component (<core:table-with-crud <widget:simple)
  ()
  (:default-initargs
    :table-title "Sites" :table (sites-table) :crud (site-crud)
    :input-element (<core:default-value-input
		       :default-value "Enter site name (ie www.core.gen.tr)")))

(defmethod/local get-instances ((self sites-component))
  (site.list application))

(defmethod/local add-instance ((self sites-component) fqdn)
  (site.add application :fqdn fqdn))

(defmethod/local delete-instance ((self sites-component) fqdn)
  (aif (site.find application :fqdn fqdn)
       (prog1 t (site.delete application it))))
