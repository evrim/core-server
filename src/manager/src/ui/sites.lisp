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
(deftable <manager:site/table ()
  ((fqdn :label "FQDN")   
   (owner :label "Owner"
	  :reader (jambda (a) (slot-value (slot-value a 'owner) 'name)))
   (timestamp :label "Timestamp" :remote-type timestamp)))

;; -------------------------------------------------------------------------
;; Site Crud
;; -------------------------------------------------------------------------
(defwebcrud <manager:site/crud ()
  ((fqdn :label "FQDN")
   (api-key :label "API Key")
   (api-password :label "API Password")
   (owner :label "Owner"
	  :reader (jambda (a)
		    (let ((owner (slot-value a 'owner)))
		      (with-slots (name username) owner
			(+ name " (" username ")")))))
   (timestamp :label "Creation Timestamp" :remote-type timestamp))
  (:default-initargs :title "Site" :editable-p nil :deletable-p t))

;; -------------------------------------------------------------------------
;; Sites Component 
;; -------------------------------------------------------------------------
(defcomponent <manager:sites (<core:table-with-crud <widget:simple)
  ()
  (:default-initargs
    :table-title "Sites"
    :table (<manager:site/table)
    :crud (<manager:site/crud)
    :input-element (<core:default-value-input :default-value
		       "Enter site name (ie www.core.gen.tr)")))

(defmethod/local get-instances ((self <manager:sites))
  (site.list application))

(defmethod/local add-instance ((self <manager:sites) fqdn)
  (site.add application :fqdn fqdn))

(defmethod/local delete-instance ((self <manager:sites) fqdn)
  (aif (site.find application :fqdn fqdn)
       (prog1 t (site.delete application it))))
