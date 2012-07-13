(in-package :manager)

;; -------------------------------------------------------------------------
;; Info Component
;; -------------------------------------------------------------------------
(defcomponent <manager:server-info (<widget:simple)
  ((_hostname :host remote :initform (hostname))
   (_memory :host remote
	    :initform (format nil "~10:D" (sb-kernel::dynamic-usage)))
   (_date :host remote :initform (get-universal-time))))

(defmethod/remote init ((self <manager:server-info))
  (append self (<:p "Date:" (date-to-string
			     (lisp-date-to-javascript (_date self)))))
  (append self (<:p "Hostname:" (_hostname self)))
  (append self (<:p "Memory: " (_memory self) " bytes")))
