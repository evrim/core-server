(in-package :core-server)

;; -------------------------------------------------------------------------
;; Sidebar
;; -------------------------------------------------------------------------
(defcomponent <core:sidebar (<:div callable-component cached-component)
  ((sidebar-css :host remote :initform +sidebar.css+)
   (dirty-p :host remote :initform nil)))

(defmethod/remote destroy ((self <core:sidebar))
  (remove-class self "core")
  (remove-class self "core-sidebar")
  (call-next-method self))

(defmethod/remote do-close ((self <core:sidebar))
  (if (dirty-p self)
      (if (confirm (_"Do you want to discard changes?"))
	  (hide-component self))
      (hide-component self)))

(defmethod/remote init ((self <core:sidebar))
  (add-class self "core-sidebar")
  (add-class self "core")
  (prepend self (<:div :class "right"
		       (<:a :onclick (lifte (do-close self))
			    :class "sidebar-close-button"
			    :title (_"close this sidebar")  " ")))
  (call-next-method self))

(defmethod/remote show-component ((self <core:sidebar))
  (load-css (sidebar-css self))
  (append (slot-value document 'body) self))

(defmethod/remote hide-component ((self <core:sidebar))
  (.remove-child (slot-value document 'body) self)
  (remove-css (sidebar-css self)))

(defmethod/remote call-component ((self <core:sidebar))
  (show-component self)
  (call-next-method self))

(defmethod/remote answer-component ((self <core:sidebar) arg)
  (hide-component self)
  (destroy self)
  (call-next-method self arg))
