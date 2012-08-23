(in-package :core-server)

;; -------------------------------------------------------------------------
;; Coretal Console 
;; -------------------------------------------------------------------------
(defcomponent <core:console (<:div callable-component)
  ((showing-p :host remote :initform nil)
   (console-div :host remote :initform nil)
   (console-css :host remote :initform +console-css+)
   (title :host remote :initform "Console")))

(defmethod/remote get-title ((self <core:console))
  (slot-value self 'title))

(defmethod/remote show-component ((self <core:console))
  (load-css (console-css self))
  (append (slot-value document 'body) self)
  (setf (showing-p self) t))

(defmethod/remote hide-component ((self <core:console))
  (.remove-child (slot-value document 'body) self)
  (remove-css (console-css self))
  (setf (showing-p self) nil))

(defmethod/remote call-component ((self <core:console))
  (show-component self)
  (call-next-method self))

(defmethod/remote answer-component ((self <core:console) arg)
  (hide-component self)
  (destroy self)
  (call-next-method self arg))

(defmethod/remote destroy ((self <core:console))
  (remove-class self "core")
  (remove-class self "core-console")
  (mapcar (lambda (a) (.remove-child self a))
	  (reverse (slot-value self 'child-nodes)))
  (call-next-method self))

(defmethod/remote do-close ((self <core:console))
  (hide-component self)
  (destroy self))

(defmethod/remote init ((self <core:console))
  (add-class self "core")
  (add-class self "core-console")
  (append self
	  (<:div :class "title"
		 (<:div (<:div :class "left" (_ (title self)))
			(<:div :class "right close"
			       (<:a :onclick (lifte (do-close self))
				    :class "close-button"
				    :title "close this log"
				    (_ "close")))
			(<:div :class "clear"))))
  (let ((_div (<:div)))
    (append self (<:div :class "body pad5"
			(setf (console-div self) _div)))))


;; -------------------------------------------------------------------------
;; Toaster Task
;; -------------------------------------------------------------------------
(defcomponent <core:toaster-task (<:div)
  ((max-messages :host remote :initform 10)
   (message-timeout :host remote :initform 2000)
   (toaster-css :host remote :initform +toaster-css+)
   (taskbar :host remote)
   (_log-div :host remote)
   (_log-baloon :host remote)
   (is-open :host remote :initform nil)))

(defmethod/remote do-close ((self <core:toaster-task))
  (setf (is-open self) nil)
  (hide self))

(defmethod/remote do-open ((self <core:toaster-task))
  (setf (is-open self) t)
  (show self))

(defmethod/remote destroy ((self <core:toaster-task))
  (let ((baloon  (_log-baloon self)))
    (.remove-child (slot-value baloon 'parent-node) baloon))
  
  (.remove-child (slot-value document 'body) self)
  (call-next-method self))

(defmethod/remote init ((self <core:toaster-task))
  (load-css (toaster-css self))
  (add-class self "core-toaster")
  (add-class self "core")  
  (append self (<:div :class "title"
		      (<:div (<:div :class "left" (_ "Log Console"))
			     (<:div :class "right close"
				    (<:a :onclick (lifte self.do-close)
					 :class "close-button"
					 :title "close this log"
					 (_ "close")))
			     (<:div :class "clear"))))
  (let ((_div (<:div :class "log" (<:div :class "toast" " "))))
    (setf (_log-div self) _div)    
    (append self _div))

  (let ((_baloon (<:div :class "core core-toaster-baloon")))
    (setf (_log-baloon self) _baloon)
    (append (slot-value document 'body) _baloon))
  
  (hide self)
  (append (slot-value document 'body) self))

(defmethod/remote toast-to-console ((self <core:toaster-task) message)
  (let ((_div (_log-div self)))    
    (when (> (slot-value (slot-value _div 'child-nodes) 'length)
	     (max-messages self))
      (.remove-child _div (slot-value _div 'first-child)))

    (append _div
	    (<:div :class "toast" (date-to-string (new (*date)))
		   " - " message))))

(defmethod/remote toast ((self <core:toaster-task) message)
  (toast-to-console self message)
  (when (null (is-open self))
      (let ((_div (<:div :class "toast" message))
	    (_baloon (_log-baloon self)))
	(append _baloon _div)
	(show _baloon)
	(window.set-timeout
	 (event ()
	   (let ((_children (slot-value _baloon 'child-nodes)))
	     (if (eq 1 (slot-value _children 'length))
		 (progn (.remove-child _baloon _div)
			(hide _baloon))
		 (.remove-child _baloon _div))))
	 (message-timeout self)))))
