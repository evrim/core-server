(in-package :core-server)

;; -------------------------------------------------------------------------
;; Task Mixin aka Taskbar Element
;; -------------------------------------------------------------------------
(defcomponent <core:task (<:ul)
  ((taskbar :host remote :initform nil)
   (title :host remote :initform "")))

(defmacro/js title (self) `(get-title ,self))
(defmethod/remote get-title ((self <core:task))
  (slot-value self 'title))

(defmethod/remote destroy ((self <core:task))
  (remove-task (taskbar self) self)
  (call-next-method self))

(defmethod/remote init ((self <core:task))
  (add-task (taskbar self) self)
  (call-next-method self))

(defmethod/remote toast ((self <core:task) message)
  (toast (taskbar self) message))

;; -------------------------------------------------------------------------
;; Coretal Menu Task
;; -------------------------------------------------------------------------
(defcomponent <core:menu-task (<core:task)
  ())

(defmethod/remote init ((self <core:menu-task))
  (add-class self "core-menu")
  (call-next-method self))

(defmethod/remote add-menu ((self <core:menu-task) item)
  (append self (<:li item)))

(defmethod/remote remove-menu ((self <core:menu-task) item)
  (.remove-child self item))

;; -------------------------------------------------------------------------
;; Taskbar Component
;; -------------------------------------------------------------------------
(defcomponent <core:taskbar (<:div)
  ((tasks :host remote :initform nil)
   (last-update-timestamp :host remote :initform 0)
   (toaster :host remote)
   (toaster-ctor :host remote :initform (<core:toaster-task))
   (menu :host remote :initform (<core:menu-task))
   (taskbar-css :host remote :initform +taskbar-css+)
   (title :host remote :initform "[Core Server]")
   (hidden-p :host remote :initform nil)
   (coretal-icon :host remote)))

(defmethod/remote get-title ((self <core:taskbar))
  (slot-value self 'title))

(defmethod/remote refresh-titles ((self <core:taskbar))
  (mapcar-cc (lambda (task)
	       (destructuring-bind (task title div) task
		 (if (typep (title task) 'string)
		     (setf (slot-value title 'inner-h-t-m-l) (title task))
		     (progn (setf (slot-value title 'inner-h-t-m-l) "")
			    (append title (title task))))))
	     (tasks self)))

(defmethod/remote remove-task ((self <core:taskbar) item)
  (let ((lst (reduce0-cc (lambda (acc atom)
			   (destructuring-bind (task title div) atom
			     (cond
			       ((eq item task)
				(when (slot-value div 'parent-node)
				  (.remove-child (slot-value div 'parent-node) div))
				acc)
			       (t
				(cons atom acc)))))
			 (tasks self))))
    (setf (tasks self) lst)
    lst))

(defmethod/remote task-mouseover ((self <core:taskbar) task)
  (if (slot-value task 'mouseover)
      (make-web-thread (lifte (mouseover task)))))

(defmethod/remote task-mouseout ((self <core:taskbar) task)
  (if (slot-value task 'mouseover)
      (make-web-thread (lifte (mouseout task)))))

(defmethod/remote add-task ((self <core:taskbar) task)
  (let* ((task-title (<:div :class "taskbar-title" (title task)))
	 (content (<:div :class "taskbar-content" task))
	 (div (<:div :class "left"
		     (<:a :onmouseover (lifte (task-mouseover self task))
			  :onmouseout (lifte (task-mouseout self task))
			  task-title content))))
    (append (slot-value self 'first-child) div)
    (setf (tasks self)	  
	  (cons (list task task-title div) (remove-task self task)))))

(defmethod/remote remove-menu ((self <core:taskbar) item)
  (remove-menu (menu self) item)
  item)

(defmethod/remote add-menu ((self <core:taskbar) item)
  (add-menu (menu self) item)
  item)

(defmethod/remote toast ((self <core:taskbar) message)
  (toast (toaster self) message))

(defmethod/remote do-open-console ((self <core:taskbar))
  (do-open (toaster self)))

(defmethod/remote show-taskbar ((self <core:taskbar))
  (setf document.body.style.margin-top "36px")
  (let* ((_style (if window.get-computed-style
		     (window.get-computed-style document.body null)
		     document.body.current-style))
	 (_bg (slot-value _style 'background-image)))
    (if (and (> (slot-value _bg 'length) 0)
	     (not (eq _bg "none")))
	(setf (slot-value document.body.style 'background-position)
	      "0 36px")))
  (show self)
  (hide (coretal-icon self))
  (set-cookie "coretal-hidden-p" false))

(defmethod/remote hide-taskbar ((self <core:taskbar))
  (setf document.body.style.margin-top "0px")
  (let* ((_style (if window.get-computed-style
		     (window.get-computed-style document.body null)
		     document.body.current-style))
	 (_bg (slot-value _style 'background-image)))
    (if (and (> (slot-value _bg 'length) 0) (not (eq _bg "none")))
	(setf (slot-value document.body.style 'background-position)
	      "0 0px")))
  (hide self)
  (show (coretal-icon self))
  (set-cookie "coretal-hidden-p" true))

(defmethod/remote init ((self <core:taskbar))
  (load-css (taskbar-css self))
  (add-class self "core-taskbar")
  (add-class self "core")
  (append self (<:div :class "row1 width-100p")) ;; (<:div :class "clear")
  (append self (<:div :class "row2 clear"));; width-100p
  (setf (menu self)
	(make-component (menu self) :taskbar self :title (title self)))

  (when (null (toaster self))
    (setf (toaster self) (make-component (toaster-ctor self) :taskbar self)))

  (add-menu self
	    (<:a :onclick (lifte (do-open-console self))
		 (_ "Console")
		 (<:span :class "subtitle" (_ "Show log console"))))  

  (setf (coretal-icon self)
	(<:div :class "coretal-icon"
	  "Powered by "
	  (<:img :src "http://www.coretal.net/style/images/icon20px.gif")
	  " "
	  (<:a :class "coretal-icon"
	       :onclick (lifte (show-taskbar self))	       
	       (<:span :class "coretal-name" "Coretal.net"))))

  (cond
    ((eq (get-cookie "coretal-hidden-p") "true")
     (setf (hidden-p self) t))
    ((eq (get-cookie "coretal-hidden-p") "false")
     (setf (hidden-p self) nil)))
  
  (if (hidden-p self)
      (hide-taskbar self)
      (show-taskbar self))
 
  (make-web-thread
   (lambda ()
     (append (slot-value document 'body) (coretal-icon self))
     (prepend (slot-value document 'body) self)))
  
  (call-next-method self))

(defmethod/remote destroy ((self <core:taskbar))
  (remove-class self "core-taskbar")
  (remove-class self "core")
  (setf document.body.style.margin-top "0px")
  (.remove-child document.body (coretal-icon self))
  (.remove-child document.body self)
  (remove-css (taskbar-css self))
  (destroy (menu self))
  (mapcar-cc (lambda (a) (.remove-child self a))
	     (reverse (slot-value self 'child-nodes)))
  (call-next-method self))
