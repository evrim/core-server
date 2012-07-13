(in-package :core-server)

(defcomponent <core:tab (<:div)
  ((tabs :host remote)
   (hilight-class :host remote :initform "hilight")
   (tab-title :host remote :initform "Tab Title")
   (nav-alignment :host remote :initform "right")
   (tab-css :host remote :initform +tab-css+)
   (default-tab :host remote :initform nil)
   (_content :host remote)
   (_nav :host remote)
   (_height :host remote :initform 0)
   (_offset :host remote :initform 0)))

(defmethod/remote hilight-tab ((self <core:tab) tab)
  (mapcar (lambda (li)
	    (let ((a (car (slot-value li 'child-nodes))))
	      (cond
		((eq tab (slot-value a 'inner-text))
		 (add-class a (hilight-class self))
		 (add-class li (hilight-class self)))
		(t
		 (remove-class a (hilight-class self))
		 (remove-class li (hilight-class self))))))
	  (slot-value (_nav self) 'child-nodes)))

(defmethod/remote make-tab ((self <core:tab) tab)
  (reduce0 (lambda (acc a)
	     (if (and (listp a) (eq tab (car a)))
		 (car (cdr a))
		 acc))
	   (tabs self)))

(defmethod/remote maintain-height ((self <core:tab))
  (with-slots (_offset) self
    (when (eq _offset 0)
      (setf (_offset self)
	    (- (parse-int (slot-value (get-style self) 'height))
	       (parse-int (slot-value (get-style (_content self)) 'height)))))

    (let* ((content (_content self))
	   (height (parse-int (slot-value (get-style content) 'height)))
	   (max-height (*math.max (_height self) height)))
      (_debug (list "foo" height (_height self) max-height (_offset self)))
      (setf (slot-value (slot-value self 'style) 'min-height)
	    (+ (+ max-height (_offset self)) "px")
	    (_height self) max-height))))

(defmethod/remote show-tab ((self <core:tab) tab)
  (hilight-tab self tab)
  (aif (make-tab self tab)
       (setf (_content self) (replace-node (_content self) it)))
  (maintain-height self))

(defmethod/remote navigation ((self <core:tab))
  (let ((nav (<:ul :class "block"
		   (mapcar-cc
		    (lambda (tab)
		      (<:li (<:a :onclick (lifte (show-tab self tab)) tab)))
		    (mapcar (lambda (a) (if (atom a) a (car a)))
			    (tabs self)))))
	(title (<:div (<:h2 (tab-title self)))))
    (add-class nav (nav-alignment self))
    (setf (_nav self) nav)
    (if (equal (nav-alignment self) "right")
	(progn (add-class title "left")
	       (<:div :class "core-tab-navigation" title nav
		      (<:div :class "clear")))
	(progn (add-class title "right")
	       (<:div :class "core-tab-navigation" nav title
		      (<:div :class "clear"))))))

(defmethod/remote destroy ((self <core:tab))
  (remove-class self "core-tab")
  (remove-class self "core")
  (call-next-method self))

(defmethod/remote init ((self <core:tab))
  (call-next-method self)
  (load-css (tab-css self))
  (add-class self "core-tab")
  (add-class self "core")
  (append self (navigation self))
  (append self (<:div :class "clear"))
  (append self (setf (_content self)
		     (<:p :class "pad10"
			  "Please select an option from above."))))
