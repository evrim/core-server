(in-package :core-server)

(defcomponent <core:tab (<:div)
  ((contents-div :host remote)))

(defmethod/remote get-titles ((component <core:tab))
  (list (list "default" "Tab 1")
	(list "tab2" "Tab 2")))

(defmethod/remote get-tab ((component <core:tab) tab)
  (cond
    ((eq tab "tab2")
     (<:div :class "tab"
	    (<:p "Second tab contents")))
    (t
     (<:div :class "tab"
	    (<:p "Default tab contents")))))

(defmethod/remote show-tab ((component <core:tab) tab)
  (setf component.contents-div.inner-h-t-m-l "")
  (.append-child component.contents-div (get-tab component tab)))

(defmethod/remote get-menu ((component <core:tab))  
  (<:ul :class "tabs"
	(mapcar (lambda (title)
		  (<:li (<:a :onclick (lambda (e)
					(show-tab component (car title)))
			     (car (cdr title)))))
		(get-titles component))))

(defmethod/remote init-tabs ((component <core:tab))
  (.append-child component (get-menu component))
  (let ((div (<:div :class "content")))
    (.append-child component div)
    (set-contents-div component div))
  (show-tab component "default")
  (add-class component "tab"))

(defmethod/remote init ((component <core:tab))
  (init-tabs component))