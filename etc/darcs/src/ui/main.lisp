(in-package :project-name)

;; Header component
(defcomponent header (ajax-widget)
  ())

(defmethod render ((self header))
  (<:h1 "Header"))

;; Body component
(defcomponent body (ajax-widget)
  ())

(defmethod render ((self body))
  (<:h1 "Merhaba Yapraaaam!"))

;; Footer component
(defcomponent footer (ajax-widget)
  ())

(defmethod render ((self footer))
  (<:h1 "Footer"))

;; Main window component
(defcomponent main-window (ajax-window)
  ((header :accessor header :initarg :header :component header)
   (body :accessor body :initarg :body :component body)
   (footer :accessor footer :initarg :footer :component footer)))

(defmethod render ((self main-window))
  (<:div :id "header"
	 (render (header self)))
  (<:div :id "body"
	 (render (body self)))
  (<:div :id "footer"
	 (render (footer self))))

(defentry-point "^index.*$" (:application *app* :class regexp-dispatcher)
    ()
  (call 'main-window))