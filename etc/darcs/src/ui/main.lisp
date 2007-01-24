(in-package :project-name)

;; Body component
(defcomponent body (ajax-widget)
  ())

(defmethod render ((self body))
  (<:h1 "Merhaba Yapraaaam!"))

(defun header ()
  (<:h1 "header"))

(defun footer ()
  (<:h1 "footer"))

;; Main window component
(defcomponent main-window (ajax-window)
  ())

(defmethod render ((self main-window))
  (<:div :id "header" (header))
  (<:div :id "body" (<:h1 "Merhaba Yapraaaam!"))
  (<:div :id "footer" (footer)))


(defentry-point "^index.*$" (:application *app* :class regexp-dispatcher)
    ()
  (call 'main-window))