(in-package :core-server)

(defcomponent <widget:simple-content-widget (<:div simple-widget)
  ((content :host remote :initform nil)))

(defmethod/remote destroy ((self <widget:simple-content-widget))
  (delete-slots self 'content)
  (call-next-method self))

(defmethod/remote init ((self <widget:simple-content-widget))
  (call-next-method self)
  (_debug (list "foo" (content self) (reverse (slot-value self 'child-nodes))))
  (mapcar (lambda (a) (append self a)) (content self)))