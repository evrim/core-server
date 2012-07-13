(in-package :core-server)

(defcomponent <widget:simple-content (<:div <widget:simple)
  ((content :host remote :initform nil :initarg :children)))

(defmethod/remote destroy ((self <widget:simple-content))
  (delete-slots self 'content)
  (call-next-method self))

(defmethod/remote init ((self <widget:simple-content))
  (call-next-method self) 
  (mapcar (lambda (a) (append self a)) (content self)))