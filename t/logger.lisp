(in-package :core-server)

(defparameter *logger* (make-instance 'logger-server))

(defclass my-worker (local-unit)
  ((id :accessor id :initarg :id)))

(defmethod/unit logmeup :async-no-return ((self my-worker))
  (log-me *logger* (format nil "I'm here as number ~D" (id self))))

(defparameter *loggerz* (loop
			   for i from 1 to 40
			   collect (make-instance 'my-worker :id i)))

(defun setup-loggerz ()
  (start *logger*)
  (mapcar #'start *loggerz*)
  (mapcar #'logmeup *loggerz*)
  (mapcar #'stop *loggerz*)
  (sleep 5)
  (stop *logger*))