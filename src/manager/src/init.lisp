(in-package :manager)
;; +-------------------------------------------------------------------------
;; | Manager Instance
;; +-------------------------------------------------------------------------

(defun register-me (&optional (server *server*))
  (if (null (status *app*)) (start *app*))
  (register server *app*))

(defun unregister-me (&optional (server *server*))
  (unregister server *app*))


(defvar *app* (make-instance 'manager-application))
