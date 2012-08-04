(in-package :core-server)

;; -------------------------------------------------------------------------
;; Persistent Application
;; -------------------------------------------------------------------------
(defclass+ persistent-application (application)
  ((start-stop-state :host local :export nil)))

(defmethod start ((self persistent-application))
  (setf (slot-value self 'start-stop-state) t))

(defmethod stop ((self persistent-application))
  (setf (slot-value self 'start-stop-state) nil))
