(in-package :core-server)


;; -------------------------------------------------------------------------
;; Persistent Application
;; -------------------------------------------------------------------------
(defclass+ persistent-application ()
  ((initialization-arguments))
  (:ctor make-persistent-application))

(defmethod shared-initialize :after ((self persistent-application) slots
				     &rest initargs)
  ;; Save Initialization Arguments
  (prog1 self (setf (s-v 'initialization-arguments) initargs)))
