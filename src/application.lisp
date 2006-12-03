(in-package :core-server)

;;; saves primitive initargs into initargs slot
(defmethod shared-initialize :after ((self application) slot-names 
				     &rest initargs 
				     &key &allow-other-keys)
  (setf (application.initargs self) 
	(remove-if #'(lambda (arg)
		       (typep arg 'standard-object))
		   initargs)))