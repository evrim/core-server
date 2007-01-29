(in-package :core-server)

;;; FIXmE: pathname bile serialize edemio bu prevalence!
(defmethod sieve-initargs ((self application) initargs)  
  (let ((current-keyword))
    (reduce #'(lambda (acc arg)
		(cond 
		  ((eq 0 (mod (position arg initargs) 2))
		   (setf current-keyword arg)
		   acc)
		  ((eq 1 (mod (position arg initargs) 2))		     
		   (if (or (functionp arg) (typep arg 'standard-object))
		       acc
		       (append acc (list current-keyword arg))))))
	    initargs
	    :initial-value '())))

;;; saves primitive initargs into initargs slot
(defmethod shared-initialize :after ((self application) slot-names 
				     &rest initargs 
				     &key &allow-other-keys)
  (setf (application.initargs self)
	(sieve-initargs self initargs)))