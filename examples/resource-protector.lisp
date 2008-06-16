(in-package :core-server)

;; Protection for concurrent access to a single resource
(defclass resource-protector (local-unit)
  ((resource :accessor resource-protector.resource :initarg :resource)))

(defmethod/unit protected-access :sync ((self resource-protector) fun)
  (prog1
      (setf (resource-protector.resource self)
	    (funcall fun (resource-protector.resource self)))
    (describe (resource-protector.resource self))))

(defmacro defprotected (resource)
  (let ((sym (gensym)))
    `(let ((,sym (make-instance 'resource-protector :resource ,resource)))
       (start ,sym)
       (cons
	(lambda (fun) 
	  (protected-access ,sym fun))
	(lambda ()
	  (stop ,sym)
	  (setf ,sym nil))))))

(defun test-protector ()
  (let* ((protector (defprotected 0))
	 (accessor (car protector))
	 (destructor (cdr protector))) 
    (labels ((fwd (i)
	       (funcall accessor #'(lambda (x) (+ i x))))
	     (rwd (i)
	       (funcall accessor #'(lambda (x) (- x i))))
	     (l00p (i)
	       (cond
		 ((< i 1) (funcall accessor #'identity))
		 (t
		  (progn
		    (thread-spawn (lambda () (fwd 2)))
		    (thread-spawn (lambda () (rwd 1)))
		    (l00p (- i 1)))))))
      (prog1 (l00p 10)
	(funcall destructor)))))

;; 2 is a (INTEGER 0 1152921504606846975).
;; 1 is a BIT.
;; 3 is a (INTEGER 0 1152921504606846975).
;; 2 is a (INTEGER 0 1152921504606846975).
;; 4 is a (INTEGER 0 1152921504606846975).
;; 3 is a (INTEGER 0 1152921504606846975).
;; 5 is a (INTEGER 0 1152921504606846975).
;; 4 is a (INTEGER 0 1152921504606846975).
;; 6 is a (INTEGER 0 1152921504606846975).
;; 5 is a (INTEGER 0 1152921504606846975).
;; 7 is a (INTEGER 0 1152921504606846975).
;; 6 is a (INTEGER 0 1152921504606846975).
;; 8 is a (INTEGER 0 1152921504606846975).
;; 7 is a (INTEGER 0 1152921504606846975).
;; 9 is a (INTEGER 0 1152921504606846975).
;; 8 is a (INTEGER 0 1152921504606846975).
;; 10 is a (INTEGER 0 1152921504606846975).
;; 9 is a (INTEGER 0 1152921504606846975).
;; 11 is a (INTEGER 0 1152921504606846975).
;; 10 is a (INTEGER 0 1152921504606846975).
;; 10 is a (INTEGER 0 1152921504606846975).

