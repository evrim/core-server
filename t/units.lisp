(in-package :tr.gen.core.server.test)

(defclass test-unit (local-unit)
  ())

(defmethod/unit add-two ((self test-unit) no1 no2)
  (+ no1 no2))


(deftest test-unit1
    (let ((unit (make-instance 'test-unit)))
      (add-two unit 4 3))
  7)

(deftest test-unit2
    (let ((unit (make-instance 'test-unit)))
      (start unit)
      (and (status unit)
	   (prog1 (add-two unit 4 3)
	     (stop unit))))
  7)

;; (defparameter *u (make-instance 'test-unit))

;; (defmethod/unit show-me-cps ((self test-unit) arg1)  
;;   (funcall +ret+ arg1)
;;   'default-return-value)

;; (assert (= 7 (add-two *u 4 3)))
;; (assert (null (remove nil
;; 		      (loop for i from 1 upto 1000
;; 			   collect (if (eq i (show-me-cps *u i))
;; 				       nil
;; 				       i)))))

;; (start *u)

;; (assert (= 7 (add-two *u 4 3)))
;; (assert (null (remove nil
;; 		      (loop for i from 1 upto 1000
;; 			   collect (if (eq i (show-me-cps *u i))
;; 				       nil
;; 				       i)))))

(defun clean-local-units ()
  (mapcar #'(lambda (thread)
	      (if (equal "Core-serveR Local Unit" (sb-thread::thread-name thread))
		  (thread-kill thread)))
	  (core-server::all-threads)))


;; (defmethod/unit add-numbers ((self test-unit) &rest numbers)
;;   ;;  (apply #'+ numbers)
;;   numbers)
;;(assert (= 6 (add-numbers *u 1 2 3)))

;; (defmethod/unit lightning :async-no-return ((test-unit test-unit) abc def)
;;   (list test-unit abc def))

;; (defun fast-lightning (test)
;;   (typecase test
;;     (core-string-io-stream nil)
;;     (core-vector-io-stream nil)
;;     (core-fd-io-stream nil)
;;     (core-file-io-stream nil)
;;     (core-stream nil)
;;     (database-server nil)
;;     (t nil)))

;; (defun speed-of-light (&optional (n 1000))
;;   (let ((unit (make-instance 'test-unit)))
;;    (start unit)
;;    (unwind-protect (time
;; 		    (loop for i from 1 upto n
;; 		       do (lightning unit 'gee 33)))
;;      (stop unit)
;;      )
;;   ;;    (time (loop for i from 1 upto n
;; ;;  	       do (fast-lightning unit)))
;;     ))

;; (defclass debug-unit (local-unit)
;;   ())

;; (defmethod/unit debug-me :async-no-return ((self unit) (remote unit) caller
;; 					   (condition condition) (lambda function))  
;;   (if +debug-units-on-error+
;;       (restart-case (swank:swank-debugger-hook condition nil)
;; 	(ignore-error ()
;; 	  :report "Ignore the error and continue processing.")
;; 	(retry ()
;; 	  :report "Retry the funcall"
;; 	  (send-message remote (slot-value remote '%thread) lambda)
;; 	  (funcall #'(lambda ()
;; 		       (apply #'values (funcall (thread-receive)))))))
;;       (thread-send caller #'(lambda () (values)))))

;; (defparameter *y (make-instance 'debug-unit))
;; (start *y)

;; (defclass my-unit (local-unit)
;;   ())

;; (defmethod run ((self local-unit))  
;;   (flet ((l00p ()
;; 	   (let ((message (receive-message self)))
;; 	     (cond
;; 	       ((eq message 'shutdown) (return-from run (values)))
;; 	       ((and (listp message) (functionp (cdr message)))		
;; 		(handler-bind ((error
;; 				#'(lambda (condition)
;; 				    (debug-me (if (s-v '%debug-unit)
;; 						  (s-v '%debug-unit)
;; 						  self)
;; 					      (current-thread)
;; 					      condition (cdr message))
;; ;;; 				    (let ((l #'(lambda ()
;; ;;; 						 (format *standard-output* "gee i'm in lambda~%")
;; ;;; 						 (if +debug-units-on-error+
;; ;;; 						     (restart-case (swank:swank-debugger-hook condition nil)
;; ;;; 						       (ignore-error ()
;; ;;; 							 :report "Ignore the error and continue processing.")
;; ;;; 						       (retry ()
;; ;;; 							 :report "Retry the funcall"
;; ;;; 							 (send-message self (current-thread) (cdr message))
;; ;;; 							 (funcall #'(lambda ()
;; ;;; 								      (apply #'values (funcall (thread-receive)))))))))))
;; ;;; 				      (if (s-v '%debug-unit)
;; ;;; 					  (send-message (s-v '%debug-unit) (current-thread) l)
;; ;;; 					  (thread-send (car message) l)))
;; 				    (return-from l00p nil))))
;; 		  (funcall (cdr message) self)))
;; 	       (t (format t "Got unknown message:~A~%" message))))))
;;     (loop (l00p))))

;; (defun gee ()
;;   (restart-case
;;       (handler-bind ((error #'(lambda (condition)
;; 				(if +debug-units-on-error+
;; 				    (swank:swank-debugger-hook condition nil)
;; 				    (invoke-restart 'my-restart 7)))))
;; 	(error "Foo."))
;;     (my-restart (&optional v) (format t "ab~%") v)))

;; (defmethod/unit valid ((self my-unit))
;;   1)

;; (defmethod/unit bogus ((self my-unit))
;;   (error "I'm gee the error"))

;; (defparameter *x (make-instance 'my-unit :debug-unit *y))
;; (start *x)
;; (defparameter *z (make-instance 'my-unit :debug-unit nil))
;; (start *z)

;; (progn
;;   (stop *y)
;;   (stop *x)
;;   (stop *z)
;;   (start *y)
;;   (start *x)
;;   (start *z))

;; (defparameter *u1 (make-instance 'local-unit))
;; (start *u1)

;; (defmethod/unit valid ((self local-unit))
;;   1)

;; (defmethod/unit bogus ((self local-unit))
;;   (error "there is a horror occurred."))

;; (defmethod/unit bogus2 :async ((self local-unit))
;;   (error "there is a horror occured."))

;; (stop *u1)
;; (defparameter *debug-unit* (make-instance 'local-unit :name "Debug Unit"))
;; (defparameter *u1 (make-instance 'local-unit :debug-unit *debug-unit*))

;; (progn
;;   (stop *debug-unit*)
;;   (stop *u1)
;;   (start *debug-unit*)
;;   (start *u1))
