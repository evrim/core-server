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
	      (if (equal "Local Unit" (sb-thread::thread-name thread))
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

