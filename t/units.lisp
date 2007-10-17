(in-package :tr.gen.core.server.test)

(defclass test-unit (local-unit)
  ())

(defmethod/unit lightning :async-no-return ((test-unit test-unit) abc def)
  (list test-unit abc def))

(defun fast-lightning (test)
  (typecase test
    (core-string-io-stream nil)
    (core-vector-io-stream nil)
    (core-fd-io-stream nil)
    (core-file-io-stream nil)
    (core-stream nil)
    (database-server nil)
    (t nil)))

(defun speed-of-light (&optional (n 1000))
  (let ((unit (make-instance 'test-unit)))
  ;;  (start unit)
;;    (unwind-protect (time
;; 		    (loop for i from 1 upto n
;; 		       do (lightning unit 'gee 33)))
;;      (stop unit)
;;      )
     (time (loop for i from 1 upto n
 	       do (fast-lightning unit)))
    ))


(defun cleanup ()
  (mapcar #'(lambda (thread)
	      (if (equal "Local Unit" (sb-thread::thread-name thread))
		  (thread-kill thread)))
	  (core-server::all-threads)))