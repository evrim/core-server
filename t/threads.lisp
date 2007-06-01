(in-package :tr.gen.core.server)

(defparameter *t1
  (thread-spawn #'(lambda ()
		    (dotimes (i 10)
		       (format *standard-output*
			       "Got message:~A~%" (thread-receive))
		       (sleep 1))
		    (format t "exiting t1~%"))
		:name "T1"))

(defparameter *t2
  (thread-spawn #'(lambda ()
		    (dotimes (i 10)
		      (thread-send *t1 (list 'look 'ma 'im 'a 'thread 'num i)))
		    (sleep 3)
		    (thread-kill *t1)
		    (describe (find-thread-mailbox *t1)) ;;must be empty
		    (format t "exiting t2~%"))
		:name "T2"))