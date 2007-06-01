(in-package :tr.gen.core.server.test)

(defparameter *msg* nil)

(deftest thread-spawn
    (let ((master (thread-spawn #'(lambda ()
				    (let* ((message '(1 2.0 "abc"))
					   (self (core-server::current-thread))
					   (peer (thread-spawn #'(lambda () 
								   (thread-send self (thread-receive))))))
				      (thread-send peer message)
				      (setf *msg* (thread-receive)))))))
      (sleep 1)
      (and (listp *msg*)
	   (eq 3 (length *msg*))))
  t)