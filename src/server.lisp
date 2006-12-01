(in-package :tr.gen.core.server)

(defmacro with-server-mutex (server &body body)
  `(sb-thread:with-recursive-lock ((server.mutex ,@server))
     ,@body))

(defmethod start :around ((self server))
  (with-server-mutex (self)
    (let ((failed))
      (unwind-protect
	(restart-case
	    (let ((swank::*sldb-quit-restart* 'give-up))
	      (setf failed t)
	      (call-next-method)
	      (setf failed nil))
	  (give-up ()
	    :report "Give up starting server."
	    (format t "Giving up.~%"))
	  (try-again ()
	    :report "Try again."
	    (start self)))
	(when failed
	  (format t "stopping server.~%")
	  (stop self))))))

(defmethod stop :around ((self server))
  (with-server-mutex (self)
    (call-next-method)))

(defmethod register :around ((self server) (app application))
  (with-server-mutex (self)
    (let ((failed))
      (unwind-protect
	(restart-case
	    (let ((swank::*sldb-quit-restart* 'give-up))
	      (setf failed t)
	      (call-next-method)
	      (setf failed nil)
	      (setf (application.server app) self))
	  (give-up ()
	    :report "Give up registering app."
	    (format t "Giving up.~%"))
	  (try-again ()
	    :report "Try again."
	    (start self)))
	(when failed
	  (format t "Unregistering application.")
	  (unregister self app))))))

(defmethod unregister :around ((self server) (app application))
  (call-next-method)
  (setf (application.server app) nil))