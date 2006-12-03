(in-package :core-server)

(defmethod run-postfix-sysv-script ((self postfix-server) params)
  (unwind-protect
       (sb-impl::process-exit-code
	(sb-ext:run-program +sudo+
			    (cons (namestring (postfix-server.postfix-script-pathname self)) params)))))

(defmethod start ((self postfix-server))
  (eq 0 (run-postfix-sysv-script self '("start"))))

(defmethod stop ((self postfix-server))
  (eq 0 (run-postfix-sysv-script self '("stop"))))

(defmethod status ((self postfix-server))
  (eq 0 (run-postfix-sysv-script self '("status"))))