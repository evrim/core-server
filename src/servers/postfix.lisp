(in-package :tr.gen.core.server)

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

(defmethod add-email ((self postfix-server) (email string) (maildir string))
  (unwind-protect
       (sb-impl::process-exit-code 
	(with-input-from-string (in (concatenate 'string email " " maildir))
	  (sb-ext:run-program +sudo+
			      (list (namestring +postmap+) "-i" "-r"
				    (format nil "hash:~A" (postfix-server.virtual-mailbox-maps self)))
			      :input in
			      :output *standard-output*)))))
(defmethod del-email ((self postfix-server) (email string) &optional delete-maildir)
  (unwind-protect
       (sb-impl::process-exit-code 
	(sb-ext:run-program +sudo+
			    (list (namestring +postmap+)
				  "-d" email
				  (format nil "hash:~A" (postfix-server.virtual-mailbox-maps self)))
			    :output *standard-output*)))
  (when delete-maildir
    (error "Removing maildirs not implemented yet")))