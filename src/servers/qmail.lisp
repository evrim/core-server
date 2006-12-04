(in-package :core-server)

(defclass qmail-server (email-server)
  ((qmail-script-pathname :accessor qmail-server.qmail-script-pathname :initarg qmail-script-pathname
			  :initform (make-pathname :directory '(:absolute "etc" "init.d") :name "svscan"))
   (vpopmail-pathname :accessor qmail-server.vpopmail-pathname :initarg vpopmail-pathname
		      :initform (make-pathname :directory '(:absolute "var" "vpopmail"))))
  (:default-initargs :name "Qmail mail Server"))

(defmethod run-qmail-sysv-script ((self qmail-server) params)
  (unwind-protect
       (sb-impl::process-exit-code
	(sb-ext:run-program +sudo+
			    (cons (namestring (qmail-server.qmail-script-pathname self)) params)))))

(defmethod qmail-server.vpopmail-bin-pathname ((self qmail-server))
  (merge-pathnames 
   (make-pathname :directory '(:relative "bin"))
   (qmail-server.vpopmail-pathname self)))

(defmethod run-vpopmail-script ((self qmail-server) cmd params)
  (unwind-protect       
       (sb-ext:run-program +sudo+
			   (cons (namestring 
				  (merge-pathnames (make-pathname :name cmd)
						   (qmail-server.vpopmail-bin-pathname self)))
				 params))))

(defmethod start ((self qmail-server))
  (eq 0 (run-qmail-sysv-script self '("start"))))

(defmethod stop ((self qmail-server))
  (eq 0 (run-qmail-sysv-script self '("stop"))))

(defmethod status ((self qmail-server))
  (eq 0 (run-qmail-sysv-script self '("status"))))

(defmethod add-domain ((self qmail-server) domain)
  (run-vpopmail-script self "vadddomain" (list "-r10" domain)))

(defmethod remove-domain ((self qmail-server))
  )