(in-package :tr.gen.core.server)

(defmethod run-ns-sysv-script ((self name-server) params)
  (unwind-protect
       (sb-impl::process-exit-code
	(sb-ext:run-program +sudo+
			    (cons (namestring (name-server.ns-script-pathname self)) params)))))

(defmethod start ((self name-server))
  (start (name-server.ns-db self))
  (eq 0 (run-ns-sysv-script self '("start"))))

(defmethod stop ((self name-server))
  (stop (name-server.ns-db self))
  (eq 0 (run-ns-sysv-script self '("stop"))))

(defmethod status ((self name-server))
  (and (eq 0 (run-ns-sysv-script self '("status")))
       (status (name-server.ns-db self))))

(defun find-host (fqdn domain)
  (aif (find-if (curry #'equal fqdn)
		(remove-if-not #'(lambda (o)
				   (or (typep o 'ns-host)
				       (typep o 'ns-alias))) domain)
		:key #'ns-record.source)
       (cond
	 ((typep it 'ns-host) it)
	 ((typep it 'ns-alias)
	  (find-host (ns-record.target it) domain)))))

;; Tx
(defun tx-add-record (system class fqdn ip)
  (let ((model (model system)))
    (symbol-macrolet ((domain (gethash (domain-part fqdn) (ns-model.domains model))))
      (flet ((replace-record (obj)
	       (setf domain (cons obj (remove-if #'(lambda (o)
						     (and (eq (type-of o) (type-of obj))
							  (equal (ns-record.source o) fqdn)))
						 domain)))))
	(cond
	  ((eql class 'ns-ns)
	   (if (not (find-host fqdn domain))
	       (replace-record (make-instance 'ns-host :source fqdn :target ip)))
	   (replace-record (make-instance 'ns-ns :source fqdn :target ip)))
	  ((eql class 'ns-host)
	   (replace-record (make-instance 'ns-host :source fqdn :target ip)))
	  ((eql class 'ns-alias)
	   (if (equal fqdn ip)
	       (error "kendi kendine ilyas ekleyemen")
	       (if (find-host ip domain)
		   (replace-record (make-instance 'ns-alias :source fqdn :target ip))
		   (error "nabion olmm? host yok sen ilyas eklion"))))
	  ((eql class 'ns-mx)	 
	   (if (null ip)
	       (let ((host (find-host fqdn domain)))
		 (if host
		     (replace-record (make-instance 'ns-mx :source fqdn :target (ns-record.target host)))
		     (error "yok oyle bi host")))
	       (replace-record (make-instance 'ns-mx :source fqdn :target ip)))))))))
    
;; Interface Implementation
(defmethod add-mx ((server name-server) fqdn &optional ip)
  (execute (name-server.ns-db server) (make-transaction 'tx-add-record 'ns-mx fqdn ip)))

(defmethod add-host ((server name-server) domain ip)
  (execute (name-server.ns-db server) (make-transaction 'tx-add-record 'ns-host domain ip)))

(defmethod add-alias ((server name-server) source target)
  (execute (name-server.ns-db server) (make-transaction 'tx-add-record 'ns-alias source target)))

(defmethod add-ns ((server name-server) domain ip)
  (execute (name-server.ns-db server) (make-transaction 'tx-add-record 'ns-ns domain ip)))

(defmethod find-domain-records ((server name-server) domain)
  (gethash domain (ns-model.domains (model (name-server.ns-db server)))))

;; Serializers
(defmethod serialize-cdb ((self ns-mx) stream)
  (format stream "@~A:~A:~A:86400~%" (domain-part (ns-record.source self)) (ns-record.target self) (ns-record.source self)))

(defmethod serialize-cdb ((self ns-host) stream)
  (format stream "=~A:~A:86400~%" (ns-record.source self) (ns-record.target self)))

(defmethod serialize-cdb ((self ns-alias) stream)
  (format stream "C~A.:~A.~%" (ns-record.source self) (ns-record.target self)))

(defmethod serialize-cdb ((self ns-ns) stream)
  (format stream ".~A:~A:~A:86400~%" (domain-part (ns-record.source self)) (ns-record.target self) (ns-record.source self)))

(defmethod name-server.ns-data-pathname ((self name-server))
  (merge-pathnames (make-pathname :name "data")
		   (name-server.ns-root-pathname self)))

(defmethod serialize ((self name-server))
  (with-open-file (s (name-server.ns-data-pathname self) :direction :output :if-does-not-exist :create :if-exists :supersede)
    (maphash #'(lambda (key val)
		 (declare (ignorable key))
		 (mapcar #'(lambda (record)
			     (serialize-cdb record s))
			 (ensure-list val)))
	    (ns-model.domains (model (name-server.ns-db self)))))
  t)

(defmethod make ((self name-server))
  (with-current-directory (name-server.ns-root-pathname self)
    (eq 0
	(sb-impl::process-exit-code
	 (sb-ext:run-program +sudo+ (cons (namestring (name-server.ns-compiler-pathname self))
					  (list (namestring (name-server.ns-data-pathname self)))))))))

(defmethod name-server.refresh ((self name-server))
  (with-server-mutex (self)
    (serialize self)
    (make self)))

(defmacro with-nameserver-refresh ((var name-server) &body body)
  `(let ((,var ,name-server))
     ,@body
     (name-server.refresh ,var)))
