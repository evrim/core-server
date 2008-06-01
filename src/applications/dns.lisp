(in-package :core-server)

(defvar +ns1+ "139.179.139.251")
(defvar +ns2+ "212.175.40.11")
(defvar +mx+ "212.175.40.55")

(defclass dns-application (web-application)
  ((ns :accessor dns-application.ns :initform (list +ns1+ +ns2+) :initarg :ns)
   (mx :accessor dns-application.mx :initform (list +mx+) :initarg :mx)
   (alias :accessor dns-application.alias :initform nil :initarg :alias)))

(defmethod deploy-ns ((self dns-application))
  (mapcar (lambda (ns)
	    (when (null (find-ns (application.server self) (web-application.fqdn self)))	      
	      (add-ns (application.server self) (web-application.fqdn self) ns)))
	  (ensure-list (dns-application.ns self))))

(defmethod deploy-mx ((self dns-application))
  (mapcar (lambda (mx)
	    (when (null (find-mx (application.server self) (web-application.fqdn self)))	      
	      (add-mx (application.server self) (web-application.fqdn self) mx)))
	  (ensure-list (dns-application.mx self))))

(defmethod deploy-alias ((self dns-application))
  (mapcar (lambda (alias)
	    (when (null (find-alias (application.server self) (web-application.fqdn self)))	      
	      (add-alias (application.server self) (car alias) (cdr alias))))
	  (ensure-list (dns-application.alias self))))

(defmethod start ((self dns-application))
  (deploy-ns self)
  (deploy-mx self)
  (deploy-alias self))

(defmethod stop ((self dns-application))
  )


