(in-package :core-server)

(defmethod ucw::application.url-prefix ((self ucw-web-application))
  (format nil "/~A/" (web-application.fqdn self)))

(defmethod shared-initialize :after ((self ucw-web-application) slot-name 
				     &rest initargs &key &allow-other-keys)
  (declare (ignorable initargs))
  (setf (ucw::application.url-prefix self)
	(ucw::application.url-prefix self)))