(in-package :core-server)

#+nil
(defmethod ucw::application.url-prefix ((self ucw-web-application))
  (format nil "/~A/" (web-application.fqdn self)))

(defmethod shared-initialize :after ((self ucw-web-application) slot-name 
				     &rest initargs &key &allow-other-keys)
  (declare (ignorable initargs))
  #+nil
  (setf (ucw::application.url-prefix self)
	(ucw::application.url-prefix self)))

#+nil
(defun current-application (&optional (context *context*))
  (ucw::context.application context))
#+nil
(defun current-window (&optional (context *context*))
  #+nil(ucw::context.window-component context))
#+nil
(defun current-session (&optional (context *context*))
  #+nil(ucw::context.session context))
