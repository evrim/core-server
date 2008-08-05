(in-package :core-server)

(defun config-1.0 (layout)  
  (link-systems layout))

(defvar +target-directory+ "/opt/core-server/")
(defparameter *layout* (if (zerop (sb-posix:geteuid))
			   (make-server-layout +target-directory+)
			   (make-layout +target-directory+)))
(install *layout*)
(config-1.0 *layout*)
