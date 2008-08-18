(in-package :core-server)

(defvar +target-directory+ "/opt/core-server/")
(defparameter *layout* (if (zerop (sb-posix:geteuid))
			   (make-server-layout +target-directory+)
			   (make-layout +target-directory+)))
(install *layout*)
