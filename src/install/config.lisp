(in-package #:tr.gen.core.install)

(defun config-1.0 (layout)
  (with-current-directory (merge-pathnames #P"rfc2388/"
			   (merge-pathnames (layout.lib layout)
					    (slot-value layout 'root)))
    (darcs :op "unpull" :errorp nil
	   :args '("--from-match" "date \"Sat Jun 16 15:25:52 EEST 2007\"" "-a")))
  (with-current-directory (merge-pathnames #P"ucw_dev/"
			   (merge-pathnames (layout.lib layout)
					    (slot-value layout 'root)))
    (darcs :op "unpull" :errorp nil
	   :args '("--from-match" "date \"Thu Jul 12 06:15:54 EEST 2007\"" "-a")))
  (link-systems layout))

(defvar +target-directory+ "/opt/core-server/")
(defparameter *layout* (if (zerop (sb-posix:geteuid))
			   (make-server-layout +target-directory+)
			   (make-layout +target-directory+)))
(install *layout*)
(config-1.0 *layout*)
