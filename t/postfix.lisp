(in-package :core-server.test)

(defparameter *m*
  (make-instance 'core-server::postfix-server
		 :virtual-mailbox-maps (make-pathname :directory '(:absolute "tmp") :name "vmailbox")))

(describe *m*)

(start *m*)
(status *m*)
(stop *m*)

(core-server::email-add *m* "aycan@core.gen.tr" "/home/aycan/eposta/")
(core-server::email-remove *m* "aycan@core.gen.tr")
