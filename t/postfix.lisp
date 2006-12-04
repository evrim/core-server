(in-package :core-server.test)

(defparameter *m*
  (make-instance 'postfix-server
		 :virtual-mailbox-maps (make-pathname :directory '(:absolute "tmp") :name "vmailbox")))

(describe *m*)

(start *m*)
(status *m*)
(stop *m*)

(add-email *m* "aycan@core.gen.tr" "/home/aycan/eposta/")
(del-email *m* "aycan@core.gen.tr")
