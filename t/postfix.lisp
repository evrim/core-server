(in-package :core-server.test)

;; (def-suite postfix :in :core-server)

;; (in-suite postfix)

(defparameter *postfix*
  (make-instance 'postfix-server
		 :virtual-mailbox-maps (make-pathname :directory '(:absolute "tmp") :name "vmailbox")))
;; (test me
;;   (is (eq t t)))

;; (describe *m*)

;; (start *m*)
;; (status *m*)
;; (stop *m*)

;; (add-email *m* "aycan@core.gen.tr" "/home/aycan/eposta/")
;; (del-email *m* "aycan@core.gen.tr")
