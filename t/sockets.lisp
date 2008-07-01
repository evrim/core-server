(in-package :tr.gen.core.server.test)

(defparameter *listen-ip* "0.0.0.0")
(defparameter *listen-port* 5555)

(deftest resolv-hostname
    (let ((h #(139 179 139 251))
	  (result (resolve-hostname "www.core.gen.tr")))
      (reduce #'(lambda (x y)
		  (if x y nil))
	      (map 'list #'equal h result)
	      :initial-value t))
  t)

(deftest server
    (let ((server (make-server :host *listen-ip* :port *listen-port*)))
      (handler-case
	  (unwind-protect
	       (let* ((stream (connect *listen-ip* *listen-port*)))
		 ;; should listen on 5555
		 (and server stream t))
	    (close-server server))
	(condition (c)
	  (close-server server))))
  t)