(in-package :tr.gen.core.server)

(defvar *www #(212 175 40 11))
(defvar *www-result (resolve-hostname "www.core.gen.tr"))
(eq t (reduce #'(lambda (acc item)
		  (if item
		      t))
	      (map 'list #'equal *www *www-result)
	      :initial-value nil))

(defparameter *server (make-server :port 5555))
(describe *server)
(close-server *server)