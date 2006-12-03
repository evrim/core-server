;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(defclass trinode-application (apache-web-application modular-application apax-application-module cookie-session-module)
  ())

()

(defentry-point "trinode.core" ...
    ()
  (call 'make-instance 'coretal :configuration (call 'get-coretal-config)))


(defclass server-listener (server)
  ())

(defgeneric find-free-node (servers)
  (:documentation "Finds most empty server."))

(defclass node ()
  ())

(defun current-node ()
  *node1*)

;; send from socket
(defmethod send ((node node) expr)
  (write expr))

(defmacro with-node ((var node) &body body)
  `(send ,(funcall node)
	 '(let ((,var ,(funcall node)))
	   ,body)))

(defparameter *node1* (make-instance 'node))

(with-node (find-free-node (*servers*))
  (defparameter *core-server* (make-instance 'core-server)))

(
  (cl-prevalence::serialize-sexp '(lambda () (format t "asd")) out)
  (cl-prevalence::deserialize-sexp out))


(defparameter *trk* nil)

(time (loop
       for i from 1 to 25000000
       do (push i *trk*)))

;;fantaazi
(defun create-servers (num)
  (loop
     for n from 1 to num
     collect (make-instance 'ucw-server
			    :backend (ucw::make-backend :mod-lisp :host "127.0.0.1" :port (+ n 3010)))))

(defun test-servers (container)
  (reduce #'(lambda (a b) (and a b))
	  (mapcar #'(lambda (server) (status server))
		  container)))

(defun stop-servers (servers)
  (mapcar #'(lambda (server)
	      (stop server))
	  servers))

(let ((servers (create-servers 10)))
  (if (test-servers)
      (stop-servers servers)
      'failed))