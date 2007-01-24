(in-package :asdf)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :core-server)
  (asdf:oos 'asdf:load-op :ucw+))

(in-package :core-server)

(defclass core-server (apache-server ucw-server)
  ()
  (:default-initargs :name "servername"))

(defvar *server* (make-instance 'core-server))
(start core-server::*server*)
(swank:create-server :port 4005)
