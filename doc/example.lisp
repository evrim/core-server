(in-package :asdf)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :core-server)
  (asdf:oos 'asdf:load-op :ucw+))

(in-package :core-server)

(defclass office1-server (apache-server ucw-server)
  ()
  (:default-initargs :name "office1"))

(defvar *server* (make-instance 'office1-server))
(start *server*)

(defvar *demo-app* (make-instance
		    'core-server::darcs-web-application
		    :project-name "demo" 
		    :fqdn "demo.core.gen.tr"
		    :admin-email "aycan.irican@core.gen.tr"
		    :source-pathname "/tmp/demo/"))

;; Now create project files and start it.
;; (serialize *demo-app*)
;; (push #P"/tmp/demo/" asdf::*central-registry*)
;; (asdf::oos 'asdf:load-op 'demo)
;; (demo::register-me *server*)

