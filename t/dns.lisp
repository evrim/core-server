(in-package :core-server.test)

(defparameter *name-server* (make-instance 'name-server))
(describe *name-server*)
(start *name-server*)
(status *name-server*)
(stop *name-server*)

(find-domain-records *name-server* "core.gen.tr")
(name-server.refresh *name-server*)

(add-ns *name-server* "ns1.core.gen.tr" "1.2.3.4")
(add-mx *name-server* "mx.core.gen.tr" "1.2.3.5")

