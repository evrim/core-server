(in-package :tr.gen.core.server)

(defmethod start ((self ucw-server))
  (format t "starting ucw-server~%"))

(defmethod stop ((self ucw-server))
  (format t "stopping ucw-server~%"))

(defmethod status ((self ucw-server))
  t)