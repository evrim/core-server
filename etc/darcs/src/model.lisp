(in-package :project-name)

(defclass project-name-model ()
  ())

(defmethod print-object ((self project-name-model) stream)
  (print-unreadable-object (self stream :type t :identity t)))
