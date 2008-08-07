(in-package :manager)

(defclass manager-model nil nil)

(defmethod print-object ((self manager-model) stream)
           (print-unreadable-object (self stream :type t :identity t)))

