(in-package :tr.gen.core.server)

(defclass a-server (server)
  ())

(defmethod start ((self a-server))
  (format t "starting a-server~%")
  (error "a server bozuldu"))

(defmethod stop ((self a-server))
  (format t "stopping a-server~%"))

(defclass b-server (server)
  ())

(defmethod start ((self b-server))
  (format t "starting b-server~%"))

(defmethod stop ((self b-server))
  (format t "stopping b-server~%"))

(defclass c-server (a-server)
  ())

(defmethod initialize-instance :after ((self c-server) &rest initargs &key &allow-other-keys)
  (format t "c-server init-instance~%"))

(defmethod start ((self c-server))
  (format t "starting c-server~%")
;;  (error "c-server'in ztartinda acaip hata oldu inanmazsin.")
  )

(defmethod stop ((self c-server))
  (format t "stopping c-server~%"))

(defclass core-server (c-server b-server ucw-server)
  ())

(defmethod initialize-instance :after ((self core-server) &rest initargs &key &allow-other-keys)
  (format t "core-server init-instance~%"))

(defmethod start ((self core-server))
  (format t "starting core-server~%"))

(defmethod start :around ((self core-server))
  (format t "around start core-server~%")
;;  (error "error in core-server")
  (call-next-method))

(defmethod stop ((self core-server))
  (format t "stopping core-server~%"))

(defmethod stop :around ((self core-server))
  (format t "around stop core-server~%")
  (call-next-method))