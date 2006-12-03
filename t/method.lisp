(in-package :core-server.test)
;; sysv-standard method combination

(defclass a-server (server)
  ())

(defclass b-server (server)
  ())

(defclass c-server (a-server)
  ())

(defclass core-server (c-server b-server ucw-server)
  ())

;; TEST> (start (make-instance 'core-server::core-server))
;; c-server init-instance
;; core-server init-instance
;; around start core-server
;; starting ucw-server
;; starting b-server
;; starting a-server
;; starting c-server
;; starting core-server
;; NIL
;; TEST> (stop (make-instance 'core-server::core-server))
;; c-server init-instance
;; core-server init-instance
;; around stop core-server
;; stopping core-server
;; stopping c-server
;; stopping a-server
;; stopping b-server
;; stopping ucw-server
;; T
