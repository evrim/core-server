(in-package :manager)

;; -------------------------------------------------------------------------
;; Server OAuth Credentials
;; -------------------------------------------------------------------------
(defclass+ facebook-credentials ()
  ((app-id :host both)
   (app-secret :host both))
  (:ctor make-facebook-credentials))

(defclass+ google-credentials ()
  ((client-id :host both)
   (client-secret :host both))
  (:ctor make-google-credentials))

(defclass+ twitter-credentials ()
  ((consumer-key :host both)
   (consumer-secret :host both))
  (:ctor make-twitter-credentials))
