(in-package :core-server)

;; -------------------------------------------------------------------------
;; Persistent Application
;; -------------------------------------------------------------------------
;; By default every web-application is persistent if :persist slot is t


;; -------------------------------------------------------------------------
;; Persistent HTTP Application Metaclass
;; -------------------------------------------------------------------------
;; This application metaclass is for the use manager-application.
(defclass+ persistent-http-application+ (dynamic-class+ http-application+)
  ())

