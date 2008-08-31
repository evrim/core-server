(in-package :core-ffi)

(define-foreign-library libev
  (:unix (:or "libev.so" "libev.so.3.0.0"))
  (t (:default "libev")))

(use-foreign-library libev)
