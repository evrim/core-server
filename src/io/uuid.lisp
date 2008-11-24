(in-package :core-ffi)

(define-foreign-library libuuid
  (:unix "libuuid.so")
  (t (:default "libuuid")))

(use-foreign-library libuuid)

(defcfun ("uuid_unparse" %uuid-unparse) :void
  (uu :pointer)
  (char :pointer))

(defcfun ("uuid_generate" %uuid-generate) :void
  (uu :pointer))
