(in-package :core-ffi)

(defmacro defwatcher-cb (name &body body)
  `(defcallback ,name :void ((loop :pointer) (watcher :pointer) (revents :int))
     (declare (ignorable loop watcher revents))
     ,@body))
