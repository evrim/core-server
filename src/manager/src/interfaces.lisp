(in-package :manager)

(defun hostname ()
  #+sbcl
  (sb-unix:unix-gethostname)
  #-sbcl
  "N/A")

(defun core-server-version ()
  (slot-value (asdf::find-system "core-server") 'asdf::version))

