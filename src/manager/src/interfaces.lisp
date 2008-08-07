(in-package :manager)

(defun hostname ()
  #+sbcl
  (sb-unix:unix-gethostname)
  #-sbcl
  "N/A")
