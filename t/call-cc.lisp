(in-package :core-server.test)

(defvar +special-var+ t)

(defun/cc handle-special (arg)
  (list arg +special-var+))


(deftest call/cc-1 
    (with-call/cc
      (handle-special 1))
  (1 t))

(deftest call/cc-2
    (with-call/cc
      +special-var+)
  t)

(deftest call/cc-3
    (with-call/cc
      (let ((+special-var+ 1))
	(handle-special 2)))
  (2 1))

(defun/cc override-special (arg)
  (let ((+special-var+ 3))
    (list arg +special-var+)))

(deftest call/cc-4
    (with-call/cc
      (override-special 1))
  (1 3))

