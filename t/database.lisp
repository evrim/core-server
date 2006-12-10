(in-package :core-server.test)

(defclass db-model ()
  ((abc :initform "abcdef")))

(defparameter *db* 
  (make-instance 'database-server
		 :db-auto-start nil
		 :directory #P"/tmp/ceek/"
		 :model-class 'db-model))
(describe *db*)
(start *db*)
(status *db*)
(stop *db*)

(defun tx-set-abc (server abc)
  (let ((model (model server)))
    (setf (slot-value model 'abc) abc)))

(defun set-abc (abc)
  (execute *db* (make-transaction 'tx-set-abc abc)))

(set-abc "789")
(describe (model *db*))
(snapshot *db*)
(set-abc "91011")