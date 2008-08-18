(in-package :core-server.test)

(defmacro defserialization-test (name value &optional (equal 'equalp))
  `(deftest ,name
       (let ((s (make-core-stream "")))
	 (serialize-xml s ,value (serialization-cache))
	 (let ((value (deserialize-xml (make-core-stream (return-stream s)) (serialization-cache))))
	   (,equal ,value value)))
     t))

(defserialization-test serialize-null nil)
(defserialization-test serialize-true t)
(defserialization-test serialize-symbol 'aycan-bey-nasilsiniz?)
(defserialization-test serialize-character #\A)
(defserialization-test serialize-int 100000)
(defserialization-test serialize-ratio (/ 1 2))
(defserialization-test serialize-complex #C(3 4))
(defserialization-test serialize-float (float (/ 1 2)))
(defserialization-test serialize-string "mooo 1 2 3>")
(defserialization-test serialize-cons (cons 1 2))
(defserialization-test serialize-list (list 3 4 5 6))

(defun serialization-hash-table ()
  (let ((table (make-hash-table :test #'equal)))
    (setf (gethash "test2" table) "test1-value"
	  (gethash "test2" table) "test2-value")
    table))

(defserialization-test serialize-hash-table (serialization-hash-table))

(defstruct (my-structure)
  (slot1))

(defun serialization-structure ()
  (make-my-structure :slot1 "test"))

(defserialization-test serialize-strcture (serialization-structure))

(defclass my-class ()
  ((slot1 :initarg :slot1)))

(defun serialization-object ()
  (make-instance 'my-class :slot1 "slot1"))

(defun my-object-equalp (a b)
  (and (equal (class-of a) (class-of b))
       (equal (slot-value a 'slot1) (slot-value b 'slot1))))

(defserialization-test serialize-object (serialization-object)
  my-object-equalp)


;; (defclass db-model ()
;;   ((abc :initform "abcdef")))

;; (defparameter *db* 
;;   (make-instance 'database-server
;; 		 :db-auto-start nil
;; 		 :directory #P"/tmp/ceek/"
;; 		 :model-class 'db-model))
;; (describe *db*)
;; (start *db*)
;; (status *db*)
;; (stop *db*)

;; (defun tx-set-abc (server abc)
;;   (let ((model (model server)))
;;     (setf (slot-value model 'abc) abc)))

;; (defun set-abc (abc)
;;   (execute *db* (make-transaction 'tx-set-abc abc)))

;; (set-abc "789")
;; (describe (model *db*))
;; (snapshot *db*)
;; (set-abc "91011")