(in-package :core-server.test)

;; Generate unique filename for temporary usage
(sb-alien:define-alien-routine "tmpnam" sb-alien:c-string
  (dest (* sb-alien:c-string)))

(defparameter *dbpath*
  (pathname (format nil "~A/" (tmpnam nil))))

(defun test-tx-1 (server key value counter)
  (with-transaction (server)
    (setf (gethash key (database.root server)) value
	  (gethash 'counter (database.root server)) counter)))

(deftransaction test-tx-2 ((server database) key value)
  (setf (gethash key (database.root server)) value))

(defun database-test-execute-fun-1 ()
  (let ((database (make-instance 'database :database-directory *dbpath*))
	(counter 0))
    (start database)
    (test-tx-1 database 'test-key-1 'test-value-1 (incf counter))
    (test-tx-2 database 'test-key-2 'test-value-2)
    (stop database)))

(defun database-test-query-fun-2 ()
  (let ((database (make-instance 'database :database-directory *dbpath*)))
    (start database)
    (prog1 (list (gethash 'test-key-1 (database.root database))
		 (gethash 'test-key-2 (database.root database))
		 (gethash 'counter (database.root database)))
      (stop database))))

(deftest database-1
  (progn (database-test-execute-fun-1) (database-test-query-fun-2))
  (test-value-1 test-value-2 1))

(defun database-test-fun-3 ()
  (let ((database (make-instance 'database :database-directory *dbpath*)))
    (start database)
    (snapshot database)
    (stop database)
    (core-server::rm :path (merge-pathnames #P"transaction-log.xml" *dbpath*))))

(deftest database-2
  (progn (database-test-fun-3) (database-test-query-fun-2))
  (test-value-1 test-value-2 1))

;; http://www.cs.vu.nl/boilerplate/
;;
;; Scrap Your Boilerplate: A Practical Design Pattern for Generic Programming
;;           Ralf Lammel                         Simon Peyton Jones
;;    Vrije Universiteit, Amsterdam           Microsoft Research, Cambridge

;; data Company = C [Dept]
;; data Dept     = D Name Manager [SubUnit]
;; data SubUnit = PU Employee | DU Dept
;; data Employee = E Person Salary
;; data Person   = P Name Address
;; data Salary   = S Float
;; type Manager = Employee
;; type Name     = String
;; type Address = String

(defclass Company ()
  ((Depts :type Dept)))

(defclass Dept ()
  ((Name :type string)
   (Manager :type Employee)
   (SubUnits :type list)))

(defclass Employee (SubUnit)
  ((Person :type Person)
   (Salary :type float)))

(defclass Person ()
  ((Name :type string)
   (Address :type string)))

(defun genCom ()
  (let ((ralf (make-instance 'Employee
			     :Person (make-instance 'Person :Name "Ralf" :Address "Amsterdam")
			     :Salary 8000))
	(joost (make-instance 'Employee
			     :Person (make-instance 'Person :Name "Joost" :Address "Amsterdam")
			     :Salary 1000))
	(marlow (make-instance 'Employee
			     :Person (make-instance 'Person :Name "Marlow" :Address "Cambridge")
			     :Salary 2000))
	(blair (make-instance 'Employee
			     :Person (make-instance 'Person :Name "Blair" :Address "London")
			     :Salary 100000)))
    (make-instance 'Company
		   :Depts (list (make-instance 'Dept
					       :Name "Research"
					       :Manager ralf
					       :SubUnits (list joost marlow))
				(make-instance 'Dept
					       :Name "Strategy"
					       :Manager blair)))))

;; Let's implement a function increase of type Float -> Company ->
;; Company so that (increase 0.1 genCom) will be just like genCom
;; except that everyone's salary is incread by 10%.

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