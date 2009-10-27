;; +----------------------------------------------------------------------------
;; | Basic Database Tests
;; +----------------------------------------------------------------------------
(in-package :core-server.test)

;; Generate unique filename for temporary usage
(sb-alien:define-alien-routine "tmpnam" sb-alien:c-string
  (dest (* sb-alien:c-string)))

(defparameter *dbpath* #P"/tmp/core-server-test/")

(defvar *db* nil)

(defun test-db ()
  (setf *dB* (make-instance 'database :database-directory *dbpath*))
  (purge *db*)
  (start *db*)
  *db*)

(deftest database-1
    (let ((db (test-db)))
      (setf (database.get db 'key) 'value)
      (stop db)
      (start db)
      (prog1 (database.get db 'key)
	(stop db)))
  value)

(deftest database-2
    (let ((db (test-db)))
      (setf (database.get db 'foo) 'bar)
      (snapshot db)
      (stop db)
      (start db)
      (prog1 (database.get db 'foo)
	(stop db)))
  bar)

(defclass test-db-class ()
  ((slot1 :initform "slot1")))

(deftest database-3
    (let ((db (test-db))
	  (str (random-string)))
      (let ((object (add-object db 'test-db-class)))
	(update-object db object (cons 'slot1 str)))
      (stop db)
      (start db)
      (let ((object (find-object-with-slot db 'test-db-class 'slot1 str)))
	(prog1 (and object t)
	  (if object (delete-object db object))
	  (stop db))))
  t)

(defclass+ persistent-class (object-with-id)
  ((slot1 :initform "slot1" :print t)))

(deftest database-4
    (let ((db (test-db))
	  (id))
      (let ((object (add-object db 'persistent-class)))
	(update-object db object (cons 'slot1 "test1-slotval"))
	(setq id (slot-value object 'id)))
      (stop db)
      (start db)
      (let ((object (find-object-with-slot db 'persistent-class 'id id)))
	(prog1 (slot-value object 'slot1)
	  (delete-object db object)
	  (stop db))))
  "test1-slotval")


(defclass+ user (object-with-id)
  ((name :initform "name" :print t :host local)
   (blogs :type blog* :relation user)
   (domains :type domain* :relation users)))

(defclass+ blog (object-with-id)
  ((title :initform "title" :print t)
   (user :type user :relation blogs)))

(defclass+ domain (object-with-id)
  ((fqdn :initform "fqdn")
   (users :type user* :relation domains)))

;; Testing Basic 1-N, N-1
(deftest database-5
    (let ((db (test-db)))
      (let* ((user (add-object db 'user (cons 'name "evrim")))
	     (blog (add-object db 'blog (cons 'title "myblog1") (cons 'user user))))
	(and (eq (blog.user blog) user)
	     (member blog (user.blogs user))
	     (progn
	       (delete-object db blog)
	       (not (member blog (user.blogs user))))
	     (progn
	       (update-object db user (cons 'blogs (list blog)))
	       (and (member blog (user.blogs user))
		    (eq (blog.user blog) user))))))
  t)

;; Testing 1-N, N-1
(deftest database-6
    (let ((db (test-db)))
      (let ((u1 (add-object db 'user (cons 'name (random-string))))
	    (u2 (add-object db 'user (cons 'name (random-string)))))
	(let ((blog (add-object db 'blog (cons 'title (random-string)) (cons 'user u1))))	  
	  (update-object db blog (cons 'user u2))
	  (and (not (member blog (user.blogs u1)))
	       (member blog (user.blogs u2))
	       (eq u2 (blog.user blog))
	       (progn
		 (delete-object db blog)
		 (not (member blog (user.blogs u2))))))))
  t)

;; Testing N-N
(deftest database-7
    (let* ((db (test-db))
	   (u1 (add-object db 'user (cons 'name (random-string))))
	   (d1 (add-object db 'domain (cons 'fqdn (random-string)) (cons 'users (list u1)))))
      (and (member u1 (domain.users d1))
	   (member d1 (user.domains u1))
	   (progn
	     (delete-object db d1)
	     (not (member d1 (user.domains u1))))))
  t)

;; Crud
(defcrud user)
(deftest database-8
    (let* ((db (test-db))
	   (u1 (user.add db :name "foo")))
      (user.update db u1 :name "bar")
      
      (prog1 (user.name u1)
	(user.delete db u1)))
  "bar")

;; ;; http://www.cs.vu.nl/boilerplate/
;; ;;
;; ;; Scrap Your Boilerplate: A Practical Design Pattern for Generic Programming
;; ;;           Ralf Lammel                         Simon Peyton Jones
;; ;;    Vrije Universiteit, Amsterdam           Microsoft Research, Cambridge

;; ;; data Company = C [Dept]
;; ;; data Dept     = D Name Manager [SubUnit]
;; ;; data SubUnit = PU Employee | DU Dept
;; ;; data Employee = E Person Salary
;; ;; data Person   = P Name Address
;; ;; data Salary   = S Float
;; ;; type Manager = Employee
;; ;; type Name     = String
;; ;; type Address = String

;; (defclass+ Person ()
;;   ((Name :type string)
;;    (Address :type string)))

;; (defclass+ Employee (Person)
;;   ((Salary :type float)))

;; (defclass+ Manager (Employee)
;;   ((Departments :type Departments :relation (1-N Department))))

;; (defclass+ Company ()
;;   ((Depts :type Dept :relation (1-N Department))))

;; (defclass+ Department ()
;;   ((Name :type string)
;;    (Manager :type Employee :relation (N-1 Employee))
;;    (SubUnits :type list :relation (1-N (or Department Employee)))))

;; (defun genCom ()
;;   (let ((ralf (make-instance 'Employee
;; 			     :Person (make-instance 'Person :Name "Ralf" :Address "Amsterdam")
;; 			     :Salary 8000))
;; 	(joost (make-instance 'Employee
;; 			     :Person (make-instance 'Person :Name "Joost" :Address "Amsterdam")
;; 			     :Salary 1000))
;; 	(marlow (make-instance 'Employee
;; 			     :Person (make-instance 'Person :Name "Marlow" :Address "Cambridge")
;; 			     :Salary 2000))
;; 	(blair (make-instance 'Employee
;; 			     :Person (make-instance 'Person :Name "Blair" :Address "London")
;; 			     :Salary 100000)))
;;     (make-instance 'Company
;; 		   :Depts (list (make-instance 'Dept
;; 					       :Name "Research"
;; 					       :Manager ralf
;; 					       :SubUnits (list joost marlow))
;; 				(make-instance 'Dept
;; 					       :Name "Strategy"
;; 					       :Manager blair)))))

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