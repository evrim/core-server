(in-package :core-server.test)

(defmacro defserialization-test (name lisp-element db-element &optional (equal 'equalp))
  `(deftest ,name
       (and (xml.equal (xml-serialize ,lisp-element) ,db-element)
	    (,equal (xml-deserialize ,db-element) ,lisp-element))     
     t))

(defserialization-test serialize-null nil (<db:null))
(defserialization-test serialize-true t (<db:true))
(defserialization-test serialize-symbol 'my-symbol? (<db:symbol "TR.GEN.CORE.SERVER.TEST::MY-SYMBOL?"))
(defserialization-test serialize-character #\A (<db:character "\"A\""))
(defserialization-test serialize-int 100000 (<db:integer "100000"))
(defserialization-test serialize-ratio (/ 1 2) (<db:ratio "1/2"))
(defserialization-test serialize-complex #C(3 4) (<db:complex "#C(3 4)"))
(defserialization-test serialize-float (float (/ 1 2)) (<db:float "0.5"))
(defserialization-test serialize-string "mooo 1 2 3>" (<db:string "\"mooo 1 2 3>\""))
(defserialization-test serialize-cons (cons 1 2)
  (<db:cons :consp t
	    (<db:integer "1")
	    (<db:integer "2")))
(defserialization-test serialize-list (list 3 4 5 6)
  (<db:cons :length 4
	    (<db:integer "3")
	    (<db:integer "4")
	    (<db:integer "5")
	    (<db:integer "6")))

(defun serialization-hash-table ()
  (let ((table (make-hash-table :test #'equal)))
    (setf (gethash "test1" table) "test1-value"
	  (gethash "test2" table) "test2-value")
    table))

(defun hash-table-equalp (a b)
  (and (equal (hash-table-keys a) (hash-table-keys b))
       (equal (hash-table-values a) (hash-table-values b))))

(defserialization-test serialize-hash-table (serialization-hash-table)
  (<db:hash-table :test "COMMON-LISP::EQUAL"
		  :size "16"
		  (<db:hash-table-entry
		   (<db:hash-table-key (<db:string "\"test1\""))
		   (<db:hash-table-value (<db:string "\"test1-value\"")))
		  (<db:hash-table-entry
		   (<db:hash-table-key (<db:string "\"test2\""))
		   (<db:hash-table-value (<db:string "\"test2-value\""))))
  hash-table-equalp)

(defstruct (my-structure)
  (slot1))

(defun serialization-structure ()
  (make-my-structure :slot1 "test"))

(defserialization-test serialize-structure (serialization-structure)
  (<db:struct :class "TR.GEN.CORE.SERVER.TEST::MY-STRUCTURE"
	      (<db:slot :name "TR.GEN.CORE.SERVER.TEST::SLOT1"
			(<db:string "\"test\""))))

(defclass my-class ()
  ((slot1 :initarg :slot1)))

(defun serialization-object ()
  (make-instance 'my-class :slot1 "slot1"))

(defun my-object-equalp (a b)
  (and (equal (class-of a) (class-of b))
       (equal (slot-value a 'slot1) (slot-value b 'slot1))))

(defserialization-test serialize-object (serialization-object)
  (<db:instance :class "TR.GEN.CORE.SERVER.TEST::MY-CLASS"
		(<db:slot :name "TR.GEN.CORE.SERVER.TEST::SLOT1"
			  (<db:string "\"slot1\"")))
  my-object-equalp)

(defclass+ my-class+ ()
  ((slot1 :initarg :slot1)
   (slot2 :host remote)))

(defserialization-test serialize-class+ (my-class+ :slot1 "slot1")
  (<db:instance :class "TR.GEN.CORE.SERVER.TEST::MY-CLASS+"
		(<db:slot :name "TR.GEN.CORE.SERVER.TEST::SLOT1"
			  (<db:string "\"slot1\"")))
  my-object-equalp)

;; Generate unique filename for temporary usage
(sb-alien:define-alien-routine "tmpnam" sb-alien:c-string
  (dest (* sb-alien:c-string)))

(defparameter *dbpath*
  (pathname (format nil "~A/" (tmpnam nil))))

(defvar *db* nil)

(defun test-db ()
  (setf *dB* (make-instance 'database :database-directory *dbpath*))
  (start *db*)
  *db*)

(deftest db-serialize-object
    (let ((db (test-db)))
      (and (= 123 (database.deserialize db (database.serialize db 123)))))
  t)
