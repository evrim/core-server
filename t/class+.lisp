(in-package :core-server.test)

(deftest class+0
    (cdr (multiple-value-list
	  (class+.register
	   (class+
	    'classX '() 
	    '((slotX :host local)) 
	    '((:default-initargs :slotX 1)
	      (:documentation "doc")
	      (:ctor (&optional slotX))
	      (:metaclass moo))))))
  (((slotx :accessor classx.slotx :initarg :slotx :initform 1))
   ((:default-initargs :slotx 1)
    (:documentation "doc")
    (:metaclass moo))))

(defclass+ classA ()
  ((slotA1 :host local :initform 'slotA1)
   (slotA2 :host local))
  (:default-initargs :slotA1 'zoo)
  (:ctor (slotA1 &rest slotA2)))

(deftest class+1
    (not (null (find-class+ 'classA)))
  t)

(deftest class+2
    (class+.slots (find-class+ 'classA))
  ((slotA1 :client-type primitive :accessor classa.slota1 :initarg :slota1 :host local :initform 'zoo)
   (slotA2 :client-type primitive :accessor classa.slota2 :initarg :slota2 :initform nil :host local)))

(deftest class+3
    (class+.default-initargs (find-class+ 'classA))
  (:slotA1 'zoo))

(deftest class+4
    (fboundp 'classA)
  t)

(deftest class+5
    (let ((class+ (classA 'slotA1X 'slotA21 'slotA22)))
      (list (classa.slotA1 class+) (classa.slotA2 class+)))
  (slotA1X (slotA21 slotA22)))

(deftest class+6
    (let ((class+ (make-instance 'classA)))
      (list (classa.slotA1 class+) (classa.slotA2 class+)))
  (zoo nil))

(defclass+ classB (classA)
  ((slotB1 :host local)
   (slotB2 :host remote))
  (:default-initargs :slotB1 'slotB1 :slotA1 'slotA1-overriden-by-B))

(deftest class+7
    (class+.slots (find-class+ 'classB))
  ((slotB1 :client-type primitive :accessor classb.slotb1 :initarg :slotb1 :initform 'slotb1 :host local)
   (slotB2 :client-type primitive :accessor classb.slotb2 :initarg :slotb2 :initform nil :host remote)
   (slotA1 :client-type primitive :accessor classa.slota1 :initarg :slota1 :host local :initform 'slotA1-overriden-by-B)
   (slotA2 :client-type primitive :accessor classa.slota2 :initarg :slota2 :initform nil :host local)))

(defclass+ classC (classB)
  ((slotC1 :host local)
   (slotC2 :host remote)))

(deftest class+8
    (class+.local-slots (find-class+ 'classc))
  ((slotc1 :client-type primitive :accessor classc.slotc1 :initarg :slotc1 :initform nil :host local)
   (slotb1 :client-type primitive :accessor classb.slotb1 :initarg :slotb1 :initform 'slotb1 :host local)
   (slota1 :client-type primitive :accessor classa.slota1 :initarg :slota1 :host local :initform 'slota1-overriden-by-b)
   (slota2 :client-type primitive :accessor classa.slota2 :initarg :slota2 :initform nil :host local)))

(deftest class+9
    (class+.remote-slots (find-class+ 'classc))
  ((slotc2 :client-type primitive :accessor classc.slotc2 :initarg :slotc2 :initform nil :host remote)
   (slotb2 :client-type primitive :accessor classb.slotb2 :initarg :slotb2 :initform nil :host remote)))
