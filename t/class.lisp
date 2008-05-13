(in-package :core-server.test)

(defclass+ classA ()
  ((slotA1 :host local :initform 'slotA1)
   (slotA2 :host local))
  (:ctor (slotA1 &rest slotA2)))

(defclass+ classB (classA)
  ((slotB1 :host local)
   (slotB2 :host remote))
  (:default-initargs :slotB1 'slotB1 :slotA1 'slotA1-overriden-by-B))

(defclass+ classC (classB)
  ((slotC1 :host local)
   (slotC2 :host remote)))

;;(deftest )