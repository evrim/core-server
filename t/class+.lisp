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

;; (defcomp totos ()
;;   ((abc :host local :initform 'gee))
;;   (:default-initargs :abc 'abc))

;; ;; (defcomponent totos ()
;; ;;   ((abc :host local))
;; ;;   (:default-initargs :abc 'abc))

;; ;; (defcommand totos ()
;; ;;   ((abc :host local))
;; ;;   (:default-initargs :abc 'abc))

;; (defcomp sotot (totos)
;;   ((def :host local :initform 'eben))
;;   (:default-initargs :def 'def :abc 'gee))

;; (defcomponent sotot (totos)
;;   ((def :host remote))
;;   (:default-initargs :def 'def :abc 'gee))

;; (defcommand sotot (totos)
;;   ((def :host remote))
;;   (:default-initargs :def 'def :abc 'gee))