(in-package :core-server)

;; -------------------------------------------------------------------------
;; DC Metaclass
;; -------------------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass dc+ (xml+)
    ()
    (:default-initargs
     :namespace "dc"
     :schema "http://purl.org/dc/elements/1.1/")))

(defclass+ dc-element (xml)
  ((type :host remote :print nil)))

;; +------------------------------------------------------------------------
;; | Atom Object Definition: defatom-tag
;; +------------------------------------------------------------------------
(defmacro defdc-tag (name &rest attributes)  
  `(progn
     (defclass+ ,name (dc-element)
       (,@(mapcar (lambda (attr) (list attr :print nil :host 'remote))
		  attributes))
       (:metaclass dc+)
       (:tag ,@(string-downcase (symbol-name name)))
       (:attributes ,@attributes))
     (find-class+ ',name)))

(defdc-tag <dc:creator)
