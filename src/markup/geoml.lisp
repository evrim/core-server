;; +-------------------------------------------------------------------------
;; | GeoRSS & GeoMarkupLanguage
;; +-------------------------------------------------------------------------
;;
;; Author: Evrim Ulu <evrim@core.gen.tr>
;; Date: July 2011
;;
;; Specification: http://tools.ietf.org/html/rfc4287
;;
(in-package :core-server)

;; -------------------------------------------------------------------------
;; GeoML Metaclass
;; -------------------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass geo-ml+ (xml+)
    ()
    (:default-initargs
     :namespace "atom"
     :schema "http://www.w3.org/2005/Atom")))

(defclass+ atom-element (xml)
  ((type :host remote :print nil)))

;; +------------------------------------------------------------------------
;; | Atom Object Definition: defatom-tag
;; +------------------------------------------------------------------------
(defmacro defatom-tag (name &rest attributes)  
  `(progn
     (defclass+ ,name (atom-element)
       (,@(mapcar (lambda (attr) (list attr :print nil :host 'remote))
		  (remove 'id attributes)))
       (:metaclass atom+)
       (:tag ,@(string-downcase (symbol-name name)))
       (:attributes ,@attributes))
     (find-class+ ',name)))
