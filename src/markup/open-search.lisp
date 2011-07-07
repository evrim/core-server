;; +-------------------------------------------------------------------------
;; | OpenSearch XML Format
;; +-------------------------------------------------------------------------
;;
;; Author: Evrim Ulu <evrim@core.gen.tr>
;; Date: July 2011
;;
;; Specification: 
;; http://www.opensearch.org/Specifications/OpenSearch/1.1
(in-package :core-server)

;; -------------------------------------------------------------------------
;; OpenSearch Metaclass
;; -------------------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass open-search+ (xml+)
    ()
    (:default-initargs
     :namespace "openSearch"
     :schema "http://www.opensearch.org/Specifications/OpenSearch/1.1")))

(defclass+ open-search-element (xml)
  ())

;; +------------------------------------------------------------------------
;; | Atom Object Definition: defatom-tag
;; +------------------------------------------------------------------------
(defmacro defopen-search-tag (name &rest attributes)  
  `(progn
     (defclass+ ,name (open-search-element)
       (,@(mapcar (lambda (attr) (list attr :print nil :host 'remote))
		  (remove 'id attributes)))
       (:metaclass open-search+)
       (:tag ,@(string-downcase (symbol-name name)))
       (:attributes ,@attributes))
     (find-class+ ',name)))

(defopen-search-tag <open-search:total-results)
(defopen-search-tag <open-search:start-index)
(defopen-search-tag <open-search:items-per-page)



