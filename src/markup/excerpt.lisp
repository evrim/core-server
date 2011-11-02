(in-package :core-server)

;; -------------------------------------------------------------------------
;; Excerpt Metaclass
;; -------------------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass excerpt+ (xml+)
    ()
    (:default-initargs
     :namespace "excerpt"
     :schema "http://wordpress.org/export/1.0/excerpt/")))

(defclass+ excerpt-element (xml)
  ((type :host remote :print nil)))

;; +------------------------------------------------------------------------
;; | Excerpt Object Definition: defexceprt-tag
;; +------------------------------------------------------------------------
(defmacro defexcerpt-tag (name &rest attributes)  
  `(progn
     (defclass+ ,name (excerpt-element)
       (,@(mapcar (lambda (attr) (list attr :print nil :host 'remote))
		  attributes))
       (:metaclass excerpt+)
       (:tag ,@(string-downcase (symbol-name name)))
       (:attributes ,@attributes))
     (find-class+ ',name)))

(defexcerpt-tag <excerpt:encoded)
