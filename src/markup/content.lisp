(in-package :core-server)

;; -------------------------------------------------------------------------
;; Content Metaclass
;; -------------------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass content+ (xml+)
    ()
    (:default-initargs
     :namespace "content"
     :schema "http://purl.org/rss/1.0/modules/content/")))

(defclass+ content-element (xml)
  ((type :host remote :print nil)))

;; +------------------------------------------------------------------------
;; | Content Object Definition: defcontent-tag
;; +------------------------------------------------------------------------
(defmacro defcontent-tag (name &rest attributes)  
  `(progn
     (defclass+ ,name (content-element)
       (,@(mapcar (lambda (attr) (list attr :print nil :host 'remote))
		  attributes))
       (:metaclass content+)
       (:tag ,@(string-downcase (symbol-name name)))
       (:attributes ,@attributes))
     (find-class+ ',name)))

(defcontent-tag <content:encoded)
