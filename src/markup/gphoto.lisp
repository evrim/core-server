;; +-------------------------------------------------------------------------
;; | Google GPhoto Markup
;; +-------------------------------------------------------------------------
;;
;; Author: Evrim Ulu <evrim@core.gen.tr>
;; Date: July 2011
;;
;; Specification:
;; http://code.google.com/apis/picasaweb/docs/2.0/reference.html#gphoto_reference
(in-package :core-server)

;; -------------------------------------------------------------------------
;; Atom Metaclass
;; -------------------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gphoto+ (xml+)
    ()
    (:default-initargs
     :namespace "gphoto"
     :schema "http://schemas.google.com/photos/2007")))

(defclass+ gphoto-element (xml)
  ())

;; +------------------------------------------------------------------------
;; | Atom Object Definition: defatom-tag
;; +------------------------------------------------------------------------
(defmacro defgphoto-tag (name &rest attributes)  
  `(progn
     (defclass+ ,name (gphoto-element)
       (,@(mapcar (lambda (attr) (list attr :print nil :host 'remote))
		  (remove 'id attributes)))
       (:metaclass gphoto+)
       (:tag ,@(string-downcase (symbol-name name)))
       (:attributes ,@attributes))
     (find-class+ ',name)))

;; https://picasaweb.google.com/data/feed/api/user/evrimulu?alt=json
(defgphoto-tag <gphoto:albumid)
(defgphoto-tag <gphoto:id)
(defgphoto-tag <gphoto:max-photos-per-album)
(defgphoto-tag <gphoto:nickname)
(defgphoto-tag <gphoto:quotacurrent)
(defgphoto-tag <gphoto:quotalimit)
(defgphoto-tag <gphoto:thumbnail)
(defgphoto-tag <gphoto:user)
(defgphoto-tag <gphoto:name)
(defgphoto-tag <gphoto:access)
(defgphoto-tag <gphoto:bytes-used)
(defgphoto-tag <gphoto:location)
(defgphoto-tag <gphoto:numphotos)
(defgphoto-tag <gphoto:numphotosremaining)
(defgphoto-tag <gphoto:checksum)
(defgphoto-tag <gphoto:comment-count)
(defgphoto-tag <gphoto:commenting-enabled)
(defgphoto-tag <gphoto:height)
(defgphoto-tag <gphoto:rotation)
(defgphoto-tag <gphoto:size)
(defgphoto-tag <gphoto:timestamp)
(defgphoto-tag <gphoto:videostatus)
(defgphoto-tag <gphoto:width)
(defgphoto-tag <gphoto:albumtitle)
(defgphoto-tag <gphoto:albumdesc)
(defgphoto-tag <gphoto:album-type)
(defgphoto-tag <gphoto:snippet)
(defgphoto-tag <gphoto:snippettype)
(defgphoto-tag <gphoto:truncated)
(defgphoto-tag <gphoto:photoid)
(defgphoto-tag <gphoto:weight)
