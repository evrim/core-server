;; +-------------------------------------------------------------------------
;; | Yahoo Media RSS Markup
;; +-------------------------------------------------------------------------
;;
;; Author: Evrim Ulu <evrim@core.gen.tr>
;; Date: July 2011
;;
;; Specification:
;; http://video.search.yahoo.com/mrss
;; http://code.google.com/apis/picasaweb/docs/2.0/reference.html#media_reference
(in-package :core-server)

;; -------------------------------------------------------------------------
;; Media Metaclass
;; -------------------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass media+ (xml+)
    ()
    (:default-initargs
     :namespace "media"
     :schema "http://search.yahoo.com/mrss/")))

(defclass+ media-element (xml)
  ())

;; +------------------------------------------------------------------------
;; | Atom Object Definition: defatom-tag
;; +------------------------------------------------------------------------
(defmacro defmedia-tag (name &rest attributes)  
  `(progn
     (defclass+ ,name (gphoto-element)
       (,@(mapcar (lambda (attr) (list attr :print nil :host 'remote))
		  (remove 'id attributes)))
       (:metaclass media+)
       (:tag ,@(string-downcase (symbol-name name)))
       (:attributes ,@attributes))
     (find-class+ ',name)))


(defmedia-tag <media:group)
(defmedia-tag <media:content url file-size type medium is-default expression
	      bitrate framerate samplingrate channels
	      duration height width)
(defmedia-tag <media:rating scheme)
(defmedia-tag <media:title type)
(defmedia-tag <media:description type)
(defmedia-tag <media:keywords)
(defmedia-tag <media:thumbnail url height width time)
(defmedia-tag <media:category scheme label)
(defmedia-tag <media:hash algo)
(defmedia-tag <media:player url height width)
(defmedia-tag <media:credit role scheme)
(defmedia-tag <media:copyright url)
(defmedia-tag <media:text type lang start end)
(defmedia-tag <media:restriction relationship type)
(defmedia-tag <media:community star-rating statistics tags)
(defmedia-tag <media:comments)
(defmedia-tag <media:comment)
(defmedia-tag <media:embed url height width)
(defmedia-tag <media:responses)
(defmedia-tag <media:response)
(defmedia-tag <media:back-links)
(defmedia-tag <media:back-link)
(defmedia-tag <media:status state reason)
(defmedia-tag <media:price type info price currency)
(defmedia-tag <media:license type href)
(defmedia-tag <media:sub-title type language href)
(defmedia-tag <media:peer-link type href)
(defmedia-tag <media:location description start end)
(defmedia-tag <media:rights status)
(defmedia-tag <media:scenes)
(defmedia-tag <media:scene)
