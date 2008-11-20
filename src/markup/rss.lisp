;; Core Server: Web Application Server

;; Copyright (C) 2006-2008  Metin Evrim Ulu, Aycan iRiCAN

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :core-server)

;;+----------------------------------------------------------------------------
;;| RSS 2 Library
;;+----------------------------------------------------------------------------
;;
;; This file contains implementation of W3C HTML 4 Standard.
;; http://cyber.law.harvard.edu/rss/rss.html
;;
(defclass rss-element (dom-element)
  ()
  (:documentation "RSS Base Class")
  (:metaclass dom-element+))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defmacro defrss-tag (name &rest attributes)
    "This macro is used to define HTML Element with 'attributes'"
    (let ((attributes (make-html-attributes attributes))
	  (symbol (intern (symbol-name name) :tr.gen.core.server.rss)))
      (export symbol (find-package :tr.gen.core.server.rss))
      `(prog1 (defclass ,symbol (rss-element)
		()
		(:metaclass dom-element+))
	 (defun ,symbol (&rest args)
	   (multiple-value-bind (attributes children) (tag-attributes args)
	     (destructuring-bind (&key ,@attributes) attributes
	       (make-instance ',symbol
			      :tag ,(symbol-name name)
			      :attributes (remove-if (lambda (attr)
						       (if (null (cdr attr))
							   t))
						     (list ,@(mapcar (lambda (attr)
								       `(cons ,(symbol-to-js attr) ,attr))
								     attributes)))
			      :children (flatten children)))))
	 (eval-when (:compile-toplevel :load-toplevel :execute)
	   (export ',symbol (find-package :tr.gen.core.server.rss)))))))

(defmethod validate-rss ((dom-element t))
  (warn "Unknown RSS Element:~A" dom-element)
  dom-element)

(defmethod validate-rss ((dom-element string))
  dom-element)

(defmethod validate-rss ((dom-element dom-element))
  (aif (find-class (intern (string-upcase (dom.tag dom-element))
			   (find-package :tr.gen.core.server.rss)) nil)
       (change-class dom-element it))
  (setf (dom.children dom-element)
	(mapcar #'validate-rss (dom.children dom-element)))
  dom-element)

(defmethod rss? ((stream core-stream))
  (let ((dom (dom-element? stream)))
    (if dom (validate-rss dom))))

(defmethod rss! ((stream core-stream) (element rss-element))
  (dom-element! stream element))
;; RSS
(defrss-tag rss)

;; CHANNEL
(defrss-tag channel)
(defrss-tag title)
(defrss-tag link)
(defrss-tag description)
(defrss-tag language)
(defrss-tag copyright)
(defrss-tag managing-editor)
(defrss-tag web-master)
(defrss-tag last-build-date)
(defrss-tag generator)
(defrss-tag docs)
(defrss-tag rating)
(defrss-tag skip-hours)
(defrss-tag skip-days)

;; IMAGE
(defrss-tag image)
(defrss-tag height)
(defrss-tag width)

;; CLOUD
(defrss-tag cloud)

;; TTL
(defrss-tag ttl)

;; TEXTINPUT
(defrss-tag text-input)
(defrss-tag name)

;; ITEM
(defrss-tag item)
(defrss-tag enclosure)

;; SOURCE
(defrss-tag source)

;; ENCLOSURE
(defrss-tag enclosure url length type)

;; CATEGORY
(defrss-tag category domain)

;; PUBDATE
(defrss-tag pub-date)

;; GUID
(defrss-tag guid is-permalink)

;; COmmENTS
(defrss-tag comments)

;; AUTHOR
(defrss-tag author)

;; SERVER> (rss? (make-core-stream "<rss><channel></channel></rss>"))
;; #<TR.GEN.CORE.SERVER.RSS:RSS rss(1) {100413EAC1}>
;; SERVER> (describe (rss? (make-core-stream "<rss><channel></channel></rss>")))

;; #<TR.GEN.CORE.SERVER.RSS:RSS rss(1) {10041B22D1}>
;; is an instance of class #<STANDARD-CLASS TR.GEN.CORE.SERVER.RSS:RSS>.
;; The following slots have :INSTANCE allocation:
;;  TAG           "rss"
;;  NAMESPACE     NIL
;;  ATTRIBUTES    NIL
;;  CHILDREN      (#<TR.GEN.CORE.SERVER.RSS:CHANNEL channel(0) {10041B10F1}>)
;; ; No value
;; SERVER> 
;; SERVER> (rss! *core-output* (rss? (make-core-stream "<rss><channel></channel></rss>")))
;; <rss>
;;   <channel></channel></rss>
