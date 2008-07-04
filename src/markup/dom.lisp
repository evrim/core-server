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
;;| DOM Library
;;+----------------------------------------------------------------------------
;;
;; This file contains data structures, parsers, renderers for generic document
;; object model of W3C (ie html, xml, rss, rdf)
;;

;;-----------------------------------------------------------------------------
;; Generic Document Object Model Element
;;-----------------------------------------------------------------------------
(defclass dom-element ()
  ((tag :accessor dom.tag :initarg :tag :initform nil)
   (namespace :accessor dom.namespace :initarg :namespace :initform nil)
   (attributes :accessor dom.attributes :initarg :attributes :initform nil)
   (children :accessor dom.children :initarg :children :initform nil))
  (:documentation "DOM Element"))

(defmethod print-object ((self dom-element) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (if (dom.namespace self)
	(format stream "~A:~A(~D)"
		(dom.tag self) (dom.namespace self) (length (dom.children self)))
	(format stream "~A(~D)"
		(dom.tag self) (length (dom.children self))))))

(defun make-dom-element (tag ns attributes &rest children)
  "Returns a fresh instance of dom-element class, ctor"
  (make-instance 'dom-element
		 :tag tag
		 :namespace ns
		 :attributes attributes
		 :children (flatten children)))

(defun dom-successor (element)
  "Returns successors of dom 'element'"
  (if (typep element 'dom-element) (dom.children element)))

;;-----------------------------------------------------------------------------
;; DOM Parser
;;-----------------------------------------------------------------------------
(defrule dom-attribute-name? (c (attribute (make-accumulator)) namespace)
  (:oom (:or (:type alphanum? c)
	     (:and #\- (:do (setq c #\-))))
	(:collect c attribute))  
  (:optional
   #\:
   (:do (setq namespace attribute)
	(setq attribute (make-accumulator)))
   (:oom (:or (:type alphanum? c)
	      (:and #\- (:do (setq c #\-))))
	 (:collect c attribute)))
  (:return (if namespace
	       (format nil "~A:~A" namespace attribute)
	       attribute)))

(defrule dom-attribute-value? (c (val (make-accumulator)))
  (:or (:and
	#\"
	(:zom (:checkpoint #\" (:return val))
	(:checkpoint #\> (:rewind-return val))
	(:type (or visible-char? space?) c)
	(:collect c val)))
       (:and
	#\'
	(:zom (:checkpoint #\' (:return val))
	(:checkpoint #\> (:rewind-return val))
	(:type (or visible-char? space?) c)
	(:collect c val))))
  (:return val))

(defrule dom-attribute? (name value)
  (:dom-attribute-name? name)
  #\=
  (:dom-attribute-value? value)
  (:return (cons name value)))

(defrule dom-tag-name? (name)
  (:dom-attribute-name? name)
  (:return name))

(defrule dom-lwsp? (c)
  (:oom (:or (:and (:type (or space? tab?)))
	     (:and (:type (or carriage-return? linefeed?))
		   (:do (setf c t))))
  (:if c
       (:return #\Newline)
       (:return #\Space))))

(defrule dom-text-node? (c (acc (make-accumulator)))
  (:not #\<)
  (:oom (:checkpoint #\< (:rewind-return acc))
	(:or (:dom-lwsp? c)
	     (:type octet? c))
	(:collect c acc))
  (:if (> (length acc) 0)
       (:return acc)))

(defrule dom-cdata? (c (acc (make-accumulator)))
  (:seq "<![CDATA[")
  (:zom (:not (:seq "]]>"))
	(:type octet? c)
	(:collect c acc))
  (:return acc))

(defrule dom-comment? (c (acc (make-accumulator)))
  (:seq "<!--")
  (:collect #\< acc) (:collect #\! acc)
  (:collect #\- acc) (:collect #\- acc)
  (:zom (:not (:seq "-->"))
	(:type octet? c)
	(:collect c acc))
  (:collect #\- acc) (:collect #\- acc) (:collect #\> acc)
  (:return acc))

;;-----------------------------------------------------------------------------
;; Generic Document Parser
;;-----------------------------------------------------------------------------
(defparser dom-element? (tag namespace attr attrs child children)
  (:lwsp?)
  (:checkpoint (:seq "<?xml")
	       (:zom (:not #\>) (:type octet?)) (:lwsp?) (:commit))
  (:checkpoint (:seq "<!DOCTYPE")
	       (:zom (:not #\>) (:type octet?)) (:lwsp?) (:commit))  
  #\<
  (:dom-tag-name? tag namespace)
  (:zom (:lwsp?)
	(:dom-attribute? attr)
	(:do (push attr attrs)))
  (:or (:and (:lwsp?)
	     (:seq "/>")
	     (:return (make-dom-element tag namespace (nreverse attrs))))
       (:and #\>
	     (:zom (:lwsp?)
		   (:or (:dom-element? child)
			(:dom-comment? child)
			(:dom-text-node? child)
			(:dom-cdata? child))
		   (:do (push child children)))
	     (:seq "</")
	     (:if namespace
		  (:and (:sci namespace) #\: (:sci tag))
		  (:sci tag))
	     #\>
	     (:return (make-dom-element tag namespace (nreverse attrs) (nreverse children))))))

;;-----------------------------------------------------------------------------
;; Generic Document Render
;;-----------------------------------------------------------------------------
(defparameter +dom-indentation-increment+ 1)

(defmethod dom-element! ((stream core-stream) (element t)
			            &optional (indentation 0))
  (declare (ignore indentation))
  stream)

(defmethod dom-element! ((stream core-stream) (element string)
			            &optional (indentation 0))
  (declare (ignore indentation))
  (string! stream element))

(defmethod dom-element! ((stream core-stream) (element dom-element)
			            &optional (indentation 0))
  (flet ((indent ()
	   (dotimes (i indentation)
	     (char! stream #\Space)
	     (char! stream #\Space)))
	 (child! (child)
	   (if (stringp child)
	       (string! stream child)
	       (progn
		 (char! stream #\Newline)
		 (dom-element! stream child (+ indentation +dom-indentation-increment+))))))
    (prog1 stream
      (indent)
      (char! stream #\<)
      (string! stream (dom.tag element))
      (mapcar (lambda (attr)
		(char! stream #\Space)
		(string! stream (car attr))
		(char! stream #\=)
		(char! stream #\")
		(string! stream (cdr attr))
		(char! stream #\"))
	      (dom.attributes element))      
      (char! stream #\>)
      (cond
	((eq 0 (length (dom.children element)))
	 )
	((eq 1 (length (dom.children element)))
	 (mapcar #'child! (dom.children element)))
	(t	 
	 (mapcar #'child! (dom.children element))
	 (char! stream #\Newline)
	 (indent)))
      (string! stream "</")
      (string! stream (dom.tag element))
      (char! stream #\>))))

(defun dom2string (element)
  "Returns rendered string representation of 'element'"
  (with-core-stream (s "")
    (dom-element! s element)
    (return-stream s)))

(deftrace dom-parsers
  '(dom-element? dom-tag-name? dom-attribute? dom-attribute-value? dom-attribute-name? dom-text-node?
    dom-comment? make-dom-element dom-element!))

