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
;;| CSS 2 Library
;;+----------------------------------------------------------------------------
;;
;; This file contains implementation of W3C CSS 2 Standard.
;; http://www.w3.org/TR/CSS21/
;;
(defclass css-element ()
  ((selector :accessor css.selector :initform nil :initarg :selector)
   (attributes :accessor css.attributes :initform nil :initarg :attributes)
   (children :accessor css.children :initform nil :initarg :children))
  (:documentation "CSS Element Base Class"))

(defmethod print-object ((self css-element) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~A(~D)" (css.selector self) (length (css.children self)))))

(defvar +css-current-selector+ nil)
(defmethod dom-element! ((stream core-stream) (element css-element)
			            &optional (indentation 0))  
  (when +css-current-selector+
    (string! stream +css-current-selector+)
    (char! stream #\Space))
  (string! stream (css.selector element))
  (string! stream " {")
  (char! stream #\Newline)
  (mapc (lambda (atom)
	  (if (keywordp atom)
	      (progn
		(string! stream (string-downcase (symbol-name atom)))
		(string! stream ": "))
	      (progn
		(dom-element! stream atom indentation)
		(char! stream #\;)
		(char! stream #\Newline))))
	(css.attributes element))
  (string! stream "};")
  (char! stream #\Newline)
  (char! stream #\Newline)
  (when (css.children element)
    (let ((+css-current-selector+ (if +css-current-selector+
				      (format nil "~A ~A"
					      +css-current-selector+ (css.selector element))
				      (css.selector element))))
      (reduce (rcurry #'dom-element! indentation)
	      (css.children element) :initial-value stream)))
  stream)

(defun css (&rest args)
  (multiple-value-bind (attributes children) (tag-attributes (cdr args))
    (make-instance 'css-element
		   :selector (car args)
		   :attributes attributes
		   :children (nreverse (flatten children)))))

;; (defparser css? (c (selector (make-accumulator))
;; 		   attributes
;; 		   (acc (make-accumulator)))
;;   (:lwsp?)
;;   (:zom ))

;; (defmacro <:media (media &body body)
;;   `(<:ah "@media " ,media " {" ~%
;;          ,@body "}" ~%))

;; Test
;; (<:css ("div.abc" "div.def")
;;        :def "gef"
;;        :color "#000"
;;        (<:css ("a:hover" "a:active" "a:link")
;;            :abc "def"))

;; div.abc, dv.def {
;;  def: gef;
;;  color: #000;
;; };

;; div.abc a:hover, dv.def a:hover, div.abc a:active, dv.def a:active, div.abc a:link, dv.def a:link {
;;  abc: def;
;; };
