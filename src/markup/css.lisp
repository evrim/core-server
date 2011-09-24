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

;;+--------------------------------------------------------------------------
;;| CSS 2 Library
;;+--------------------------------------------------------------------------
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
    (format stream "~A(~D)" (css.selector self)
	    (length (css.children self)))))

(defvar +css-current-selector+ nil)
(defmethod write-stream ((stream xml-stream) (element css-element))
  (with-slots (%stream) stream
    (when +css-current-selector+
      (string! %stream +css-current-selector+)
      (char! %stream #\Space))
    (string! %stream (css.selector element))
    (string! %stream " {")
    (char! %stream #\Newline)
    (mapc (lambda (atom)
	    (if (keywordp atom)
		(progn
		  (string! %stream (string-downcase (symbol-name atom)))
		  (string! %stream ": "))
		(progn
		  (write-stream %stream atom)
		  (char! %stream #\;)
		  (char! %stream #\Newline))))
	  (css.attributes element))
    (string! %stream "}")
    (char! %stream #\Newline)
    (char! %stream #\Newline)
    (when (css.children element)
      (let ((+css-current-selector+
	     (if +css-current-selector+
		 (format nil "~A ~A"
			 +css-current-selector+ (css.selector element))
		 (css.selector element))))
	(reduce #'write-stream
		(css.children element) :initial-value stream)))
    stream))

(defun css (&rest args)
  (multiple-value-bind (attributes children) (tag-attributes (cdr args))
    (make-instance 'css-element
		   :selector (car args)
		   :attributes attributes
		   :children (flatten children))))

(defmethod css! ((stream core-stream) (element css-element))
  (write-stream stream element))


;;+----------------------------------------------------------------------------
;;| css 2 Query via Selectors
;;+----------------------------------------------------------------------------
;; http://www.w3.org/TR/REC-CSS2/selector.html
;;

;;-----------------------------------------------------------------------------
;; 5.1 Pattern matching
;;-----------------------------------------------------------------------------
;; Pattern	        Meaning	Described in section
;;-----------------------------------------------------------------------------
;; (defatom css-value-type ()
;;   (or (alphanum? c) (eq c #.(char-code #\-))))

;; (defparser css-value? (c (acc (make-accumulator)))
;;   (:oom (:type css-value-type c)
;; 	(:collect c acc))
;;   (:return acc))

;; ;; *	                Matches any element.
;; (defparser css-universal-selector? ()
;;   #\* (:return (cons 'universal-selector)))

;; ;; E                    Matches any E element (i.e., an element of type E). 
;; (defparser css-type-selector? (val)
;;   (:css-value? val)
;;   (:return (list 'type-selector val)))

;; ;; DELETE me
;; (defparser css-selector? (val)
;;   (:css-type-selector? val)
;;   (:return val))

;; ;; E F                  Matches any F element that is a descendant of an E
;; ;;                      element.
;; (defparser css-descendant-selector? (a b)
;;   (:css-selector? a) (:lwsp?) (:css-selector? b)
;;   (:return (list 'descendant-selector a b)))

;; ;; E > F                Matches any F element that is a child of an element E.
;; (defparser css-child-selector? (a b)
;;   (:css-type-selector? a) (:lwsp?) #\> (:lwsp?) (:css-type-selector? b)
;;   (:return (list 'child-selector a b)))

;; ;; E:first-child	Matches element E when E is the first child of its
;; ;;                      parent.
;; (defparser css-first-child-selector? (a)
;;   (:css-type-selector? a) (:sci ":first-child")
;;   (:return (list 'first-child-selector a)))

;; ;; E:link               Matches element E if E is the source anchor of a
;; ;; E:visited            hyperlink of which the target is not yet visited
;; ;;                      (:link) or already visited (:visited).
;; (defparser css-link-selector? (a type)
;;   (:css-type-selector? a) (:or (:and (:sci ":link") (:do (setq type 'link)))
;; 			       (:and (:sci ":visited") (:do (setq type 'visited))))
;;   (:return (list 'link-selector a type)))

;; ;; E:active             Matches E during certain user actions.  The dynamic
;; ;; E:hover              pseudo-classes
;; ;; E:focus
;; (defparser css-dynamic-selector? (a)
;;   (:css-type-selector? a)
;;   (:or (:sci ":active") (:sci ":hover") (:sci ":focus"))
;;   (:return (list 'dynamic-selector a)))

;; ;; E:lang(c)            Matches element of type E if it is in (human) language
;; ;;                      c (the document language specifies how language is
;; ;;                      determined).
;; (defparser css-lang-selector? (a c (acc (make-accumulator)))
;;   (:css-type-selector? a)
;;   (:sci ":lang(")
;;   (:oom (:not #\))
;; 	(:type octet? c)
;; 	(:collect c acc))
;;   (:return (list 'lang-selector a acc)))

;; ;; E + F                Matches any F element immediately preceded by an
;; ;;                      element E
;; (defparser css-adjacent-selector? (a b)
;;   (:css-type-selector? a) (:lwsp?) #\+ (:lwsp?) (:css-type-selector? b)
;;   (:return (list 'adjacent-selector a b)))

;; ;; E[foo]               Matches any E element with the "foo" attribute set
;; ;;                      (whatever the value).
;; ;; E[foo="warning"]     Matches any E element whose "foo" attribute value
;; ;;                      is exactly equal to "warning".
;; ;; E[foo~="warning"]    Matches any E element whose "foo" attribute value
;; ;;                      is a list of space-separated values, one of which
;; ;;                      is exactly equal to "warning".
;; ;; E[lang|="en"]        Matches any E element whose "lang" attribute has a
;; ;;                      hyphen-separated list of values beginning
;; ;;                      (from the left) with "en".
;; (defparser css-attribute-selector? (c type attrs (attr (make-accumulator))
;; 				     operator value)
;;   (:css-type-selector? type)
;;   (:oom #\[
;; 	(:oom (:type css-value-type c)
;; 	      (:collect c attr))
;; 	(:optional
;; 	 (:or (:and #\= (:quoted? value) (:do (setq operator 'equal)))
;; 	      (:and (:seq "~=") (:quoted? value) (:do (setq operator 'space-member)))
;; 	      (:and (:seq "|=") (:quoted? value) (:do (setq operator 'hypen-member)))))
;; 	#\]
;; 	(:do (push (list (or operator 'exists) attr value) attrs)
;; 	     (setf attr (make-accumulator)
;; 		   operator nil
;; 		   value nil)))
;;   (:return (list 'attribute-selector type (nreverse attrs))))

;; ;; DIV.warning	        HTML only. The same as DIV[class~="warning"].
;; (defparser css-class-selector? (type class)
;;   (:css-type-selector? type) #\.
;;   (:css-value? class)
;;   (:return (list 'attribute-selector type (list (list 'equal "class" class)))))

;; ;; E#myid        	Matches any E element ID equal to "myid".
;; (defparser css-id-selector? (type id)
;;   (:or (:and (:css-type-selector? type) #\# (:css-value? id))
;;        (:and #\# (:css-value? id) (:do (setq type "*"))))
;;   (:return (list 'id-selector type id)))

;; ;; 5.2 Selector syntax

;; ;; A simple selector is either a type selector or universal selector
;; ;; followed immediately by zero or more attribute selectors, ID
;; ;; selectors, or pseudo-classes, in any order. The simple selector
;; ;; matches if all of its components match.
;; (defparser css-simple-selector? (c selectors)
;;   (:or (:css-type-selector? c)
;;        (:css-universal-selector? c))
;;   (:do (push c selectors))
;;   (:lwsp?)
;;   (:zom (:or (:css-attribute-selector? c)
;; 	     (:css-id-selector? c)
;; 	     (:css-pseudo-selector? c))
;; 	(:do (push c selectors))
;; 	(:lwsp?))
;;   (:return selectors))

;; ;; A selector is a chain of one or more simple selectors separated by
;; ;; combinators. Combinators are: whitespace, ">", and "+". Whitespace
;; ;; may appear between a combinator and the simple selectors around it.
;; (defatom css-combinator ()
;;   (or (= c #.(char-code #\>))
;;       (= c #.(char-code #\+))
;;       (= c #.(char-code #\Space))))

;; (defparser css-selector? (a b combinator)
;;   (:zom (:css-simple-selector? a)
;; 	(:lwsp?)
;; 	(:do (setq combinator #\Space))
;; 	(:checkpoint
;; 	 (:or (:and #\> (:lwsp?) (:do (setq combinator #\>)))
;; 	      (:and #\+ (:lwsp?) (:do (setq combinator #\+))))
;; 	 (:commit))
;; 	(:css-simple-selector? b)))

;; The elements of the document tree that match a selector are called
;; subjects of the selector. A selector consisting of a single simple
;; selector matches any element satisfying its
;; requirements. Prepending a simple selector and combinator to a
;; chain imposes additional matching constraints, so the subjects of a
;; selector are always a subset of the elements matching the rightmost
;; simple selector.

;; One pseudo-element may be appended to the last simple selector in a
;; chain, in which case the style information applies to a subpart of
;; each subject.

;; Query DOM Structures via CSS Selectors
;; (defmethod dom.query ((element dom-element) (query string))
;;   (let ((targets (css-query? query)))
;;     (dom.query element targets)))


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
