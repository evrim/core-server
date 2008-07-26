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
;;| HTML 4 Library
;;+----------------------------------------------------------------------------
;;
;; This file contains implementation of W3C HTML 4 Standard.
;; http://www.w3.org/TR/html4/
;;

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defvar +html-attributes+
    '(:core (class id style title)
      :i18n (dir lang)
      :event (onclick ondblclick)))

  (defun make-html-attributes (attributes)  
    (nreverse
     (reduce (lambda (acc atom)
	       (if (keywordp atom)
		   (aif (getf +html-attributes+ atom)
			(append (reverse it) acc)
			(error "Attributes list ~A not found." atom))
		   (cons atom acc)))
	     attributes :initial-value nil)))
  
  (defun tag-attributes (arguments &optional acc key)
    (cond
      (key
       (tag-attributes (cdr arguments) (cons (car arguments) acc) nil))
      ((keywordp (car arguments))
       (tag-attributes (cdr arguments) (cons (car arguments) acc) t)) 
      (t
       (values (nreverse acc) arguments)))))

;;+----------------------------------------------------------------------------
;;| HTML Class
;;+----------------------------------------------------------------------------
(defclass html-element (dom-element)
  ()
  (:documentation "HTML Element Class (ie html)"))

(defclass empty-html-element (html-element)
  ()
  (:documentation "Empty HTML Element Class (ie img)"))

(defmethod dom-element! ((stream core-stream) (element empty-html-element)
			            &optional (indentation 0))
  (flet ((indent ()
	   (dotimes (i indentation)
	     (char! stream #\Space)
	     (char! stream #\Space))))
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
      (string! stream "/>"))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro defhtml-tag (name &rest attributes)
    "This macro is used to define HTML Element with 'attributes'"
    (let ((attributes (make-html-attributes attributes))
	  (symbol (intern (symbol-name name)
			  (find-package :tr.gen.core.server.html))))
      (export symbol (find-package :tr.gen.core.server.html))
      `(prog1 (defclass ,symbol (html-element)
		())
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
	 (eval-when (:load-toplevel :compile-toplevel :execute)
	   (export ',symbol (find-package :tr.gen.core.server.html))))))

  (defmacro defhtml-empty-tag (name &rest attributes)
    "This macro is used to define Empty HTML Element with 'attributes'"
    (let ((attributes (make-html-attributes attributes))
	  (symbol (intern (symbol-name name)
			  (find-package :tr.gen.core.server.html))))     
      (export symbol (find-package :tr.gen.core.server.html))
      `(prog1 (defclass ,symbol (empty-html-element)
		())
	 (defun ,symbol (&rest args)
	   (multiple-value-bind (attributes) (tag-attributes args)
	     (destructuring-bind (&key ,@attributes) attributes
	       (make-instance ',symbol
			      :tag ,(symbol-name name)
			      :attributes (remove-if (lambda (attr)
						       (if (null (cdr attr))
							   t))
						     (list ,@(mapcar (lambda (attr)
								       `(cons ,(symbol-to-js attr) ,attr))
								     attributes)))
			      :children nil))))
	 (eval-when (:load-toplevel :compile-toplevel :execute)
	   (export ',symbol (find-package :tr.gen.core.server.html)))))))

;;-----------------------------------------------------------------------------
;; HTML 4 Tag Definitions
;;-----------------------------------------------------------------------------
;;
;; See http://www.w3.org/TR/xhtml1/dtds.html for exact definitions.
;;
(defhtml-tag a :core :i18n :event accesskey charset coords href hreflang
	     name onblur onfocus rel rev shape tabindex target type onmouseover onmouseout)

(defmethod dom-element! ((stream core-stream) (element <:a)
			 &optional (indentation 0))
  (prog1 stream
    (if +context+
	(let ((uri (uri? (make-core-stream (get-attribute element "href")))))
	  (if (and uri (slot-boundp +context+ 'session))
	      (call-next-method
	       stream	     
	       (set-attribute element "href" 
			      (with-core-stream (s "")
				(uri! s (prog1 uri
					  (setf (uri.queries uri)
						(cons
						 (cons
						  (symbol-to-js +session-query-name+)
						  (session.id (context.session +context+)))		  
						 (uri.queries uri))
						(uri.paths uri)
						(cons
						 (list
						  (symbol-to-js
						   (web-application.fqdn
						    (context.application +context+))))
						 (uri.paths uri)))
					  ;; (describe uri)
					  ))
				(return-stream s)))
	       indentation)
	      (call-next-method)))
	(call-next-method))))

(defhtml-tag abbr :core :event :i18n)
(defhtml-tag acronym :core :event :i18n)
(defhtml-tag address :core :event :i18n onmouseover onmouseout)
(defhtml-empty-tag area :core :event :i18n alt accesskey coords href nohref
		   onblur onfocus shape tabindex onmouseover onmouseout)
(defhtml-tag b :core :event :i18n onmouseover onmouseout)
(defhtml-empty-tag base href)
(defhtml-tag bdo :i18n id style title onmouseover onmouseout)
(defhtml-tag big :core :event :i18n onmouseover onmouseout)
(defhtml-tag blockquote :core :event :i18n cite onmouseover onmouseout)
(defhtml-tag body :core :i18n :event onload onunload onmouseover onmouseout)
(defhtml-empty-tag br :core)
(defhtml-tag button :core :event :i18n accesskey disabled name onblur
	     onfocus tabindex type value onmouseover onmouseout)
(defhtml-tag caption :core :event :i18n onmouseover onmouseout)
(defhtml-tag cite :core :event :i18n onmouseover onmouseout)
(defhtml-tag code :core :event :i18n onmouseover onmouseout)
(defhtml-empty-tag col :core :event :i18n align char charoff span valign width)
(defhtml-tag colgroup :core :event :i18n align char charoff span valign width)
(defhtml-tag dd :core :event :i18n onmouseover onmouseout)
(defhtml-tag del :core :event :i18n cite datetime)
(defhtml-tag dfn :core :event :i18n onmouseover onmouseout)
(defhtml-tag div :core :event :i18n dojo-type open onmouseover onmouseout)
(defhtml-tag dl :core :event :i18n onmouseover onmouseout)
(defhtml-tag dt :core :event :i18n onmouseover onmouseout)
(defhtml-tag em :core :event :i18n onmouseover onmouseout)
(defhtml-tag fieldset :core :event :i18n onmouseover onmouseout)
(defhtml-tag form :core :event :i18n action accept-charset enctype method
	     name onreset onsubmit target onmouseover onmouseout)
(defhtml-empty-tag frame :core frameborder longdesc marginheight marginwidth
		   noresize scrolling src)
(defhtml-tag frameset :core cols onload olunload rows)
(defhtml-tag h1 :core :event :i18n onmouseover onmouseout)
(defhtml-tag h2 :core :event :i18n onmouseover onmouseout)
(defhtml-tag h3 :core :event :i18n onmouseover onmouseout)
(defhtml-tag h4 :core :event :i18n onmouseover onmouseout)
(defhtml-tag h5 :core :event :i18n onmouseover onmouseout)
(defhtml-tag h6 :core :event :i18n onmouseover onmouseout)
(defhtml-tag head :i18n profile)
(defhtml-empty-tag hr :core :event width align onmouseover onmouseout)

(defhtml-tag html dir lang)
(defmethod dom-element! ((stream core-stream) (element <:html) &optional (indentation 0))
  (string! stream "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\"")
  (char! stream #\Newline)
  (string! stream "   \"http://www.w3.org/TR/html4/frameset.dtd\">")
  (char! stream #\Newline)
  (call-next-method stream element indentation))

(defhtml-tag i :core :event :i18n onmouseover onmouseout)
(defhtml-tag iframe :core frameborder longdesc marginheight marginwidth name scrolling src)
(defhtml-empty-tag img :core :event :i18n alt src height ismap longdesc usemap width onmouseover onmouseout)
(defhtml-empty-tag input :core :event :i18n accept accesskey alt checked
		   disabled maxlength name onblur onchange onfocus
		   onselect readonly size src tabindex type usemap
		   value width height onmouseover onmouseout)
(defhtml-tag ins :core :event :i18n cite datetime)
(defhtml-tag kbd :core :event :i18n onmouseover onmouseout)
(defhtml-tag label :core :event :i18n accesskey for onblur onfocus onmouseover onmouseout)
(defhtml-tag legend :core :event :i18n accesskey onmouseover onmouseout)
(defhtml-tag li :core :event :i18n onmouseover onmouseout)
(defhtml-tag link :core :event :i18n charset href hreflang media rel rev type)
(defhtml-tag map :core :event :i18n name onmouseover onmouseout)
(defhtml-tag meta :i18n content http--equiv name scheme)
(defhtml-tag noframes :core :event :i18n)
(defhtml-tag noscript :core :event :i18n)
(defhtml-tag object :core :event :i18n archive classid codebase codetype
	     data declare height name standby tabindex type usemap
	     width align)
(defhtml-tag ol :core :event :i18n onmouseover onmouseout)
(defhtml-tag optgroup :core :event :i18n label disabled)
(defhtml-tag option :core :event :i18n disabled label selected value)
(defhtml-tag p :core :event :i18n onmouseover onmouseout)
(defhtml-empty-tag param name id type value valuetype)
(defhtml-tag pre :core :event :i18n onmouseover onmouseout)
(defhtml-tag q :core :event :i18n cite)
(defhtml-tag samp :core :event :i18n onmouseover onmouseout)
(defhtml-tag script type charset defer src title language)
(defhtml-tag select :core :event :i18n disabled multiple name accesskey
	     onblur onfocus onchange size tabindex onmouseover onmouseout)
(defhtml-tag small :core :event :i18n onmouseover onmouseout)
(defhtml-tag span :core :event :i18n onmouseover onmouseout)
(defhtml-tag strong :core :event :i18n onmouseover onmouseout)
(defhtml-tag style :i18n type media title)
(defhtml-tag sub :core :event :i18n onmouseover onmouseout)
(defhtml-tag sup :core :event :i18n onmouseover onmouseout)
(defhtml-tag table :core :event :i18n border cellpadding cellspacing
	     frame summary width onmouseover onmouseout)
(defhtml-tag tbody :core :event :i18n align char charoff valign onmouseover onmouseout)
(defhtml-tag td :core :event :i18n abbr align axis char charoff colspan
	     headers rowspan scope valign width onmouseover onmouseout)
(defhtml-tag textarea :core :event :i18n cols rows accesskey disables
	     name onblur onchange onfocus onselect readonly tabindex onmouseover onmouseout)
(defhtml-tag tfoot :core :event :i18n onmouseover onmouseout)
(defhtml-tag th :core :event :i18n abbr align axis char charoff colspan
	     headers rowspan scope valign onmouseover onmouseout)
(defhtml-tag thead :core :event :i18n align char charoff valign onmouseover onmouseout)
(defhtml-tag title :i18n)
(defhtml-tag tr :core :event :i18n align char charoff valign onmouseover onmouseout)
(defhtml-tag tt :core :event :i18n onmouseover onmouseout)
(defhtml-tag ul :core :event :i18n onmouseover onmouseout)
(defhtml-tag var :core :event :i18n onmouseover onmouseout)
(defhtml-tag embed src width height type quality bgcolor name align
	     allow-script-access pluginspage)

;;-----------------------------------------------------------------------------
;; Html Validator
;;-----------------------------------------------------------------------------
;;
;; TODO: Implement validator
;;
(defmethod validate-html ((dom-element t))
  (warn "Unknown Html Element: ~A" dom-element)
  dom-element)

(defmethod validate-html ((dom-element string))
  dom-element)

(defmethod validate-html ((dom-element dom-element))
  (aif (find-class (intern (string-upcase (dom.tag dom-element))
			   (find-package :tr.gen.core.server.html)) nil)
       (change-class dom-element it))
  (setf (dom.children dom-element)
	(mapcar #'validate-html (dom.children dom-element)))
  dom-element)

(defun html? (stream)
  "Returns associated html element parsing from 'stream'"
  (let ((dom (dom-element? stream)))
    (if dom (validate-html dom))))

(defmethod html! ((stream core-stream) element)
  (dom-element! stream element))

(defmacro with-html-output (stream &body body)
  "Renders html elements in 'body' to 'stream'"
  (with-unique-names (element)
    `(progn
       ,@(mapcar #'(lambda (elem)
		     `(let ((,element ,elem))
			(dom-element! ,stream ,element)))
		 body))))

;;-----------------------------------------------------------------------------
;; Html to Javascript Transformation
;;-----------------------------------------------------------------------------
(defgeneric dom2js (html-element)
  (:documentation "Html to Javascript transformator"))

(defmethod dom2js ((element t))
  nil)

(defmethod dom2js ((element html-element))  
  `((lambda ()
      (let ((elem (document.create-element ,(dom.tag element))))
	,@(mapcar (lambda (attr)
		    (if (equal (car attr) "class")			 
			`(setf (slot-value elem 'class-name) ,(cdr attr))
			`(setf (slot-value elem ,(car attr)) ,(cdr attr))))
		  (dom.attributes element))
	,@(mapcar (lambda (child)
		    (if (stringp child)
			`(elem.append-child (document.create-text-node ,child))
			`(elem.append-child ,(dom2js child))))
		  (dom.children element))
	(return elem)))))

(defun href (base &rest params)
  "Convenient function for generating hyperlinks"
  (with-output-to-string (href)
    (write-string base href)
    (when params
      (write-char #\? href)
      (loop
	 for (key value . rest) on params by #'cddr
	 do (etypecase key
              (string (write-string key href))
              (symbol (write-string (string-downcase key) href))) 
	 do (write-char #\= href)
	 do (princ value href)
	 when rest
	 do (write-char #\& href)))))

