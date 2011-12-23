;;+--------------------------------------------------------------------------
;;| HTML 4 Library
;;+--------------------------------------------------------------------------
;; This file contains implementation of W3C HTML 4 Standard.
;; http://www.w3.org/TR/html4/
(in-package :core-server)

;; --------------------------------------------------------------------------
;; Helpers needed while defining HTML Objects
;; --------------------------------------------------------------------------
(eval-when (:compile-toplevel :execute :load-toplevel)
  (defvar +html-attributes+
    '(:core (class id style title)
      :i18n (dir lang)
      :event (onclick ondblclick onmousedown onmouseup onmouseover
	      onmousemove onmouseout onkeypress onkeydown onkeyup)))

  (defun make-html-attributes (attributes)
    (let ((attrs))
      (mapcar (lambda (attr) (pushnew attr attrs))	      
	      (reduce (lambda (acc atom)
			(if (keywordp atom)
			    (aif (getf +html-attributes+ atom)
				 (append (reverse it) acc)
				 (error "Attributes list ~A not found."
					atom))
			    (cons atom acc)))
		      attributes :initial-value nil))
      attrs))
  
  (defun tag-attributes (arguments &optional acc key)
    (cond
      (key
       (tag-attributes (cdr arguments) (cons (car arguments) acc) nil))
      ((keywordp (car arguments))
       (tag-attributes (cdr arguments) (cons (car arguments) acc) t)) 
      (t
       (values (nreverse acc) arguments)))))

;; +----------------------------------------------------------------------------
;; | HTML Metaclass
;; +----------------------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass html+ (xml+)
    ()))

(defmacro defhtml-javascript-tag (name &rest attributes)
  (let* ((attributes (make-html-attributes attributes))
	 (tag (symbol-to-js name)))
    `(defjsmacro ,name (&rest args)
       (multiple-value-bind (attributes children) (tag-attributes args)
	 (destructuring-bind (&key ,@attributes) attributes
	   (declare (ignorable ,@attributes))
	   `(make-dom-element ,',tag
	      (jobject
	       ,@(reduce0
		  (lambda (acc attr)
		    (if (cdr attr)
			(append
			 (cond
			   ((eq (car attr) 'class)
			    `(:class-name ,(cdr attr)))
			   ((eq (car attr) 'colspan)
			    `(:col-span ,(cdr attr)
					:colspan ,(cdr attr)))
			   (t
			    `(,(make-keyword (car attr)) ,(cdr attr))))
				acc)
			acc))
		  (list ,@(mapcar (lambda (attr) `(cons ',attr ,attr))
				  attributes))))
	      (array ,@children)))))))

;; +------------------------------------------------------------------------
;; | HTML Object Definition: defhtml
;; +------------------------------------------------------------------------
(defmacro defhtml (name supers &rest attributes)  
  `(progn
     (defclass+ ,name (,@supers xml)
       (,@(mapcar (lambda (attr) (list attr :print nil :host 'remote))
		  (remove 'id attributes)))
       (:metaclass html+)
       (:tag ,@(string-downcase (symbol-name name)))
       (:namespace ,@"html")
       (:attributes ,@attributes))
     (defhtml-javascript-tag ,name ,@attributes)
     (find-class+ ',name)))

;;+--------------------------------------------------------------------------
;;| HTML Class
;;+--------------------------------------------------------------------------
(defclass+ html-element (xml)
  ((id :host both))
  (:documentation "HTML Element Class (ie html)")
  (:metaclass html+))

(defclass+ empty-html-element (html-element)
  ()
  (:documentation "Empty HTML Element Class (ie img)")
  (:metaclass html+))

(defmacro defhtml-tag (name &rest attributes)
  `(defhtml ,name (html-element) ,@(make-html-attributes attributes)))

(defmacro defhtml-empty-tag (name &rest attributes)
  `(defhtml ,name (empty-html-element) ,@(make-html-attributes attributes)))

;;---------------------------------------------------------------------------
;; HTML 4 Tag Definitions
;;---------------------------------------------------------------------------
;;
;; See http://www.w3.org/TR/xhtml1/dtds.html for exact definitions.
;;
(defhtml-tag <:a :core :i18n :event accesskey charset coords href
	     hreflang name onblur onfocus rel rev shape tabindex
	     target type onmouseover onmouseout)
(defhtml-tag <:abbr :core :event :i18n)
(defhtml-tag <:acronym :core :event :i18n)
(defhtml-tag <:address :core :event :i18n onmouseover onmouseout)
(defhtml-empty-tag <:area :core :event :i18n alt accesskey coords href
		   nohref onblur onfocus shape tabindex onmouseover
		   onmouseout)
(defhtml-tag <:b :core :event :i18n onmouseover onmouseout)
(defhtml-empty-tag <:base href)
(defhtml-tag <:bdo :i18n id style title onmouseover onmouseout)
(defhtml-tag <:big :core :event :i18n onmouseover onmouseout)
(defhtml-tag <:blockquote :core :event :i18n cite onmouseover onmouseout)
(defhtml-tag <:body :core :i18n :event onload onunload onmouseover
	     onmouseout)
(defhtml-empty-tag <:br :core)
(defhtml-tag <:button :core :event :i18n accesskey disabled name onblur
	     onfocus tabindex type value onmouseover onmouseout)
(defhtml-tag <:caption :core :event :i18n onmouseover onmouseout)
(defhtml-tag <:cite :core :event :i18n onmouseover onmouseout)
(defhtml-tag <:code :core :event :i18n onmouseover onmouseout)
(defhtml-empty-tag <:col :core :event :i18n align char charoff span
		   valign width)
(defhtml-tag <:colgroup :core :event :i18n align char charoff span
	     valign width)
(defhtml-tag <:dd :core :event :i18n onmouseover onmouseout)
(defhtml-tag <:del :core :event :i18n cite datetime)
(defhtml-tag <:dfn :core :event :i18n onmouseover onmouseout)
(defhtml-tag <:div :core :event :i18n open onmouseover onmouseout)
(defhtml-tag <:dl :core :event :i18n onmouseover onmouseout)
(defhtml-tag <:dt :core :event :i18n onmouseover onmouseout)
(defhtml-tag <:em :core :event :i18n onmouseover onmouseout)
(defhtml-tag <:fieldset :core :event :i18n onmouseover onmouseout)
(defhtml-tag <:form :core :event :i18n action accept-charset enctype method
	     name onreset onsubmit submit target onmouseover onmouseout)
(defhtml-empty-tag <:frame :core frameborder longdesc marginheight
		   marginwidth noresize scrolling src)
(defhtml-tag <:frameset :core cols onload olunload rows)
(defhtml-tag <:h1 :core :event :i18n onmouseover onmouseout)
(defhtml-tag <:h2 :core :event :i18n onmouseover onmouseout)
(defhtml-tag <:h3 :core :event :i18n onmouseover onmouseout)
(defhtml-tag <:h4 :core :event :i18n onmouseover onmouseout)
(defhtml-tag <:h5 :core :event :i18n onmouseover onmouseout)
(defhtml-tag <:h6 :core :event :i18n onmouseover onmouseout)
(defhtml-tag <:head :i18n profile)
(defhtml-empty-tag <:hr :core :event width align onmouseover onmouseout)
(defhtml-tag <:html dir lang)
(defhtml-tag <:i :core :event :i18n onmouseover onmouseout)
(defhtml-tag <:iframe :core frameborder longdesc marginheight
	     marginwidth name scrolling src allowtransparency)
(defhtml-empty-tag <:img :core :event :i18n alt src height ismap
		   longdesc usemap width onmouseover onmouseout)
(defhtml-empty-tag <:input :core :event :i18n accept accesskey alt checked
		   disabled maxlength name onblur onchange onfocus
		   onselect readonly size src tabindex type usemap
		   value width height onmouseover onmouseout)
(defhtml-tag <:ins :core :event :i18n cite datetime)
(defhtml-tag <:kbd :core :event :i18n onmouseover onmouseout)
(defhtml-tag <:label :core :event :i18n accesskey for onblur onfocus
	     onmouseover onmouseout)
(defhtml-tag <:legend :core :event :i18n accesskey onmouseover onmouseout)
(defhtml-tag <:li :core :event :i18n onmouseover onmouseout)
(defhtml-tag <:link :core :event :i18n charset href hreflang media rel
	     rev type)
(defhtml-tag <:map :core :event :i18n name onmouseover onmouseout)
(defhtml-tag <:meta :i18n content http--equiv name scheme)
(defhtml-tag <:noframes :core :event :i18n)
(defhtml-tag <:noscript :core :event :i18n)
(defhtml-tag <:object :core :event :i18n archive classid codebase codetype
	     data declare height name standby tabindex type usemap
	     width align)
(defhtml-tag <:ol :core :event :i18n onmouseover onmouseout)
(defhtml-tag <:optgroup :core :event :i18n label disabled)
(defhtml-tag <:option :core :event :i18n disabled label selected value)
(defhtml-tag <:p :core :event :i18n onmouseover onmouseout)
(defhtml-empty-tag <:param name id type value valuetype)
(defhtml-tag <:pre :core :event :i18n onmouseover onmouseout)
(defhtml-tag <:q :core :event :i18n cite)
(defhtml-tag <:samp :core :event :i18n onmouseover onmouseout)
(defhtml-tag <:script onload onreadystate type charset defer src title
	     language)
(defhtml-tag <:select :core :event :i18n disabled multiple name accesskey
	     onblur onfocus onchange size tabindex onmouseover onmouseout)
(defhtml-tag <:small :core :event :i18n onmouseover onmouseout)
(defhtml-tag <:span :core :event :i18n onmouseover onmouseout)
(defhtml-tag <:strike :core :event :i18n onmouseover onmouseout)
(defhtml-tag <:strong :core :event :i18n onmouseover onmouseout)
(defhtml-tag <:style :i18n type media title)
(defhtml-tag <:sub :core :event :i18n onmouseover onmouseout)
(defhtml-tag <:sup :core :event :i18n onmouseover onmouseout)
(defhtml-tag <:table :core :event :i18n border cellpadding cellspacing
	     frame summary width onmouseover onmouseout)
(defhtml-tag <:tbody :core :event :i18n align char charoff valign
	     onmouseover onmouseout)
(defhtml-tag <:td :core :event :i18n abbr align axis char charoff colspan
	     headers rowspan scope valign width onmouseover onmouseout)
(defhtml-tag <:textarea :core :event :i18n cols rows accesskey
	     disables name onblur onchange onfocus onselect readonly
	     tabindex onmouseover onmouseout)
(defhtml-tag <:tfoot :core :event :i18n onmouseover onmouseout)
(defhtml-tag <:th :core :event :i18n abbr align axis char charoff colspan
	     headers rowspan scope valign onmouseover onmouseout)
(defhtml-tag <:thead :core :event :i18n align char charoff valign
	     onmouseover onmouseout)
(defhtml-tag <:title :i18n)
(defhtml-tag <:tr :core :event :i18n align char charoff valign
	     onmouseover onmouseout)
(defhtml-tag <:tt :core :event :i18n onmouseover onmouseout)
(defhtml-tag <:u :core :event :i18n onmouseover onmouseout)
(defhtml-tag <:ul :core :event :i18n onmouseover onmouseout)
(defhtml-tag <:var :core :event :i18n onmouseover onmouseout)
(defhtml-tag <:embed src width height type quality bgcolor name align
	     allow-script-access pluginspage)

;; --------------------------------------------------------------------------
;; HTML Stream
;; --------------------------------------------------------------------------
(defclass html-stream (relaxed-xml-stream)
  ()
  (:default-initargs :namespace "html"))

(defun make-html-stream (stream)
  (make-instance 'html-stream :stream stream))

(defun parse-html (text)
  (read-stream (make-html-stream
		(make-core-stream (format nil "<div>~A</div>" text)))))

(defmethod write-stream ((stream html-stream) (element <:html))
  (prog1 stream
    (let ((%stream (slot-value stream '%stream)))
      (string! %stream "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\"")
      (char! %stream #\Newline)
      (string! %stream "   \"http://www.w3.org/TR/html4/frameset.dtd\">")
      (char! %stream #\Newline)
      (call-next-method stream element))))

(defmethod write-stream ((stream html-stream) (element string))
  (write-stream (slot-value stream '%stream) element))

(defmethod write-stream ((stream html-stream) (element empty-html-element))
  (prog1 stream
    (let ((%stream (slot-value stream '%stream)))
      (char! %stream #\<)
      (string! %stream (xml.tag element))
      (mapcar (lambda (attr)
		(let ((value (slot-value element attr)))
		  (when value
		    (char! %stream #\Space)
		    (string! %stream (symbol-to-js attr))
		    (char! %stream #\=)
		    (char! %stream #\")
		    (write-stream %stream (slot-value element attr))
		    (char! %stream #\"))))
	      (xml.attributes element))
      (string! %stream ">")
      (reduce #'write-stream
	      (filter (lambda (a) (and (typep a '<:script) a))
		      (slot-value element 'children))
	      :initial-value stream))))

;;--------------------------------------------------------------------------
;; HTML 4 Extra Tags a.k.a. Bonuses
;;--------------------------------------------------------------------------

;; Inline Images having src as a rfc 2046 toplevel media
;;--------------------------------------------------------------------------
;; (<:img :media *a-toplevel-media-from-rfc2046*)
(defmethod write-stream ((stream core-string-io-stream)
			 (media top-level-media))
  (prog1 stream
    (string! stream "data:")
    (string! stream (format nil "~{~A~^/~}" (mime.content-type media)))
    (string! stream ";base64,")
    (base64! stream (mime.data media))))

(defmacro with-html-output (stream &body body)
  "Renders dom elements in 'body' to 'stream'"
  (with-unique-names (element %stream)
    `(let ((,%stream (if (typep ,stream 'xml-stream)
			 ,stream
			 (make-html-stream ,stream))))
       (let ((,element (progn ,@body)))
	 (write-stream ,%stream ,element)))))

;;---------------------------------------------------------------------------
;; Html to Javascript Transformation
;;---------------------------------------------------------------------------
(defgeneric dom2js (html-element)
  (:documentation "Html to Javascript transformator"))

(defmethod dom2js ((element t))
  nil)

(defmethod dom2js ((element xml))  
  `((lambda ()
      (let ((elem (document.create-element ,(xml.tag element))))
	,@(reduce (lambda (acc attr)
		    (let ((value (slot-value element attr)))
		      (if value
			  (cons
			   (cond
			     ((eq attr 'class)			 
			      `(setf (slot-value elem 'class-name) ,value))
			     ((eq attr 'style)
			      `(progn
				 (.set-attribute elem "style" ,value)
				 (setf (slot-value (slot-value elem 'style)
						   'css-text)
				       ,value)))
			     (t
			      `(setf (slot-value elem ',attr) ,value)))
			   acc)
			  acc)))
		  (xml.attributes element) :initial-value nil)
	,@(mapcar (lambda (child)
		    (if (stringp child)
			`(elem.append-child
			  (document.create-text-node ,child))
			`(elem.append-child ,(dom2js child))))
		  (xml.children element))
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

;; -------------------------------------------------------------------------
;; Safe Html Stream
;; -------------------------------------------------------------------------
;; This is used to process html provided by client.  Some security
;; measures are taken like, script, img tags are removed, also links
;; are processed to be <:a tags.
(defclass safe-html-stream (html-stream)
  ())

(defun make-safe-html-stream (stream)
  (make-instance 'safe-html-stream :stream stream))

(defun fix-hacker-tags (dom-node)
  (filter-xml-nodes dom-node
		    (lambda (node)
		      (or (equal (xml.tag node) "script")
			  (equal (xml.tag node) "img")
			  ;; (equal (xml.tag node) "a")
			  ))))

(defparser extract-links (lst c acc1 (acc (make-accumulator :byte)))
  (:zom (:or (:and (:seq "http://")
		   (:do (push (octets-to-string acc :utf-8) lst)
			(setq acc1 (make-accumulator :byte)
			      acc (make-accumulator :byte)))
		   (:oom (:not (:or #\Space #\Newline #\Linefeed))
			 (:type octet? c)
			 (:collect c acc1)) 
		   (:do (let ((a (format nil "http://~A"
					 (octets-to-string acc1 :utf-8))))
			  (push (<:a :href a :target "_blank" a)
				lst))))
	     (:and (:seq "www.")
		   (:do (push (octets-to-string acc :utf-8) lst)
			(setq acc1 (make-accumulator :byte)
			      acc (make-accumulator :byte)))
		   (:oom (:not (:or #\Space #\Newline #\Linefeed))
			 (:type octet? c)
			 (:collect c acc1))
		   (:do (let ((a (format nil "http://www.~A"
					 (octets-to-string acc1 :utf-8))))
			  (push (<:a :href a :target "_blank" a)
				lst))))
	     (:and (:type octet? c) (:collect c acc))))
  (:do (push (octets-to-string acc :utf-8) lst))
  (:return (remove "" (nreverse lst) :test #'equal)))

(defun make-links (root)
  (cond
    ((stringp root)
     (extract-links (make-core-stream root)))
    ((typep root '<:a)
     (setf (slot-value root 'core-server::target) "_blank")
     root)
    (t
     (let ((ctor (core-server::class+.ctor-name (class-of root))))
       (apply ctor
	      (append (reduce0
		       (lambda (acc attribute)
			 (aif (xml.attribute root attribute)
			      (cons (make-keyword attribute)
				    (cons it acc))
			      acc))
		       (xml.attributes root))
		      (reduce0 (lambda (acc child)
				 (append acc
					 (ensure-list (make-links child))))
			       (xml.children root))))))))

(defmethod read-stream ((self safe-html-stream))
  (let ((a (call-next-method self)))
    (if a
	(make-links (fix-hacker-tags a)))))

(defun parse-safe-html (text)
  (read-stream (make-safe-html-stream
		(make-core-stream (format nil "<div>~A</div>" text)))))

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
