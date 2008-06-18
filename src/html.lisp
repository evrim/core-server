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

(defun href (base &rest params)
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

(defclass dom-element ()
  ((tag :accessor tag :initarg :tag :initform nil)
   (attributes :accessor attributes :initarg :attributes :initform nil)
   (children :accessor children :initarg :children :initform nil)))

(defmethod print-object ((self dom-element) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "tag:~A" (tag self))))

(defun make-dom-element (tag attributes &rest children)
  (make-instance 'dom-element
		 :tag tag
		 :attributes attributes
		 :children (reverse (flatten children))))

(defun dom-successor (element)
  (if (typep element 'dom-element) (children element)))

(defrule attribute-name? (c (acc (make-accumulator)))
  (:oom (:or (:type alphanum? c)
	     (:and #\- (:do (setq c #\-))))
	(:collect c acc))
  (:return acc))

(defrule attribute-value? (c (val (make-accumulator)))
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

(defrule attribute? (name value)
  (:attribute-name? name)
  #\=
  (:attribute-value? value)
  (:return (cons name value)))

(defrule tag-name? (name)
  (:attribute-name? name) (:return name))

(defrule html-lwsp? (c)
  (:oom (:or (:and (:type (or space? tab?)))
	     (:and (:type (or carriage-return? linefeed?))
		   (:do (setf c t))))
  (:if c
       (:return #\Newline)
       (:return #\Space))))

(defrule text-node? (c (acc (make-accumulator)))
  (:not #\<)
  (:oom (:checkpoint #\< (:rewind-return acc))
	(:or (:html-lwsp? c)
	     (:type octet? c))
	(:collect c acc))
  (:if (> (length acc) 0)
       (:return acc)))

(defrule html-comment? (c (acc (make-accumulator)))
  (:seq "<!--")
  (:collect #\< acc) (:collect #\! acc)
  (:collect #\- acc) (:collect #\- acc)
  (:zom (:not (:seq "-->"))
	(:type octet? c)
	(:collect c acc))
  (:collect #\- acc) (:collect #\- acc) (:collect #\> acc)
  (:return acc))

(defparser dom-element? (tag attr attrs child children)
  (:lwsp?)
  (:checkpoint (:seq "<?xml")
	       (:zom (:not #\>) (:type octet?)) (:lwsp?) (:commit))
  (:checkpoint (:seq "<!DOCTYPE")
	       (:zom (:not #\>) (:type octet?)) (:lwsp?) (:commit))  
  #\<
  (:tag-name? tag)
  (:zom (:lwsp?)
	(:attribute? attr)
	(:do (push attr attrs)))
  (:or (:and (:lwsp?) (:seq "/>") (:return (make-dom-element tag (nreverse attrs))))
       (:and #\>
	     (:zom (:lwsp?)
		   (:or (:dom-element? child) (:html-comment? child) (:text-node? child))
		   (:do (push child children)))
	     (:seq "</")
	     (:seq tag)
	     #\>
	     (:return (apply #'make-dom-element tag (nreverse attrs) (nreverse children))))))

(defparameter *dom-parsers*
  '(dom-element? tag-name? attribute? attribute-value? attribute-name? text-node?
    html-comment? make-dom-element dom-element!))

(defun trace-dom-parsers () (mapcar (lambda (fun) (eval `(trace ,fun))) *dom-parsers*))

(defun untrace-dom-parsers () (mapcar (lambda (fun) (eval `(untrace ,fun))) *dom-parsers*))

(defparameter +indentation-increment+ 1)

(defmethod dom-element! ((stream core-stream) (element t) &optional (indentation 0))
  (declare (ignore indentation))
;;  (break element)
  element)

(defmethod dom-element! ((stream core-stream) (element string) &optional (indentation 0))
  (declare (ignore indentation))
  (string! stream element))

(defmethod dom-element! ((stream core-stream) (element dom-element) &optional (indentation 0))
  (flet ((indent ()
	   (dotimes (i indentation)
	     (char! stream #\Space)
	     (char! stream #\Space)))
	 (child! (child)
	   (if (stringp child)
	       (string! stream child)
	       (progn
		 (char! stream #\Newline)
		 (dom-element! stream child (+ indentation +indentation-increment+))))))
    (prog1 stream
      (indent)
      (char! stream #\<)
      (string! stream (tag element))
      (mapcar (lambda (attr)
		(char! stream #\Space)
		(string! stream (car attr))
		(char! stream #\=)
		(char! stream #\")
		(string! stream (cdr attr))
		(char! stream #\"))
	      (attributes element))      
      (char! stream #\>)
      (cond
	((eq 0 (length (children element)))
	 )
	((eq 1 (length (children element)))
	 (mapcar #'child! (children element)))
	(t	 
	 (mapcar #'child! (children element))
	 (char! stream #\Newline)
	 (indent)))
      (string! stream "</")
      (string! stream (tag element))
      (char! stream #\>))))

(defun dom2string (element)
  (with-core-stream (s "")
    (dom-element! s element)
    (return-stream s)))

(defclass empty-dom-element (dom-element)
  ())

(defmethod dom-element! ((stream core-stream) (element empty-dom-element) &optional (indentation 0))
  (flet ((indent ()
	   (dotimes (i indentation)
	     (char! stream #\Space)
	     (char! stream #\Space))))
    (prog1 stream
      (indent)
      (char! stream #\<)
      (string! stream (tag element))
      (mapcar (lambda (attr)
		(char! stream #\Space)
		(string! stream (car attr))
		(char! stream #\=)
		(char! stream #\")
		(string! stream (cdr attr))
		(char! stream #\"))
	      (attributes element))
      (string! stream "/>"))))

;; TODO: Implement validator
(defmethod validate-dom-tree ((dom-element t))
  dom-element)

(defmethod validate-dom-tree ((dom-element dom-element))
  (aif (find-class (intern (string-upcase (tag dom-element))
			   (find-package :tr.gen.core.server.html)) nil)
       (change-class dom-element it))
  (setf (children dom-element) (mapcar #'validate-dom-tree (children dom-element)))
  dom-element)

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defvar +html-attributes+
    '(:core (class id style title)
      :i18n (dir lang)
      :event (onclick ondblclick)))

  (defun make-attributes (attributes)  
    (nreverse
     (reduce (lambda (acc atom)
	       (if (keywordp atom)
		   (aif (getf +html-attributes+ atom)
			(append (reverse it) acc)
			(error "Attributes list ~A not found." atom))
		   (cons atom acc)))
	     attributes :initial-value nil))))

(defun tag-attributes (arguments &optional acc key)
  (cond
    (key
     (tag-attributes (cdr arguments) (cons (car arguments) acc) nil))
    ((keywordp (car arguments))
     (tag-attributes (cdr arguments) (cons (car arguments) acc) t)) 
    (t
     (values (nreverse acc) arguments))))

(defmacro deftag (name &rest attributes)
  (let ((attributes (make-attributes attributes)))
    `(prog1 (defclass ,name (dom-element)
	      ())
       (defun ,name (&rest args)
	 (multiple-value-bind (attributes children) (tag-attributes args)
	   (destructuring-bind (&key ,@attributes) attributes
	     (validate-dom-tree
	      (apply #'make-dom-element ,(symbol-name name)		     
		     (remove-if (lambda (attr)
				  (if (null (cdr attr))
				      t))
				(list ,@(mapcar (lambda (attr)
						  `(cons ,(symbol-to-js attr) ,attr))
						attributes)))
		     children)))))
       (export ',name (find-package :tr.gen.core.server.html)))))

(defmacro defempty-tag (name &rest attributes)
  (let ((attributes (make-attributes attributes)))    
    `(prog1 (defclass ,name (empty-dom-element)
	      ())
       (defun ,name (&rest args)
	 (multiple-value-bind (attributes) (tag-attributes args)
	   (destructuring-bind (&key ,@attributes) attributes
	     (validate-dom-tree
	      (apply #'make-dom-element ,(symbol-name name)		     
		     (remove-if (lambda (attr)
				  (if (null (cdr attr))
				      t))
				(list ,@(mapcar (lambda (attr)
						  `(cons ,(symbol-to-js attr) ,attr))
						attributes)))
		     nil)))))
       (export ',name (find-package :tr.gen.core.server.html)))))

;; HTmL4 Tag Definitions
;; http://www.w3.org/TR/xhtml1/dtds.html
(deftag <:a :core :i18n :event accesskey charset coords href hreflang
	name onblur onfocus rel rev shape tabindex target type)
(deftag <:abbr :core :event :i18n)
(deftag <:acronym :core :event :i18n)
(deftag <:address :core :event :i18n)
(defempty-tag <:area :core :event :i18n alt accesskey coords href nohref
	      onblur onfocus shape tabindex)
(deftag <:b :core :event :i18n)
(defempty-tag <:base href)
(deftag <:bdo :i18n id style title)
(deftag <:big :core :event :i18n)
(deftag <:blockquote :core :event :i18n cite)
(deftag <:body :core :i18n :event onload onunload)
(defempty-tag <:br :core)
(deftag <:button :core :event :i18n accesskey disabled name onblur
	onfocus tabindex type value)
(deftag <:caption :core :event :i18n)
(deftag <:cite :core :event :i18n)
(deftag <:code :core :event :i18n)
(defempty-tag <:col :core :event :i18n align char charoff span valign width)
(deftag <:colgroup :core :event :i18n align char charoff span valign width)
(deftag <:dd :core :event :i18n)
(deftag <:del :core :event :i18n cite datetime)
(deftag <:dfn :core :event :i18n)
(deftag <:div :core :event :i18n dojo-type open)
(deftag <:dl :core :event :i18n)
(deftag <:dt :core :event :i18n)
(deftag <:em :core :event :i18n)
(deftag <:fieldset :core :event :i18n)
(deftag <:form :core :event :i18n action accept-charset enctype method
              name onreset onsubmit target)
(defempty-tag <:frame :core frameborder longdesc marginheight marginwidth
	      noresize scrolling src)
(deftag <:frameset :core cols onload olunload rows)
(deftag <:h1 :core :event :i18n)
(deftag <:h2 :core :event :i18n)
(deftag <:h3 :core :event :i18n)
(deftag <:h4 :core :event :i18n)
(deftag <:h5 :core :event :i18n)
(deftag <:h6 :core :event :i18n)
(deftag <:head :i18n profile)
(defempty-tag <:hr :core :event width align)

(deftag <:html dir lang)
(defmethod dom-element! ((stream core-stream) (element <:html) &optional (indentation 0))
;;  (string! stream "<?xml version=\"1.0\" encoding=\"utf-8\"?>")
  (char! stream #\Newline)  
;;;   (string! stream "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
;;;     \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">")
  (string! stream "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
     \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")   
  (char! stream #\Newline)
  (call-next-method stream element indentation))

(deftag <:i :core :event :i18n)
(deftag <:iframe :core frameborder longdesc marginheight marginwidth name scrolling src)
(defempty-tag <:img :core :event :i18n alt src height ismap longdesc usemap width)
(defempty-tag <:input :core :event :i18n accept accesskey alt checked
	      disabled maxlength name onblur onchange onfocus
	      onselect readonly size src tabindex type usemap
	      value width height)
(deftag <:ins :core :event :i18n cite datetime)
(deftag <:kbd :core :event :i18n)
(deftag <:label :core :event :i18n accesskey for onblur onfocus)
(deftag <:legend :core :event :i18n accesskey)
(deftag <:li :core :event :i18n)
(deftag <:link :core :event :i18n charset href hreflang media rel rev type)
(deftag <:map :core :event :i18n name)
(deftag <:meta :i18n content http--equiv name scheme)
(deftag <:noframes :core :event :i18n)
(deftag <:noscript :core :event :i18n)
(deftag <:object :core :event :i18n archive classid codebase codetype
              data declare height name standby tabindex type usemap
              width align)
(deftag <:ol :core :event :i18n)
(deftag <:optgroup :core :event :i18n label disabled)
(deftag <:option :core :event :i18n disabled label selected value)
(deftag <:p :core :event :i18n)
(defempty-tag <:param name id type value valuetype)
(deftag <:pre :core :event :i18n)
(deftag <:q :core :event :i18n cite)
(deftag <:samp :core :event :i18n)
(deftag <:script type charset defer src title language)
(deftag <:select :core :event :i18n disabled multiple name accesskey
              onblur onfocus onchange size tabindex)
(deftag <:small :core :event :i18n)
(deftag <:span :core :event :i18n)
(deftag <:strong :core :event :i18n)
(deftag <:style :i18n type media title)
(deftag <:sub :core :event :i18n)
(deftag <:sup :core :event :i18n)
(deftag <:table :core :event :i18n border cellpadding cellspacing
              frame summary width)
(deftag <:tbody :core :event :i18n align char charoff valign)
(deftag <:td :core :event :i18n abbr align axis char charoff colspan
              headers rowspan scope valign width)
(deftag <:textarea :core :event :i18n cols rows accesskey disables
              name onblur onchange onfocus onselect readonly tabindex)
(deftag <:tfoot :core :event :i18n)
(deftag <:th :core :event :i18n abbr align axis char charoff colspan
              headers rowspan scope valign)
(deftag <:thead :core :event :i18n align char charoff valign)
(deftag <:title :i18n)
(deftag <:tr :core :event :i18n align char charoff valign)
(deftag <:tt :core :event :i18n)
(deftag <:ul :core :event :i18n)
(deftag <:var :core :event :i18n)
(deftag <:embed src width height type quality bgcolor name align
              allow-script-access pluginspage)

(defmethod dom2js ((element t))
  nil)

(defmethod dom2js ((element dom-element))  
  `((lambda ()
      (let ((elem (document.create-element ,(tag element))))
	,@(mapcar (lambda (attr)
		    (if (equal (car attr) "class")			 
			`(setf (slot-value elem 'class-name) ,(cdr attr))
			`(setf (slot-value elem ,(car attr)) ,(cdr attr))))
		  (attributes element))
	,@(mapcar (lambda (child)
		    (if (stringp child)
			`(elem.append-child (document.create-text-node ,child))
			`(elem.append-child ,(dom2js child))))
		  (children element))
	(return elem)))))


(defmacro <:ai (&body body)
  `(list ,@body))

(defmacro <:ah (&body body)
  `(list ,@body))

(defmacro <:js (&body body)
  (with-unique-names (output)
    `(let ((,output (if +context+ (http-response.stream (response +context+)) *core-output*)))
       (prog1 (with-html-output ,output
		(js:js* ,@body))
	 (char! ,output #\Newline)))))

(defmacro with-html-output (stream &body body)
  (with-unique-names (element)
    `(progn
       ,@(mapcar #'(lambda (elem)
		     `(let ((,element ,elem))
			(dom-element! ,stream ,element)))
		 body))))


;; (defclass dojo-element (dom-element)
;;   ())

;; (defmacro defdojo (name &rest attributes)
;;   (let ((attributes (make-attributes attributes))
;; 	(symbol-package (symbol-package name)))
;;     `(prog1 (defclass ,name (dojo-element)
;; 	      ())
;;        (defun ,name (&rest args)
;; 	 (multiple-value-bind (attributes children) (tag-attributes args)
;; 	   (destructuring-bind (&key ,@attributes) attributes
;; 	     (validate-dom-tree
;; 	      (apply #'make-dom-element ,(symbol-name name)		     
;; 		     (remove-if (lambda (attr)
;; 				  (if (null (cdr attr))
;; 				      t))
;; 				(list ,@(mapcar (lambda (attr)
;; 						  `(cons ,(symbol-to-js attr) ,attr))
;; 						attributes)))
;; 		     children)))))
;;        (export ',name (find-package ,(package-name symbol-package))))))

;; (defdojo <dijit::dialog open duration title)

;; (defvar *current-selected* nil)
;; (defun <:css (selectors &allow-other-attributes others &body body)
;;   `(let* ((selectors ,(if (and (listp selectors) (or (stringp (car selectors)) (not (fboundp (car selectors)))))
;;                           (cons 'list (mapcar #'identity selectors))
;;                           selectors))
;;           (*current-selected* (if *current-selected*
;;                                   (reduce #'(lambda (acc item)
;;                                               (nconc acc                                                    
;;                                                      (mapcar #'(lambda (current)
;;                                                                  (string-join (list current item) " "))
;;                                                              *current-selected*)))
;;                                           (ensure-list selectors)
;;                                           :initial-value nil)
;;                                   (ensure-list selectors)))
;;           (properties (reduce #'(lambda (acc item)
;;                                    (if (keywordp item)
;;                                        (concatenate 'string acc " " (string-downcase (string item)) ":")
;;                                        (concatenate 'string acc " " item ";" ~%)))
;;                                (list ,@others)
;;                                :initial-value nil)))
;;      (when properties
;;        (<:ah
;;         (format nil "~A {~%~A}~%~%"
;;                 (string-join *current-selected* ", ") properties)))
;;      ,@body))

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
