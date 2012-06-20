(in-package :core-server)

;;+-----------------------------------------------------------------------------
;;| Javascript Macros
;;+-----------------------------------------------------------------------------
;;This file contains javascript macros functions.

(defmacro/js list (&rest args)
  `(new (*array ,@args)))

(defmacro/js make-array (&rest args)
  `(list ,@args))

(defmacro/js create (&rest args)
  `(jobject ,@args))

(defmacro/js 1+ (arg)
  `(+ ,arg 1))

(defmacro/js 1- (arg)
  `(- ,arg 1))

(defmacro/js when (a &body b)
  `(if ,a (progn ,@b)))

(defmacro/js awhen (a &body b)
  `(let ((it ,a))
     (when it ,@b)))

(defmacro/js unless (a &body b)
  `(if (not ,a) (progn ,@b)))

(defmacro/js setf (&rest rest)
  (assert (evenp (length rest)))
  `(progn
     ,@(let (state)
	    (nreverse
	     (reduce (lambda (acc atom)
		       (if (null state)
			   (prog1 acc (setf state atom))
			   (prog1 (cons `(setq ,state ,atom) acc)
			     (setq state nil))))
		     rest :initial-value nil)))))

(defmacro/js case (object &rest cases)
  (flet ((one (case)
	   (if (eq 't (car case))
	       `(t ,@(cdr case))
	       `((eq ,object ,(car case))
		 ,@(cdr case)))))
    `(cond
       ,@(mapcar #'one cases))))

(defmacro/js with-slots (slots object &body body)
  `(let ,(mapcar (lambda (slot)
		   (list slot `(slot-value ,object ',slot)))
		 slots)
     ,@body))

(defmacro/js s-v (b)
  `(slot-value self ,b))

(defmacro/js destructuring-bind (list val &body body)
  (with-unique-names (g)
    `(let ((,g ,val))
       (let ,(mapcar (lambda (a seq)
		       `(,a (nth ,seq ,g)))
		     list (seq (length list)))
	 ,@body))))

(defmacro/js aif (a b &optional c)
  `((lambda (it)     
      (if it
	  ,b
	  ,c)) ,a))

(defmacro/js typep (object type)
  `(= ,(if (eq 'quote (car type))
	   (symbol-to-js (cadr type))
	   type)
      (typeof ,object)))

(defmacro/js null (atom)
  `(or (typep ,atom 'undefined) (eq 'null ,atom)))

(defmacro/js defined (atom)
  `(not (typep ,atom 'undefined)))

(defmacro/js by-id (id)
  `(document.get-element-by-id ,id))

(defmacro/js make-component (ctor &rest args)
  `(call/cc ,ctor (jobject ,@args)))

(defmacro/js delete-slots (self &rest slots)
  `(remove-slots ,self (array ,@(mapcar (lambda (a) (symbol-to-js (cadr a)))
					;; cadr is for (quote somesymbol)
					;; -evrim.
					slots))))

(defmacro/js delete-slot (self slot)
  `(delete-slots ,self ,slot))

;; -------------------------------------------------------------------------
;; Lift Macros for 0 arg & 1 arg functions 
;; -------------------------------------------------------------------------
(defmacro/js lift1 (fun)
  `(lambda (arg) (call/cc ,fun arg)))

(defmacro/js lift0 (fun)
  `(lambda () (call/cc ,fun)))

;; -------------------------------------------------------------------------
;; Lift Javascript Event Handler with a call/cc function.
;; -------------------------------------------------------------------------
(defmacro/js lifte (fun &rest args)
  (if (atom fun)
      `(event (e)
	 (try (with-call/cc (call/cc ,fun ,@args)) (:catch (e) (_debug e)))
	 false)
      `(event (e)
	 (try (with-call/cc ,fun) (:catch (e) (_debug e)))
	 false)))

(defmacro/js lifte2 (fun &rest args)
  (if (atom fun)
      `(event (e)
	 (try (with-call/cc (call/cc ,fun ,@args)) (:catch (e) (_debug e)))
	 true)
      `(event (e)
	 (try (with-call/cc ,fun) (:catch (e) (_debug e)))
	 true)))

;; -------------------------------------------------------------------------
;; Method Macro
;; -------------------------------------------------------------------------
(defmacro/js method (lambda-list &rest body)
  `(lambda ,lambda-list     
     (javascript-cps-method-body ,@body)))

;; -------------------------------------------------------------------------
;; Localization Macro
;; -------------------------------------------------------------------------
(defmacro/js _ (str &rest args)
  `(gettext ,str (array ,@args)))

;; -------------------------------------------------------------------------
;; Cosmetix Macro: With-field
;; -------------------------------------------------------------------------
(defmacro with-field (label input &optional (validation nil))
  "Syntactic sugar to handle form label and values"
  `(<:div :class "field"
	  (<:div :class "label" ,label)
	  (<:div :class "value" ,input)
	  ,(if validation validation)))

(defjsmacro with-field (label input &optional validation)
  (macroexpand-1 `(with-field ,label ,input ,validation)))

;; -------------------------------------------------------------------------
;; Bookmarklet Macro
;; -------------------------------------------------------------------------
(defmacro/js bookmarklet-script (url)
  `(+ "javascript:void((function(){"
      (decode-u-r-i-component "var%20a=document.createElement(\"SCRIPT\");")
      "a.type=\"text/javascript\";"
      "a.src=\"" ,url "\";"      
      "document.getElementsByTagName(\"HEAD\")[0].appendChild(a);"
      "})())"))

;; Core Server: Web Application Server

;; Copyright (C) 2006-2012  Metin Evrim Ulu

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


;; (defmacro/js $ (id)
;;   `(document.get-element-by-id ,id))

;; (defmacro/js $idoc (iframe-id)
;;   `(slot-value ($ ,iframe-id) 'content-document))

;; (defmacro/js doc-body ()
;;   `(aref (document.get-elements-by-tag-name "body") 0))

;; (defmacro/js input-value (id)
;;   `(slot-value ($ ,id) 'value))

;; (defmacro/js $fck (id)
;;   `(*f-c-keditor-a-p-i.*get-instance ,id))
