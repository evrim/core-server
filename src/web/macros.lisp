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

(defmacro with-field (label input &optional (validation nil))
  "Syntactic sugar to handle form label and values"
  `(<:div :class "field"
	  (<:div :class "value" ,input)
	  (<:div :class "label" ,label)
	  ,(if validation validation)))

(defjsmacro with-field (label input &optional validation)
  (macroexpand-1 `(with-field ,label ,input ,validation)))

;; JS Macros
(defjsmacro $ (id)
  `(document.get-element-by-id ,id))

(defjsmacro doc-body ()
  `(aref (document.get-elements-by-tag-name "body") 0))

(defjsmacro dj_debug (&rest rest)
  `(dojo.debug ,@rest))

;; Dojo 0.4
(defjsmacro debug (&rest rest)
  `(dojo.debug ,@rest))

;; Dojo 1.0
(defjsmacro debug (&rest rest)
  `(console.debug ,@rest))

(defjsmacro $$ (id)
  `(dojo.widget.by-id ,id))

(defjsmacro $fck (id)
  `(*f-c-keditor-a-p-i.*get-instance ,id))

(defjsmacro s-v (a b)
  `(slot-value ,a ,b))

(defjsmacro $idoc (iframe-id)
  `(s-v ($ ,iframe-id) 'content-document))

(defjsmacro aif (a b c)
  `((lambda (it)     
      (if it
	  ,b
	  ,c)) ,a))

(defjsmacro typep (object type)
  `(= ,(if (eq 'quote (car type))
	   (symbol-to-js (cadr type))
	   type)
      (typeof ,object)))

(defjsmacro nreverse (lst)
  `(reverse ,lst))

;; (defjsmacro mapobj (lambda object)
;;   `())

;; (defjsmacro null (arg)
;;   `(= 'null ,arg))

;; (defjsmacro undefined (arg)
;;   `(= "undefined" (typeof ,arg)))

(defjsmacro input-value (id)
  `(slot-value ($ ,id) 'value))

;; (defjsmacro wrap-on-load (&body body)
;;   (if (context.ajax-request *context*)
;;       `(progn
;; 	 ,@body)
;;       `(dojo.add-on-load
;; 	(lambda ()
;; 	  ,@body))))

;; (defmacro with-yaclml-output-to-string (&body body)
;;   `(let ((*yaclml-stream* (make-core-stream ""))
;; 	 (*yaclml-indent* nil))
;;      ,@body
;;      (return-stream *yaclml-stream*)))
