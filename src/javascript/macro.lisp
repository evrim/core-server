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

;;+-----------------------------------------------------------------------------
;;| Javascript Macros
;;+-----------------------------------------------------------------------------
;;
;; This file contains javascript macros functions.
;;

(defjsmacro list (&rest args)
  `(new (*array ,@args)))

(defjsmacro make-array (&rest args)
  `(list ,@args))

(defjsmacro create (&rest args)
  `(jobject ,@args))

(defjsmacro 1+ (arg)
  `(+ ,arg 1))

(defjsmacro 1- (arg)
  `(- ,arg 1))

(defjsmacro when (a &body b)
  `(if ,a (progn ,@b)))

(defjsmacro unless (a &body b)
  `(if (not ,a) (progn ,@b)))

(defjsmacro setf (&rest rest)
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

(defjsmacro case (object &rest cases)
  (flet ((one (case)
	   (if (eq 't (car case))
	       `(t ,@(cdr case))
	       `((eq ,object ,(car case))
		 ,@(cdr case)))))
    `(cond
       ,@(mapcar #'one cases))))

(defjsmacro with-slots (slots object &body body)
  `(let ,(mapcar (lambda (slot)
		   (list slot `(slot-value ,object ',slot)))
		 slots)
     ,@body))

(defjsmacro s-v (b)
  `(slot-value self ,b))

(defmacro/js destructuring-bind (list val &body body)
  (with-unique-names (g)
    `(let ((,g ,val))
       (let ,(mapcar (lambda (a seq)
		       `(,a (nth ,seq ,g)))
		     list (seq (length list)))
	 ,@body))))

(defjsmacro aif (a b &optional (c 'nil))
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

(defjsmacro null (atom)
  `(or (typep ,atom 'undefined)
       (eq 'null ,atom)))

(defjsmacro defined (atom)
  `(not (typep ,atom 'undefined)))

(defjsmacro $ (id)
  `(document.get-element-by-id ,id))

(defjsmacro $idoc (iframe-id)
  `(slot-value ($ ,iframe-id) 'content-document))

(defjsmacro doc-body ()
  `(aref (document.get-elements-by-tag-name "body") 0))

(defjsmacro input-value (id)
  `(slot-value ($ ,id) 'value))

(defjsmacro $fck (id)
  `(*f-c-keditor-a-p-i.*get-instance ,id))

(defmacro/js make-component (ctor &rest args)
  `(call/cc ,ctor (jobject ,@args)))

(defmacro/js lift1 (fun)
  `(lambda (arg) (call/cc ,fun arg)))

(defmacro/js lift0 (fun)
  `(lambda () (call/cc ,fun)))

;; -------------------------------------------------------------------------
;; Lift Javascript Event Handler with a call/cc function.
;; -------------------------------------------------------------------------
(defmacro/js lifte (fun)
  `(lambda (e) (call/cc ,fun)))