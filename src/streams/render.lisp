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
;;;-----------------------------------------------------------------------------
;;; Render
;;;-----------------------------------------------------------------------------
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defgeneric expand-render (form expander stream &optional continue checkpoint)
    (:documentation "Special dsl for rendering."))

  (defmethod expand-render ((form operator) expander (stream symbol) &optional continue checkpoint)
    (expand-grammar form #'expand-render stream continue checkpoint))

  (defmethod expand-render ((form t) expander (stream symbol) &optional continue checkpoint)
    (declare (ignorable continue checkpoint))
    `(write-stream ,stream ,form))

  (defmethod expand-render ((form bind-operator) expander (stream symbol) &optional continue checkpoint)
    (declare (ignorable continue checkpoint))
    (if (not (fboundp (func form)))
      (setf (func form) (intern (symbol-name (func form)) :core-server)))
    
    `(,(func form) ,stream ,@(arguments form)))

  (defmethod expand-render ((form sep-operator) expander (stream symbol) &optional continue checkpoint)
    (if (symbolp (children form))
	`(prog1 ,continue
	   ,(funcall expander (walk-grammar `(car ,(children form)))
		     expander stream continue checkpoint)
	   (mapcar (lambda (atom)		 
		     ,(funcall expander (walk-grammar `(:and ,(seperator form) atom))
			       expander stream continue checkpoint))
		   (cdr ,(children form))))
	(error ":sep does not support quoted lists, please use macro.")))
  
  (defmethod expand-render ((form indent-operator) expander (stream symbol) &optional continue checkpoint)
    (declare (ignorable checkpoint expander))
    `(prog1 ,continue (increase-indent ,stream ,(how-many form))))

  (defmethod expand-render ((form deindent-operator) expander (stream symbol) &optional continue checkpoint)
    (declare (ignorable checkpoint expander))
    `(prog1 ,continue (decrease-indent ,stream ,(how-many form)))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun optimize-render-1 (form)
    "Optimizes forms and elimiates unnessary class dispatch.

SERVER> (describe (optimize-render-1 (walk-grammar `(:and \"abc\" \"def\" (:and #\Newline \"zee\")))))
#<AND-OPERATOR {1003713BE1}>
is an instance of class #<STANDARD-CLASS AND-OPERATOR>.
The following slots have :INSTANCE allocation:
 ARGS    (\"abcdef
zee\")"
    (let ((ands (operator-search-type form 'and-operator)))
      (labels ((concat-args (and)
		 (setf (args and)
		       (nreverse
			(reduce (lambda (acc arg)
				  (cond
				    ((and (or (stringp (car acc)) (characterp (car acc)))
					  (or (stringp arg) (characterp arg)))
				     (cons (format nil "~A~A" (car acc) arg) (cdr acc)))
				    (t (cons arg acc))))
				(cdr (args and)) :initial-value (list (car (args and))))))
		 and)
	       (flatten-args (and)
		 (setf (args and)
		       (nreverse
			(reduce (lambda (acc arg)
				  (cond
				    ((typep arg 'and-operator)
				     (append (nreverse (args arg)) acc))
				    ((null arg)
				     acc)
				    (t
				     (cons arg acc))))
				(args and) :initial-value nil)))
		 and))
	(mapc #'concat-args (mapcar #'flatten-args ands))
	form))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro defrender (name args &rest body)
    "Rule definition macro that provides a common lexical environment for rules."
    (with-unique-names (stream)      
      `(defun ,name ,(cons stream args)
	 (block rule-block
	   (let ((cp (current-checkpoint ,stream)))
	     (declare (ignorable cp))
	     ,(expand-render
	       (optimize-render-1
		(walk-grammar `(:checkpoint ,@body (:commit))))
	       #'expand-render stream)
	     ,stream))))))

(defun byte! (stream byte)
  (write-stream stream byte))

(defun char! (stream char)
  (let ((code (char-code char)))
    (if (> code 127) 
	(reduce #'(lambda (acc atom)
		    (declare (ignore acc))
		    (byte! stream atom)
		    nil)
		(string-to-octets (string char) :utf-8) :initial-value nil)
	(byte! stream code))))

(defun string! (stream string)
  (write-stream stream (string-to-octets string :utf-8)))

;; (defun version! (stream version)
;;   (fixnum! stream (car version))
;;   (reduce #'(lambda (acc atom)
;; 	      (declare (ignore acc))
;; 	      (char! stream #\.)
;; 	      (fixnum! stream atom)
;; 	      nil)
;; 	  (cdr version) :initial-value nil))

(defrender version! (version)
  (:fixnum! (car version))
  (:map (cdr version)
	(:char! #\.)
	(:fixnum! it)))

(defun symbol! (stream symbol)
  (string! stream (string-downcase (symbol-name symbol))))

(defun symbol-with-package! (stream symbol)
  (string! stream (package-name (symbol-package symbol)))
  (string! stream "::")
  (string! stream (symbol-name symbol)))

(defun fixnum! (stream number)
  (string! stream (format nil "~D" number)))

(defun quoted! (stream string)
  (char! stream #\")
  (reduce #'(lambda (acc atom)
	      (declare (ignore acc))
	      (cond
		((or (char= #\" atom) (char= #\\ atom))
		 (char! stream #\\) (char! stream atom))
		((or (char= #\Newline atom) (char= #\Linefeed atom))
		 )
		(t (char! stream atom)))
	      nil)
	  string
	  :initial-value nil)
  (char! stream #\"))

(defun quoted-fixnum! (stream number)
  (quoted! stream (format nil "~D" number)))

(defvar +hex-alphabet+
  #.(reduce #'(lambda (acc atom) (push-atom (char-code atom) acc) acc)
	    "0123456789ABCDEF" :initial-value (make-accumulator :byte)))

(defun hex-value! (stream hex)
  (byte! stream (aref +hex-alphabet+ (floor (/ hex 16))))
  (byte! stream (aref +hex-alphabet+ (rem hex 16))))

(defrender hex-value (hex)
  (:byte! (aref +hex-alphabet+ (floor (/ hex 16))))
  (:byte! (aref +hex-alphabet+ (rem hex 16))))

;;; sugars
(defmacro with-separator ((var lst sep stream) &body body) 
  `(let ((,var (car ,lst)))
     ,@body
     (reduce #'(lambda (acc ,var)
		 (declare (ignore acc))
		 (char! ,stream ,sep)
		 (char! ,stream #\ )
		 ,@body)
	     (cdr ,lst) :initial-value nil)))
